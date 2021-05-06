import logging
import os
import random

import numpy as np
import pandas as pd
import sormas as sormas_api
from sormas import Disease, SymptomState, CaseOutcome, PresentCondition

from generator.cases import gen_case_dto
from generator.district import insert_district
from generator.event import gen_event_dto
from generator.location import gen_location_dto
from generator.person import gen_person_dto
from generator.region import insert_region, region_ref
from generator.symptoms import gen_symptom_dto
from generator.utils import sormas_db_connect
from universe.case import Case
from universe.contact import Contact
from universe.event import Event
from universe.event_participant import EventParticipant
from universe.tick import Tick
from universe.util import export

random.seed(42)


class World:
    def __init__(self, disease=Disease.CORONAVIRUS):
        self.today = Tick()
        self.model = self._load_model()

        sormas_domain = os.environ.get("DOMAIN_NAME")
        host = sormas_domain if sormas_domain else 'localhost'
        configuration = sormas_api.Configuration(
            host=f'http://{host}:6080/sormas-rest',
            username=os.getenv('SORMAS_REST_USERNAME', 'SurvOff'),
            password=os.getenv('SORMAS_REST_PASSWORD', 'SurvOff'),
        )

        configuration.verify_ssl = False
        configuration.debug = True
        self.sormas_api_config = configuration

        self.current_disease = disease
        self.regions = {}
        self.current_region = {}
        self.districts = {}
        self.current_district = {}

        with sormas_db_connect() as conn:
            with conn.cursor() as cur:
                cur.execute("SELECT name, id, uuid FROM region ORDER BY name")
                regions = cur.fetchall()
                for res in regions:
                    logging.info(f'Region: {res}')
                    self.regions[res[0]] = {}
                    self.regions[res[0]]['id'] = res[1]
                    self.regions[res[0]]['uuid'] = res[2]

                cur.execute("""
                    SELECT d.name, d.id, d.uuid , r.uuid
                      FROM district d
                      LEFT OUTER JOIN region r
                        ON d.region_id = r.id
                     ORDER BY r.name, d.name
                """)
                districts = cur.fetchall()
                for res in districts:
                    logging.info(f'District: {res}')
                    self.regions[res[0]] = {}
                    self.regions[res[0]]['id'] = res[1]
                    self.regions[res[0]]['uuid'] = res[2]
                    self.regions[res[0]]['region'] = res[3]

                cur.execute(f"UPDATE diseaseconfiguration SET active = true WHERE disease = '{self.current_disease}'")
                # # disable other diseases
                # cur.execute(f"UPDATE diseaseconfiguration SET active = false WHERE disease != '{self.current_disease}'")

        # default region and district
        if self.regions:
            first_reg = list(self.regions.keys())[0]
            self.current_region = self.regions[first_reg]
        if self.districts:
            first_dis = list(self.districts.keys())[0]
            self.current_district = self.districts[first_dis]

    @staticmethod
    def _load_model():
        path = "../generator/data/out/"

        persons = pd.read_csv(path + "persons_df.csv")
        persons = persons.rename(columns={'id': 'id_person'})

        cases = persons.query('is_case == True')

        symptoms_cases = pd.read_csv(path + "symptoms_cases_df.csv")
        symptoms_cases = symptoms_cases.groupby('id_person').symptom.agg(','.join).reset_index()

        cases = pd.merge(cases, symptoms_cases, on='id_person')

        contacts = pd.read_csv(path + "contacts_df.csv")
        contacts = contacts.rename(columns={'id_contact': 'id_person'})
        contacts = pd.merge(contacts, persons, on='id_person')

        event_participants = pd.read_csv(path + "event_participants_df.csv")
        event_participants = event_participants.rename(columns={'id_participant': 'id_person'})
        event_participants = pd.merge(event_participants, persons, on='id_person')
        events = pd.read_csv(path + "events_df.csv")

        return {
            'cases': cases,
            'contacts': contacts,
            'events': events,
            'event_participants': event_participants
        }

    def _create_person(self, pers):
        from sormas import Sex
        date = pers.reporting_date
        birthdate_yyyy = None if type(date) is float else int(date.split('-')[0]) - pers.age

        lat = 0 if np.isnan(pers.latitude) else pers.latitude  # Fixme 0 condition
        lon = 0 if np.isnan(pers.longitude) else pers.longitude  # Fixme 0 condition
        location_dto = gen_location_dto(
            lat, lon,
            self.current_district.get('region', self.current_region.get('uuid')),
            self.current_district.get('uuid')
        )
        person = gen_person_dto(
            first_name=pers.first_name,
            last_name=pers.family_name,
            # todo other values like diverse
            sex=Sex.MALE if pers.sex == 'm' else Sex.FEMALE,
            birthdate_yyyy=birthdate_yyyy,
            address=location_dto,
            present_condition=PresentCondition.DEAD if pers.died else PresentCondition.ALIVE
        )
        # todo use Person class?
        return person

    def add_region(self, region):
        region_id, region_uuid = insert_region(region)
        self.regions[region] = {}
        self.regions[region]['id'] = region_id
        self.regions[region]['uuid'] = region_uuid
        self.current_region = self.regions[region]
        self.current_district = {}

    def add_district(self, district, region):
        self.add_region(region)

        region_id = self.current_region['id']
        district_id, district_uuid = insert_district(district, region_id)
        self.districts[district] = {}
        self.districts[district]['id'] = district_id
        self.districts[district]['uuid'] = district_uuid
        self.districts[district]['region'] = self.current_region['uuid']
        self.current_district = self.districts[district]

    # todo Create individual cases that reproduce the
    #  aggregated data from covid dashboard and SurvStat
    def pre_populate_cases_and_contacts(self, n=5):
        def map_symptom(s):
            _map = {
                'Fieber': 'fever',
                'Husten': 'cough',
                'Geruchssinnsverlust': 'loss_of_smell'
            }
            res = {}
            for symptom in s.split(','):
                tmp = _map.get(symptom)
                if tmp is None:
                    logging.info(symptom + ' is not mapped')
                    continue
                res[tmp] = SymptomState.YES
            return res

        model_cases = self.model['cases']
        model_contacts = self.model['contacts']

        for i in range(n):
            m_case = model_cases.iloc[i]
            person_dto = self._create_person(m_case)
            symptoms_dto = gen_symptom_dto(self.current_disease, map_symptom(m_case.symptom))

            case_outcome = CaseOutcome.DECEASED if m_case.died else CaseOutcome.NO_OUTCOME
            outcome_date = m_case.reporting_date  # FIXME

            case_dto = gen_case_dto(
                date=m_case.reporting_date,
                p_uuid=person_dto.uuid,
                disease=self.current_disease,
                symptoms=symptoms_dto,
                case_outcome=case_outcome,
                outcome_date=outcome_date
            )
            case = Case(person_dto, case_dto)
            self.today.cases.append(case)

            # create contacts for this case
            m_contacts = model_contacts.query(f'id_index == {m_case.id_person}')
            for m_contacts in m_contacts.iterrows():
                person_dto = self._create_person(m_contacts[1])
                contact = Contact(person_dto, case.inner.uuid, case.disease())
                self.today.contacts.append(contact)

    def pre_populate_events_and_participants(self, n=2):
        model_events = self.model['events']
        model_participants = self.model['event_participants']
        for i in range(n):
            m_event = model_events.iloc[i]
            import datetime
            # todo missing fields address longitude latitude
            start_date = datetime.date.today()  # m_event.date
            event_desc = 'hope1'  # random.choice(['Party', 'BBQ', 'Family meeting'])
            location_dto = gen_location_dto(
                m_event.latitude,
                m_event.longitude,
                self.current_district.get('region', self.current_region.get('uuid')),
                self.current_district.get('uuid')
            )
            # FIXME use the real place
            event_dto = gen_event_dto(event_desc, start_date, location_dto, self.current_disease)

            # add participants
            m_participants = model_participants.query(f'id_event == {m_event.id}')
            participants = []
            for m_particpant in m_participants.iterrows():
                person = self._create_person(m_particpant[1])
                particpant = EventParticipant(person, event_dto.uuid)
                participants.append(particpant)
            self.today.events.append(Event(event_dto, participants))

    def pre_populate_infection_chains(self):
        raise NotImplementedError

    def export_sormas(self):
        export.sormas_export(self)

    def export_json(self):
        export.json_export(self)
