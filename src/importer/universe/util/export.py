import datetime
import json as _json
import logging
from json import JSONEncoder

import sormas as sormas_api
from sormas.rest import ApiException

from universe.event import Event
from universe.event_participant import EventParticipant


def json(world):
    """
    Dump cases of our world to JSON
    :type world: World
    """

    class DateTimeEncoder(JSONEncoder):
        def default(self, obj):
            if isinstance(obj, (datetime.date, datetime.datetime)):
                return obj.isoformat()

    with open('cases.json', 'w') as f:
        out = list(map(lambda tick: tick.to_dict(), world.history))
        s = _json.dumps(out, indent=2, cls=DateTimeEncoder)
        f.write(s)


def sormas(world):
    """

    :type world: World
    """

    with sormas_api.ApiClient(world.sormas_api_config) as api_client:
        logging.info(f'Export to SORMAS')
        day = world.today
        for case in day.cases:
            person_dto = case.person
            case_data_dto = case.inner
            try:
                logging.info(f'importing person: {person_dto.uuid}')
                sormas_api.PersonControllerApi(api_client).post_persons(person_dto=[person_dto])
                logging.info(f'importing case: {person_dto.uuid}')
                sormas_api.CaseControllerApi(api_client).post_cases(case_data_dto=[case_data_dto])
            except ApiException as e:
                logging.exception("Exception: %s\n" % e)

        for contact in day.contacts:
            person_dto = contact.person
            contact_dto = contact.inner
            try:
                logging.info(f'importing person: {person_dto.uuid}')
                sormas_api.PersonControllerApi(api_client).post_persons(person_dto=[person_dto])
                logging.info(f'importing contact: {person_dto.uuid}')
                sormas_api.ContactControllerApi(api_client).post_contacts(contact_dto=[contact_dto])
            except ApiException as e:
                logging.exception("Exception: %s\n" % e)

        event: Event
        for event in day.events:
            try:
                logging.info(f'importing event: {person_dto.uuid}')
                sormas_api.EventControllerApi(api_client).post_events(event_dto=[event.inner])
            except ApiException as e:
                logging.exception("Exception: %s\n" % e)

        for event in day.events:
            try:
                participant: EventParticipant
                for participant in event.participants:
                    logging.info(f'importing person: {person_dto.uuid}')
                    sormas_api.PersonControllerApi(api_client).post_persons(person_dto=[participant.person])
                    logging.info(f'importing event participant: {person_dto.uuid}')
                    sormas_api.EventParticipantControllerApi(api_client).post_event_participants(
                        event_participant_dto=[participant.inner]
                    )

            except ApiException as e:
                logging.exception("Exception: %s\n" % e)

    logging.info('DONE!')
