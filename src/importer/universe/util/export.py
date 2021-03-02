import datetime
import json as _json
import logging
import os
from json import JSONEncoder

import sormas as sormas_api
from sormas.rest import ApiException

from config import PERFORMANCE_LOG_DIR, std_formatter
from universe.event import Event
from universe.event_participant import EventParticipant

logger = logging.getLogger(__name__)


def json_export(world):
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


def sormas_export(world):
    """

    :type world: World
    """

    with sormas_api.ApiClient(world.sormas_api_config) as api_client:
        day = world.today

        case_handler = logging.FileHandler(os.path.join(PERFORMANCE_LOG_DIR, 'cases.log'))
        case_handler.setFormatter(std_formatter)
        logger.addHandler(case_handler)
        for case in day.cases:
            person_dto = case.person
            case_data_dto = case.inner
            try:
                logger.info(f'importing person: {person_dto.uuid}')
                sormas_api.PersonControllerApi(api_client).post_persons(person_dto=[person_dto])
                logger.info(f'importing case: {case_data_dto.uuid}')
                sormas_api.CaseControllerApi(api_client).post_cases(case_data_dto=[case_data_dto])
            except ApiException as e:
                logging.exception("Exception: %s\n" % e)

        logger.removeHandler(case_handler)

        contact_handlers = logging.FileHandler(os.path.join(PERFORMANCE_LOG_DIR, 'contacts.log'))
        contact_handlers.setFormatter(std_formatter)
        logger.addHandler(contact_handlers)

        for contact in day.contacts:
            person_dto = contact.person
            contact_dto = contact.inner
            try:
                logger.info(f'importing person: {person_dto.uuid}')
                sormas_api.PersonControllerApi(api_client).post_persons(person_dto=[person_dto])
                logger.info(f'importing contact: {contact_dto.uuid}')
                sormas_api.ContactControllerApi(api_client).post_contacts(contact_dto=[contact_dto])
            except ApiException as e:
                logging.exception("Exception: %s\n" % e)

        logger.removeHandler(contact_handlers)

        event_handlers = logging.FileHandler(os.path.join(PERFORMANCE_LOG_DIR, 'events.log'))
        event_handlers.setFormatter(std_formatter)
        logger.addHandler(event_handlers)

        event: Event
        for event in day.events:
            event_dto = event.inner
            try:
                logger.info(f'importing event: {event_dto.uuid}')
                sormas_api.EventControllerApi(api_client).post_events(event_dto=[event_dto])
            except ApiException as e:
                logging.exception("Exception: %s\n" % e)

        logger.removeHandler(event_handlers)

        event_participants_handlers = logging.FileHandler(os.path.join(PERFORMANCE_LOG_DIR, 'event_participants.log'))
        event_participants_handlers.setFormatter(std_formatter)
        logger.addHandler(event_participants_handlers)

        for event in day.events:
            try:
                participant: EventParticipant
                for participant in event.participants:
                    person_dto = participant.person
                    participant_dto = participant.inner
                    logger.info(f'importing person: {person_dto.uuid}')
                    sormas_api.PersonControllerApi(api_client).post_persons(person_dto=[person_dto])
                    logger.info(f'importing event_participant: {participant_dto.uuid}')
                    sormas_api.EventParticipantControllerApi(api_client).post_event_participants(
                        event_participant_dto=[participant_dto]
                    )

            except ApiException as e:
                logging.exception("Exception: %s\n" % e)

        logger.removeHandler(event_participants_handlers)
