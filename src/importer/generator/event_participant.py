from sormas import EventParticipantDto

from generator.event import event_ref
from generator.person import person_ref
from generator.utils import duuid


def gen_event_participant_dto(person_uuid, event_uuid):
    event_participant_dto = EventParticipantDto(
        uuid=duuid(), # todo triggers EventParticipantFacadeEjb:174
        person=person_ref(person_uuid),
        event=event_ref(event_uuid)
        # todo creation date is not required
    )
    return event_participant_dto
