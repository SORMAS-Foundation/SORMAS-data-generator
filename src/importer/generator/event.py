import random

from sormas import EventDto, EventStatus, EventReferenceDto,Disease

from generator.user import surv_sup_user_ref
from generator.utils import dnow, duuid

# todo Begin

frac_events_cases = 0.05  # number of events divided by number of cases
mu_event_size = 10  # mean parameter of the Gaussian used in draw the event size
sig_event_size = 10  # standard deviation of the Gaussian
min_event_size = 5  # minimum size of an event
p_known_participant = 0.01  # 0.2 # proportion of participants who are a case

# or a contact, knowing that there is at least one case participating

# todo End

random.seed(42)


def gen_event_dto(event_desc=None, start_date=None, location=None,disease= None):
    if event_desc is None:
        raise NotImplementedError

    if start_date is None:
        raise NotImplementedError

    if location is None:
        raise NotImplementedError

    if disease is None:
        raise NotImplementedError

    event_dto = EventDto(
        uuid=duuid(),  # todo EventFacade:163 not null fires
        event_status=EventStatus.EVENT,
        event_desc=event_desc,
        report_date_time=dnow(),
        reporting_user=surv_sup_user_ref(),
        start_date=start_date,
        event_location=location,
        disease=Disease.CORONAVIRUS
        # todo region + district is required in the UI!
    )
    return event_dto


def event_ref(event_uuid):
    event_ref_dto = EventReferenceDto(uuid=event_uuid)
    return event_ref_dto
