from generator.event_participant import gen_event_participant_dto


class EventParticipant:
    def __init__(self, person, event_uuid):
        self.person = person
        self.inner = gen_event_participant_dto(person.uuid, event_uuid)

    def __str__(self):
        return f"({self.person})"

    def __repr__(self):
        return self.__str__()

    def to_dict(self):
        return {
            "person": self.person.to_dict(),
            "inner": self.inner.to_dict()
        }
