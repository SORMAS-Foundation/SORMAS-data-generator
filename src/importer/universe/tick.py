class Tick:
    def __init__(self, cases=None, contacts=None, events=None):

        if cases is None:
            cases = []
        if contacts is None:
            contacts = []

        if events is None:
            events = []

        self.cases = cases
        self.contacts = contacts

        self.events = events

    def __str__(self):
        return f'Cases: {len(self.cases)}, Contacts: {len(self.contacts)}, Events: {len(self.events)}'

    def __repr__(self):
        return self.__str__()

    def to_dict(self):
        return {
            'cases': list(map(lambda cases: cases.to_dict(), self.cases)),
            'contacts': list(map(lambda contacts: contacts.to_dict(), self.contacts)),
            'events': list(map(lambda events: events.to_dict(), self.events))
        }
