class Event:
    def __init__(self, event, participants):
        self.inner = event
        self.participants = participants

    def __str__(self):
        return f"({self.inner}: {self.participants})"

    def __repr__(self):
        return self.__str__()

    def to_dict(self):
        return {
            'inner': self.inner.to_dict(),
            'participants': list(map(lambda participant: participant.to_dict(), self.participants))
        }
