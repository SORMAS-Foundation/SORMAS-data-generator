from generator.cases import gen_case_dto


# todo make Case extend the Dto?
class Case:
    def __init__(self,  person, case):
        self.person = person
        self.inner = case

    def __str__(self):
        return f"({self.inner.disease}: {self.person})"

    def __repr__(self):
        return self.__str__()

    def disease(self):
        # todo can python do delegation?
        return self.inner.disease

    def to_dict(self):
        return {
            "person": self.person.to_dict(),
            "inner": self.inner.to_dict()
        }
