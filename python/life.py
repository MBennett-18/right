import random
import sys

def r_cont(cdf):
    x = random.random()
    return [ n for n,i in enumerate(cdf) if i>=x ][0]

class AbstractPerson(object):
    def __init__(self):
        self.gender = '?'
        self.age    = -1
        self.events = self.possible_events()
    def randomize(self):
        self.gender = self.assign_gender()
        self.age    = self.assign_age()
        self.death  = self.assign_death()
        for e in self.events:
            e.reset(self)
    def grow_older(self):
        if self.age >= self.death: return False 
        for e in self.events:
            if e.occurred(self):
                if e.death(): return False
        self.age += 1 
        return True
    def assign_gender(self):
        raise NotImplementedError("AbstractPerson::random_gender")
    def assign_age(self):
        raise NotImplementedError("AbstractPerson::random_age")
    def assign_death(self):
        raise NotImplementedError("AbstractPerson::random_death")
    def possible_events(self, events):
        raise NotImplementedError("AbstractPerson::possible_events")

class AbstractEvent(object):
    def __init__(self):
        self.occurs_at = sys.maxint
    def occurred(self, person):
        raise NotImplementedError("AbstractEvent::occurred")
    def death(self): 
        raise NotImplementedError("AbstractEvent::death")
    def reset(self,person): 
        raise NotImplementedError("AbstractEvent::reset")

class Simulation(object):
    def __init__(self, person, steps):
        self.person       = person
        self.steps        = steps
        self.deaths       = 0
        self.age_at_death = []
    def run(self,replications):
        self.deaths = 0 
        for _ in range(replications):
            self.person.randomize()
            for _ in range(self.steps): 
                if not self.person.grow_older():
                    self.deaths += 1 
                    self.age_at_death.append(self.person.age)
                    break
