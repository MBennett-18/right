import life
import random
import matplotlib.pyplot as plt
import scipy.stats as ss
import sys
import numpy as np

class Stroke(life.AbstractEvent):
    ANNUAL_RISK_STROKE = [0.0, 0.013, 0.022, 0.032, 0.04, 0.067, 0.098, 0.096, 0.067, 0.152]
    def __init__(self):
        self.acc_strokes = 0
        self.acc_deaths = 0
        self.alive = True
    def set_next_event(self, person):
        # Assumption that simulation is only 5 years (range(5+1))
        cdf = ss.nbinom.cdf(range(6),1,self.risk(person))
        cdf = np.append(cdf, 1.0) # Cap at 6 years
        self.occurs_at = person.age - 1 + life.r_cont(cdf)
    def reset(self, person):
        self.alive = True
        self.strokes = life.r_cont([0, 0.65, 0.9, 1]) - 1
    def occurred(self, person):
        risk = self.risk(person)
        if random.random() <= risk: 
            self.strokes += 1 # Had an event
            self.acc_strokes  += 1 # Accumulator
            if random.random() <= 0.19:
                self.acc_deaths += 1 # Accumulator
                self.alive = False  
            return True
        else:
            return False
    def death(self): return self.alive
    def chads(self, person):
        score = 0
        if person.age >= 75:                     score += 2
        if person.age < 75 and person.age > 65:  score += 1
        if person.gender == "F":                 score += 1
        if self.strokes > 0:                     score += 2
        return score
    def risk(self, person):
        return self.ANNUAL_RISK_STROKE[self.chads(person)]

class Person(life.AbstractPerson):
    def r_male_age(self):
        x = random.random()
        if x > 0.86:
            return random.randint(81,85)
        elif x > 0.62:
            return random.randint(76,80)
        elif x > 0.33:
            return random.randint(66,75)
        else:
            return random.randint(50,65)
    def r_female_age(self):
        x = random.random()
        if x > 0.93:
            return random.randint(81,85)
        elif x > 0.77:
            return random.randint(76,80)
        elif x > 0.43:
            return random.randint(66,75)
        else:
            return random.randint(50,65)
    def assign_gender(self):
        if random.random <= 0.63: return 'M'
        else:                     return 'F'
    def assign_death(self):
        return 111
    def assign_age(self):
        if self.gender == 'M': return self.r_male_age()
        else:                  return self.r_female_age()
    def possible_events(self): return [Stroke()]

def run():
  p = Person()
  sim = life.Simulation(p, 5) # Run a person defined above for 5 steps (years)
  sim.run(50)                 # 50 individuals
  return [p.events[0].acc_strokes, p.events[0].acc_deaths]

strokes = []
deaths  = []
for _ in range(1000):
   r = run()
   strokes.append(r[0]) 
   deaths.append(r[1])

bins = np.arange(-0.5, 16.5, 1.0)
plt.hist(strokes, normed=1, alpha=0.75, bins=bins)
plt.title("Stroke Simulation in 50 Elderly Patients")
plt.xlabel("Strokes")
plt.ylabel("Density")
plt.show()

print ss.ttest_1samp(strokes, 7.2884)


plt.hist(deaths, normed=1, alpha=0.75,bins=bins)
plt.title("Stroke Simulation in 50 Elderly Patients")
plt.xlabel("Deaths")
plt.ylabel("Density")
plt.show()

print ss.ttest_1samp(deaths, 1.3794)

