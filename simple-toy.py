import random

def rMaleAge():
    x = random.random()
    if x > 0.86:
        return random.randint(81,85)
    elif x > 0.62:
        return random.randint(76,80)
    elif x > 0.33:
        return random.randint(66,75)
    else:
        return random.randint(50,65)

def rFemaleAge():
    x = random.random()
    if x > 0.93:
        return random.randint(81,85)
    elif x > 0.77:
        return random.randint(76,80)
    elif x > 0.43:
        return random.randint(66,75)
    else:
        return random.randint(50,65)

class RandomPerson(object):
    ANNUAL_RISK_STROKE = [0.0, 0.013, 0.022, 0.032, 0.04, 0.067, 0.098, 0.096, 0.067, 0.152]
    def __init__(self):
        self.gender = "?"
        self.age = -1
        self.nStrokes = 0
    def reset(self):
        x = random.random()
        if x > 0.63:
            self.gender = "F"
            self.age = rFemaleAge()
        else:
            self.gender = "M"
            self.age = rMaleAge()
        x = random.random()
        if x > 0.9:
            self.nStrokes = 2
        elif x > 0.65:
            self.nStrokes = 1
        else:
            self.nStrokes = 0
    def chads(self):
        score = 0
        if self.age >= 75:                   score += 2
        if self.age < 75 and self.age > 65:  score += 1
        if self.gender == "F":               score += 1
        if self.nStrokes > 0:                score += 2
        return score
    def risk(self):
        return self.ANNUAL_RISK_STROKE[self.chads()]
    def alive(self):
        return self.age >= 0
    def dead(self):
        return self.age < 0
    def growOlder(self):
        if random.random() <= self.risk():
            self.nStrokes += 1
            if random.random() <= 0.19:
                self.age = -1 # DEAD
            else: 
                self.age += 1
            return True
        else:
            self.age += 1
            return False

p = RandomPerson()
nStrokes = 0
nDeaths  = 0
for _ in range(50*10000):
    p.reset()
    for _ in range(6):
        if p.growOlder():
            nStrokes += 1
            if p.dead():
                nDeaths += 1
                break

print(nStrokes / 10000.0)
print(nDeaths  / 10000.0)
    
