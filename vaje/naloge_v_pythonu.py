import random





#######################################################
# 10.  SORTING
#######################################################

def randlist(length, maximum):
    l = []
    for i in range(length):
        l.append(random.randint(0, maximum))
    return l
        
def insert(y, l):
    if l[0] >= y:
        l.insert(0, y)
        return l
    if l[len(l)-1] <= y:
        l.append(y)
        return l
    for i in range(len(l)-2):
        if l[i] <= y and l[i+1] >= y:
            l.insert(i+1, y)
            return l
