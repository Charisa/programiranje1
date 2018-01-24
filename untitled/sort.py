# ALGORITMI ZA SORTIRANJE

from functools import lru_cache
memoizacija = lru_cache(maxsize=None)


# BUBBLE SORT

def bubble_sort(sez):
    n = len(sez)
    while n > 0:
        for i in range(n-1):
            if sez[i] > sez[i + 1]:
                sez[i], sez[i + 1] = sez[i + 1], sez[i]
        n -= 1
    return sez

s = [3,5,1,8,2,9,15,3,2]
print(bubble_sort(s))


# SELECTION SORT

def selection_sort(sez):
    n = 0
    while n < len(sez):
        min = sez[n]
        swap = n
        for i in range(n,len(sez)):
            if sez[i] < min:
                min = sez[i]
                swap = i
        sez[swap], sez[n] = sez[n], min
        n += 1
    return sez

s1 = [3,5,1,8,2,9,15,3,2]
print(selection_sort(s1))

# INSERTION SORT

def insertion_sort(sez):
    n = 0
    while n < len(sez):
        e = sez[n]
        for i in range(n-1,-1,-1):
            if sez[i] >= e and sez[i-1] <= e:
                sez[i], sez[i+1] = e, sez[i]
            elif sez[i] >= e:
                sez[i+1], sez[i] = sez[i], e
        n += 1
    return sez

s2 = [3,5,1,8,2,9,15,3,2]
print(insertion_sort(s2))


# MERGE SORT

def zlij(s, s1):
        new = []
        i = 0
        j = 0
        while i < len(s) and j < len(s1):
            if s[i] >= s1[j]:
                new.append(s1[j])
                j += 1
            else:
                new.append(s[i])
                i += 1
        while i < len(s):
            new.append(s[i])
            i += 1
        while j < len(s1):
            new.append(s1[j])
            j += 1
        return new

def merge_sort(sez):
    if len(sez) == 1:
        return sez
    n = len(sez)
    sez1 = sez[0:(n // 2)]
    sez2 = sez[(n // 2):n]
    a = merge_sort(sez1)
    b = merge_sort(sez2)
    return zlij(a, b)


s3 = [3, 5, 1, 8, 2, 9, 15, 3, 2]
print(merge_sort(s3))





