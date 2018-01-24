##########################################################################
# Želimo definirati pivotiranje na mestu za tabelo a.
# Ker bi želeli pivotirati zgolj dele tabele a, se hkrati omejimo na
# del tabele, ki se nahaja med indeksoma start in end.
# Na primer, za start = 0 in end = 8 tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo pivot_list(a, start, end), ki preuredi tabelo a tako,
# da bo a[start] postal pivot za del tabele med indeksoma start in end.
# Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele a.
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot_list(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
##########################################################################


def pivot_list(a, start, end):
    pivot = a[start]
    i = start
    j = end
    while j - i  != 0:
        print(a)
        if a[i+1] == pivot:
            i += 1
        if a[j] == pivot:
            j -= 1
        if a[i+1] > pivot and a[j] < pivot:
            a[i+1], a[j] = a[j], a[i+1]
            i += 1
            j -= 1
        elif a[i+1] > pivot and a[j] > pivot:
            j -= 1
        elif a[i+1] < pivot and a[j] < pivot:
            i += 1
        else:
            i += 1
            j += 1
    a[start], a[i] = a[i], pivot
    return i



'''
def pivot_list(a, start, end) :
    # Shrani pivot
    pivot = a[start]
    # Shrani kazalca
    front_i = start
    back_i = end
    # Premikaj kazalca in zamenjaj elemente če potrebno
    while front_i != back_i :
        if a[front_i + 1] <= pivot :
            front_i += 1
        elif a[back_i] > pivot :
            back_i -= 1
        else:
            temp = a[front_i + 1]
            a[front_i + 1] = a[back_i]
            a[back_i] = temp
    # Premakni pivot na pravo mesto
    a[start] = a[front_i]
    a[front_i] = pivot
    # Vrni indeks pivota
    return front_i
'''
#a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#print(pivot_list(a, 1, 7))


##########################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja, ki smo ga
# spoznali na predavanjih.
#
# Napišite funkcijo quicksort(a), ki uredi tabelo a s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: definirajte pomožno funkcijo quicksort_part(a, start, end), ki
#        uredi zgolj del tabele a.
#
#   >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#   >>> quicksort(a)
#   [2, 3, 4, 5, 10, 11, 15, 17, 18]
##########################################################################


def quicksort_part(a, start, end):
    if start >= end:
        return
    else:
        pivot_i = pivot_list(a, start, end)
        quicksort_part(a, start, pivot_i - 1)
        quicksort_part(a, pivot_i + 1, end)
        return

def quicksort(a):
    quicksort_part(a, 0, len(a)-1)
    return

#a1 = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#print(quicksort(a1))



#############################################################
# DOLZINA NAJDALJSEGA STROGEGA NARASCAJOCEGA ZAPOREDJE
#############################################################

s = [7, 2, 5, 6, 1, 9, 13, 8, 7, 5, 12, 18, 1, 8, 7, 5, 4, 3, 2]

def najdaljse_strogo_narascajoce_zaporedje(s):
    n = len(s)
    l = [1] * n
    for j in range(n):
        for i in range(j):
            if s[i] < s[j]:
                l[j] = max(l[j], 1 + l[i])
    print(l)
    return max(l)

print(najdaljse_strogo_narascajoce_zaporedje(s))

#############################################################
# DOLZINA NAJDALJSEGA STROGEGA NARASCAJOCE ZAPOREDJE
#############################################################


a = ["o", "p", "e", "r", "a", "c", "i", "j", "s", "k", "e", "r", "a"]
b = ["d", "r", "u", "g", "i", "l", "e", "t", "n", "i", "k"]

def najdaljse_skupno_podzaporedje(a, b):
    m = len(a)
    n = len(b)
    tabela = [[0] * (m+1)] * (n+1)
    for i in range(1,n+1):
        for j in range(1,m+1):
            if a[j-1] == b[i-1]:
                tabela[i][j] = 1 + tabela[i-1][j-1]
            else:
                tabela[i][j] = max(tabela[i-1][j], tabela[i][j-1])
    return tabela[n][m]

print(najdaljse_skupno_podzaporedje(a, b))


#############################################################
# HUMANIRARNA KRIZA
#############################################################

L1 = [0, 45, 70, 90, 105, 120]
L2 = [0, 20, 45, 75, 110, 150]
L3 = [0, 50, 70, 80, 100, 100]

def humanitarna_kriza(l1, l2, l3):
    n = len(L1)
    prvi = L1
    prvi_in_drugi = [0] * n
    prvi_drugi_tretji = [0] * n
    for i in range(n):
        a = []
        for l in range(i+1):
            a.append((prvi[i-l] + L2[l]))
        prvi_in_drugi[i] = max(a)
    for i in range(n):
        a = []
        for l in range(i+1):
            a.append((prvi_in_drugi[i-l] + L3[l]))
        prvi_drugi_tretji[i] = max(a)
    return prvi_drugi_tretji[n-1]


print(humanitarna_kriza(L1, L2, L3))


#############################################################
# MNOZENJE ZAPOREDJA MATRIK
#############################################################

A1 = [6, 7]
A2 = [7, 3]
A3 = [3, 2]
A4 = [2, 3]

def mnozenje_zaporedja_matrik(A1, A2, A3, A4):
    p0 = [A1[0]]
    p1 = [A2[0]]
    p2 = [A3[0]]
    p3 = [A4[0]]
    p4 = [A4[1]]
    p = p0 + p1 + p2 + p3 + p4
    n = 4
    m = [[0 for _ in range(n)] for _ in range(n)]
    # mnozenje dveh matrik
    for i in range(n-1):
        m[i][i+1] = p[i]*p[i+1]*p[i+2]
    return m


print(mnozenje_zaporedja_matrik(A1, A2, A3, A4))



#############################################################
# MINIMALNA VSOTA PO MATRIKI
#############################################################
import sys

def minCost(cost, m, n):
    if (n < 0 or m < 0):
        return sys.maxsize
    elif (m == 0 and n == 0):
        return cost[m][n]
    else:
        return cost[m][n] + min(minCost(cost, m-1, n), minCost(cost, m, n-1))


cost= [ [1, 2, 3],
        [4, 8, 2],
        [1, 5, 3] ]

print(minCost(cost, 2,2))





##################PREDAVANJA#################################


def najdaljse_skupno(xs, ys):
    if not xs or not ys:
        return ()
    elif xs[0] == ys[0]:
        return (xs[0],) + najdaljse_skupno(xs[1:], ys[1:])
    else:
        l = najdaljse_skupno(xs, ys[1:])
        d = najdaljse_skupno(xs[1:], ys)
        return l if len(l) >= len(d) else d


def najdaljsi_podpalindrom(niz):
    if len(niz) <= 1:
        return niz
    elif niz[0] == niz[-1]:
        return niz[0] + najdaljsi_podpalindrom(niz[1:-1]) + niz[-1]
    else:
        levi = najdaljsi_podpalindrom(niz[:-1])
        desni = najdaljsi_podpalindrom(niz[1:])
        return levi if len(levi) >= len(desni) else desni


def stolpi(n):
    if n == 0:
        return 1
    return modri_stolpi(n) + rdeci_stolpi(n)


def modri_stolpi(n):
    if n == 0:
        return 1
    elif n < 0:
        return 0
    vsota = 0
    for k in [2, 3]:
        vsota += rdeci_stolpi(n - k)
    return vsota


def rdeci_stolpi(n):
    if n == 0:
        return 1
    elif n < 0:
        return 0
    vsota = 0
    for k in [1, 2]:
        vsota += modri_stolpi(n - k)
    return vsota