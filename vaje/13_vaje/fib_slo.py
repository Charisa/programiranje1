from functools import lru_cache
import numpy as np

# Cilj: izračunajte vrednosti Fibonaccijevega zaporadja za 100, 500, 1000,
# 10**5, and 10**6 člen.
# Za vsako definicijo preizkusite kako pozne člene lahko izračuante in poglejte
# zakaj se pojavi problem (neučinkovitost, pregloboka rekurzija,
# premalo spomina ...).

# Definirajte naivno rekurzivno različico.
# Omejitev: Prepočasno.

def fib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    return (fib(n-1) + fib(n-2))

# Z uporabo dekoratorja izboljšajte naivno različico.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~350.

@lru_cache()

def fib_cache(n):
    if n<= 1:
        return n
    else:
        return fib_cache(n-1) + fib_cache(n-2)
        

# Nariši drevo klicov za navadno rekurzivno fib funkcijo pri n=5 in
# ugotovi kateri podproblemi so klicani večkrat.

# Definirajte rekurzivno memoizirano funkcijo fib brez uporabe dekoratorja.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~1000.

def fib_memo_rec(n):
    res = [None] * max(n+1, 2)
    res[0] = 0
    res[1] = 1
    def aux(n):
        if res[n] != None:
            return res[n]
        else:
            x = aux(n-1) + aux(n-2)
            res[n] = x
            return x
    return aux(n) 

# Na katere podprobleme se direktno skicuje rekurzivna definicija fib?

# Definirajte fib ki gradi rezultat od spodaj navzgor (torej računa in si zapomni
# vrednosti od 1 proti n.)

def fib_memo_iter(n):
    a = [0, 1]
    for i in range(n-1):
        a.append(a[i] + a[i+1])
    return a[len(a)-1]


# Izboljšajte prejšnjo različico tako, da hrani zgolj rezultate, ki jih v
# nadaljevanju nujno potrebuje.

def fib_iter(n):
    if n < 2:
        return n
    a = [0, 1]
    for i in range(1,n+1):
        if i % 2 != 0:
            a[0] += a[1]
        else:
            a[1] += a[0]
            
    if n % 2 == 0:
        return a[1]
    else:
        return a[0]


# x_2 = 0
# x_1 = 1
# for i in range(2,n+1):
#   x = x_1 + x_2
# x_2 = x_1
# x_1 = x
#return x





    
