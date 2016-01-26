#!/bin/python3

# Ce code n'a malheureusement pas été écris pour être simple et
# maintenable. L'essence de la solution est simpement de faire une
# recherche dans le graphe de configurations possibles pour trouver la
# longueur du chemin jusqu'à la configuration où tous les individus
# sont sur l'autre rive. On entend ici par "configuration" une distribution
# des individus + bateau sur les rives.

from collections import defaultdict
import itertools
import sys

lines = sys.stdin.readlines()
(N, P) = map(int, lines[0].split())

# On met tous les triplets de contraintes dans une liste
constraints_lists = [[int(word) for word in line.split()] for line in lines[1:]]

# On crée une dictionnaire des contraintes conscernant chaque élément
constraints = defaultdict(list)
for (i, o, v) in constraints_lists:
    constraints[i].append((i, o, v))
    constraints[o].append((o, i, v))

# On crée l'ensemble des individus
individus = set(range(1, N+1))

# Les côtés ou se trouvent le bateau, simplement des constantes dans
# le but d'être facilement comparables/hachables
A = "a"
B = "b"

# Une classe configuration qui correspond à un noeud du graphe de
# mouvement de bateau
class Config:
    def __init__(self, sidea, boat):
        self.sidea = sidea
        self.boat = boat

    # La fontion norm() permet de facilement comparer les
    # configurations en obtenant une forme canonique comparable et
    # hachable du contenu de l'objet
    def __norm(self):
        return (self.boat,) + tuple(sorted(self.sidea))
    
    def __lt__(self, other):
        return self.__norm() < other.__norm()
    
    def __hash__(self):
        return hash(self.__norm())

    def __eq__(self, other):
        return self.__norm() == other.__norm()
    
    def get_sidea(self):
        return self.sidea
    def get_sideb(self):
        return individus - self.sidea

    def is_legit(self):
        for side in self.get_sidea(), self.get_sideb():
            if not self.legit_group(side):
                return False
        return True

    def legit_group(self, group):
        for individu_g in group:
            for (i, other, veto) in constraints[individu_g]:
                if other in group and not veto in group:
                    return False
        return True
    
    def neighbors(self):
        boatstart = self.get_sidea() if self.boat == A else self.get_sideb()
        for voyageurs in itertools.chain.from_iterable((itertools.combinations(boatstart, p)) for p in range(1, P+1)):
            voyageurs = frozenset(voyageurs)
            if self.legit_group(voyageurs):
                conf =  Config(self.get_sidea() - voyageurs, B) if self.boat == A else Config(self.get_sidea() | voyageurs, A)
                if conf.is_legit():
                    yield conf

    def __str__(self):
        return str(tuple([self.sidea, self.boat]))
    def __repr__(self):
        return str(self)

start = Config(frozenset(individus), A)
end = Config(frozenset(), B)

class Graph:
    def __getitem__(self, key):
        return key.neighbors()

from collections import defaultdict
from heapq import *

# On fait une simple recherche par l'algorithme de Dijkstra, mais on
# aurait pu sans doute pu applique un A*.

def dijkstra(graph, f, t):
    q, seen = [(0,f,())], set()
    while q:
        (cost,v1,path) = heappop(q)
        if v1 not in seen:
            seen.add(v1)
            path = path + (v1,)
            if v1 == t: return (cost, path)

            for v2 in graph[v1]:
                if v2 not in seen:
                    heappush(q, (cost+1, v2, path)) # Unit step cost

    return (None, tuple())

(cost, path) = dijkstra(Graph(), start, end)
if cost is not None:
    print(cost)
else:
    print(":(")

