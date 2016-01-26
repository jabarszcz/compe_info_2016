#!/usr/bin/env python3

import argparse
import random

# Probably slow
# From http://stackoverflow.com/questions/22229796/choose-at-random-from-combinations
def random_combination(iterable, r):
    "Random selection from itertools.combinations(iterable, r)"
    pool = tuple(iterable)
    n = len(pool)
    indices = sorted(random.sample(range(n), r))
    return tuple(pool[i] for i in indices)

parser = argparse.ArgumentParser()
parser.add_argument("N", type=int)
parser.add_argument("B", type=int)
parser.add_argument("C", type=int)
parser.add_argument("seed", type=int)
args = parser.parse_args()

random.seed(a=args.seed)

class Constraint:
    def __init__(self, a, b, c):
        (self.a, self.b) = sorted((a, b))
        self.c = c
    def __str__(self):
        return " ".join(str(x) for x in [self.a, self.b, self.c])
    def __eq__(self, other):
        return sorted((self.a, self.b)) == sorted((other.a, other.b)) and self.c == other.c
    def __hash__(self):
        return hash(tuple(sorted((self.a, self.b))) + (self.c,))
        
# This way of generating constraints does not guarantee a solution to
# exist. The seed must be well chosen (via trial and error).
constraints = set(Constraint(*random_combination(range(1, args.N+1), 3)) for i in range(args.C))

print(args.N, args.B)
for c in constraints:
    print(c)
