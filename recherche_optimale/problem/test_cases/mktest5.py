#!/usr/bin/env python3

import sys;

top = int(sys.argv[1])
ncases = int(sys.argv[1])

print(top, ncases)
print(*(i for i in range(1, top+1)))
for i in range(1, top+1):
    print(i)
