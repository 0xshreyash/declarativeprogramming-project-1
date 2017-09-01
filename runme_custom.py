#!/usr/bin/env python
import subprocess
import os
import sys
import itertools
import time

from collections import Counter


def write_load(perc):
    n = int(perc/100*80) 
    np = 80 - n
    sys.stdout.write("\r\tRunning all chords [" + "="*+(n) + ">" + " "*np + "]")

def analyze(lst):
    c = Counter(lst)
    for k in sorted(c.keys()):
        print("Number of {} guess(es): {}".format(k, c[k]))

chords = [a+d for a in "ABCDEFG" for d in "123"]

space = list(itertools.combinations(chords, 3))
assert len (space) == 1330
unknown = 0
g = []
start = time.time()
for i, chord in enumerate(space):
    perc = (i+1)/1330*100
    # if i%5 and int(perc)%5 == 0:
    write_load(perc)
    sys.stdout.write("\t\t{:5.2f}%".format(perc))
    retval = subprocess.check_output(['./Proj1Test', chord[0], chord[1], chord[2]])
    guesses = retval.decode().split("\n")[-2]
    fail = not guesses.endswith("guesses!")
    unknown += fail
    if not fail:
        guesses_made = int(guesses.split()[-2])
        g.append(guesses_made)


print("\n\nTime elapsed: {} seconds".format(time.time() - start))
print("The average guesses were: {}".format(sum(g)/len(g)))
print("The highest guess was: {}".format(max(g)))
print("The lowest guess was: {}".format(min(g)))
print("The number of failed runs were: {}".format(unknown))
analyze(g)
