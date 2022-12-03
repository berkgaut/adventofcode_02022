#!/usr/bin/env python3

elves1 = []

with open("1.txt") as f:
    elf = list()
    for line in f.readlines():
        if len(line)==1:
            elves1.append(elf)
            elf = list()
        else:
            elf.append(int(line))

elves2 = list(map(lambda elf: sum(elf), elves1))
elves2.sort()

# part 1

print(elves2[-1])

# part 2

print(sum(elves2[-3:]))



