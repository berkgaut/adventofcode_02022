#!/usr/bin/env python3

theirs_code={'A': 'R', 'B': 'P', 'C': 'S'}
ours_code={'X': 'R', 'Y': 'P', 'Z': 'S'}
score={'R': 1, 'P':2, 'S':3}

# 0 - lose, 1 - draw, 2 - win
command_code={'X':0, 'Y':1, 'Z': 2}

ours_move = {
    'R': 'SRP',
    'P': 'RPS',
    'S': 'PSR',
}

# first column ours, second column theirs
outcome={
    'RR': 3,
    'RP': 0,
    'RS': 6,
    'PR': 6,
    'PP': 3,
    'PS': 0,
    'SR': 0,
    'SP': 6,
    'SS': 3,
}

with open("../data/02.txt") as f:
    total_score = 0
    for line in f.readlines():
        theirs = theirs_code[line[0]]
        command = command_code[line[2]]
        ours = ours_move[theirs][command]
        round_score = score[ours] + outcome[ours + theirs]
        total_score += round_score
print(total_score)

