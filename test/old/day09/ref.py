# AoF - Reference Solution for Day 9
# Created:      2026-01-02
# Modified:     2026-01-12
# Author:       Kagan Dikmen

import sys

def area(p1, p2):
    dx = abs(p2[0] - p1[0]) + 1
    dy = abs(p2[1] - p1[1]) + 1
    return dx * dy

def is_between(p1, p2, q):
    x = (p2[0] < q[0] < p1[0]) or (p1[0] < q[0] < p2[0])
    y = (p2[1] < q[1] < p1[1]) or (p1[1] < q[1] < p2[1])
    return x and y

def main():
    input_file = sys.argv[1]
    red_tiles = []

    with open(input_file, 'r') as f:
        for line in f:
            x, y = line.strip().split(',')
            red_tiles.append((int(x), int(y)))

    a_max_1 = 0           
    a_max_2 = 0
    border_tiles = set()
    
    for p_idx, p in enumerate(red_tiles):
        q_idx = (p_idx+1) % len(red_tiles)
        q = red_tiles[q_idx]

        if(p[0] == q[0]):
            # means the line is vertical
            rg = range(p[1], q[1]+1) if q[1] >= p[1] else range(q[1], p[1]+1)
            for y in rg:
                border_tiles.add((p[0], y))
        elif(p[1] == q[1]):
            # means the line is horizontal
            rg = range(p[0], q[0]+1) if q[0] >= p[0] else range(q[0], p[0]+1)
            for x in rg:
                border_tiles.add((x, p[1]))

    for p_idx, p in enumerate(red_tiles):
        for q_idx, q in enumerate(red_tiles[p_idx+1:]):
            a = area(p, q)
            if a > a_max_1:
                a_max_1 = a

            if a <= a_max_2:
                continue

            crosses_border = False
            for bt in border_tiles:
                if is_between(p, q, bt):
                    crosses_border = True
                    break
            
            if not crosses_border:
                if a > a_max_2:
                    a_max_2 = a

    print('Part 1: ' + str(a_max_1))
    print('Part 2: ' + str(a_max_2))


if __name__ == '__main__':
    main()
