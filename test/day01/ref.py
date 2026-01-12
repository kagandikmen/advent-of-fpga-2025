# AoF - Reference Solution for Day 1
# Created:      2025-12-10
# Modified:     2026-01-12
# Author:       Kagan Dikmen

from typing import Tuple
import sys

def turn_knob(status: int, value: int) -> Tuple[int, int]:

    turn_right = value >= 0
    turn_abs = abs(value)
    q = turn_abs // 100
    r = turn_abs % 100
    
    for _ in range(r):
        if turn_right:
            status += 1
        else:
            status -= 1

        if (status == 0) or (status == 100):
            q += 1

    pos_final = status % 100

    return pos_final, q

def main():
    pos = 50
    zero_count = 0
    hit_count = 0

    with open(sys.argv[1], "r") as f:
        for line in f:
            line = line.strip()

            turn_value = int(line[1:])
            turn_direction = line[0]

            match turn_direction:
                case "R":
                    new_pos, q = turn_knob(pos, turn_value)
                case "L":
                    new_pos, q = turn_knob(pos, -turn_value)

            
            zero_count += (new_pos == 0)
            hit_count += q

            pos = new_pos
    
    print(zero_count)
    print(hit_count)

if __name__ == "__main__":
    main()
