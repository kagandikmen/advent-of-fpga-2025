# AoF - Reference Solution for Day 6
# Created:      2025-12-26
# Modified:     2026-01-12
# Author:       Kagan Dikmen

import math
import sys

def solve_parts(input_txt):
    result_p1 = 0
    result_p2 = 0

    lines = input_txt.splitlines()
    line_width = len(lines[0])

    is_col_separator = [all(line[c_idx] == ' ' for line in lines) for c_idx in range(line_width)]

    opblocks = []
    c_idx = 0
    while c_idx < line_width:
        if is_col_separator[c_idx]:
            c_idx += 1
            continue

        start = c_idx
        while c_idx < line_width and not is_col_separator[c_idx]:
            c_idx += 1
        end = c_idx
        opblocks.append([line[start:end] for line in lines])

    for opblk in opblocks:
        opds = opblk[:-1]
        op = opblk[-1].strip()

        opds_p1 = [int(opd.strip()) for opd in opds]
        
        opds_p2 = []
        opblk_width = len(opds[0])
        for col_idx in range(opblk_width-1, -1, -1):
            d = "".join(row[col_idx] for row in opds)
            # the vacant digits are represented as spaces in the string d,
            # so they vanish when we do this:
            if d:
                opds_p2.append(int(d))

        match op:
            case '+':
                result_p1 += sum(opds_p1)
                result_p2 += sum(opds_p2)
            case '*':
                result_p1 += math.prod(opds_p1)
                result_p2 += math.prod(opds_p2)
            case _:
                raise ValueError

    return result_p1, result_p2


def main():
    input_file = sys.argv[1]
    with open(input_file, 'r') as f:
        input = f.read()

        p1, p2 = solve_parts(input)

        print(f'Part 1: {p1}')
        print(f'Part 2: {p2}')

    return 0

if __name__ == '__main__':
    main()
