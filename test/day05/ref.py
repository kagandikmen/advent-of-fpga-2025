# AoF - Reference Solution for Day 5
# Created:      2025-12-23
# Modified:     2026-01-12
# Author:       Kagan Dikmen

import sys

def merge_ranges(ranges: list[tuple[int, int]]) -> list[list[int]]:
    merged = []

    ranges.sort()

    for r in ranges:
        lo = r[0]
        hi = r[1]
        if not merged or lo > merged[-1][1] + 1:
            merged.append([lo, hi])
        else:
            merged[-1][1] = max(merged[-1][1], hi)

    return merged


def main():
    ranges = []
    ingredients = []

    num_fresh = 0
    num_covered_ids = 0

    with open(sys.argv[1], "r") as f:
        ranges_done = False
        for line in f:
            line = line.strip()
            if not line:
                ranges_done = True
                continue
            if not ranges_done:
                lo, hi = line.split('-')
                ranges.append((int(lo), int(hi)))
            else:
                ingredients.append(int(line))

    ranges = merge_ranges(ranges)

    # Step 1
    for ing_id in ingredients:
        fresh = False
        for r in ranges:
            lower_bd = r[0]
            upper_bd = r[1]
            if lower_bd <= ing_id <= upper_bd:
                fresh = True
                break
        
        num_fresh += fresh == True

    # Step 2
    num_covered_ids = sum(r[1]-r[0]+1 for r in ranges)
    
    print(f'Number of fresh ingredients (Step 1): {num_fresh}')
    print(f'Number of all fresh ingredient IDs (Step 2): {num_covered_ids}')

if __name__ == '__main__':
    main()