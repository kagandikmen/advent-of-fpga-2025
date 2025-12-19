# AOF - Reference Solution for Day 3 (Part 1 & Part 2)
# Created:      2025-12-19
# Modified:     2025-12-19
# Author:       Kagan Dikmen

import sys

def max_joltage(s: str, w: int) -> int:
    chars = list(s.strip())
    digits = [int(c) for c in chars]

    digits_picked = []

    # pick digits
    for idx in range(w):
        if(idx == w-1):
            digit_pool = digits
        else:
            digit_pool = digits[:(idx-w+1)]
        d = max(digit_pool)
        digits_picked.append(d)
        d_idx = digits.index(d)
        digits = digits[d_idx+1:]

    # list -> decimal
    result = 0
    for dgt in digits_picked:
        result = result * 10 + dgt

    return result


def main():

    joltage_widhts = [2, 12]

    for jw in joltage_widhts:
        max_joltages = []
        with open(sys.argv[1], "r") as f:
            for line in f:
                max_joltages.append(max_joltage(line, jw))

        total_output_joltage = sum(max_joltages)
        print(f"Total output joltage (joltage width = {jw}) = {total_output_joltage}")


if __name__ == '__main__':
    main()
