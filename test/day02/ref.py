# AoF - Reference Solution for Day 2
# Created:      2025-12-14
# Modified:     2026-01-12
# Author:       Kagan Dikmen

import sys

# for part 1
def is_silly_number(num: int):
    num_digits = len(str(num))

    if (num_digits % 2):
        return False
    
    len_seq = num_digits // 2
    return str(num) == 2 * str(num)[:len_seq]


# for part 2
def is_goofy_number(num: int):
    num_digits = len(str(num))
    num_digits_half = num_digits // 2

    for seq_len in range(1, num_digits_half+1):
        if (num_digits % seq_len):
            continue
        seq_count = num_digits // seq_len
        if str(num) == seq_count * str(num)[:seq_len]:
            return True
    
    return False


def main():
    silly_sum = 0
    sillier_sum = 0

    with open(sys.argv[1], "r") as f:
        line = f.readline()
        intervals = line.split(',')
        
        for interval in intervals:
            values = interval.split('-')
            lower_bound = int(values[0])
            upper_bound = int(values[1])
            for tv in range(lower_bound, upper_bound+1):
                if is_silly_number(tv):
                    silly_sum += tv
                if is_goofy_number(tv):
                    sillier_sum += tv

    print(silly_sum)
    print(sillier_sum)

if __name__ == "__main__":
    main()