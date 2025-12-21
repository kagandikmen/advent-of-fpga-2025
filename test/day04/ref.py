# AOF - Reference Solution for Day 4 (Part 1 & Part 2)
# Created:      2025-12-21
# Modified:     2025-12-21
# Author:       Kagan Dikmen

import sys

dim = 138

def main():
    grid = [[0] * (dim+2) for _ in range(dim+2)]

    accessible_list = []
    num_accessible = 1
    num_removed = 0

    with open(sys.argv[1], "r") as f:
        for li, line in enumerate(f):
            for ci, char in enumerate(line.strip()):
                grid[li+1][ci+1] = char == '@'

    iter_cnt = 0
    while(num_accessible != 0):
        
        for li in range(1, dim+1):
            for ci in range(1, dim+1):

                adjacent_paper_rolls = -grid[li][ci]
                for li_n in range(li-1, li+2):
                    for ci_n in range(ci-1, ci+2):
                        adjacent_paper_rolls += grid[li_n][ci_n]

                accessible = (adjacent_paper_rolls < 4) * grid[li][ci]
                if accessible:
                    accessible_list.append((li, ci))
        
        num_accessible = len(accessible_list)

        while accessible_list:
            li, ci = accessible_list.pop()
            grid[li][ci] = 0
        
        num_removed += num_accessible

        if(iter_cnt == 0):
            print(f"Step 1: {num_accessible}")

        iter_cnt += 1
    
    print(f"Step 2: {num_removed}")


if __name__ == '__main__':
    main()