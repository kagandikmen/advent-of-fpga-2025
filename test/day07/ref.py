# AoF - Reference Solution for Day 7
# Created:      2025-12-28
# Modified:     2026-01-12
# Author:       Kagan Dikmen

import sys

def main():
    input_file = sys.argv[1]
    dim = 0
    rays = []

    with open(input_file, 'r') as f:
        lines = f.readlines()
        dim = len(lines[0])
        split_counter = 0

        rays.append([1 if ch == 'S' else 0 for ch in lines[0]])
        
        for line in lines[1:]:
            rays_n = [0 for _ in range(dim)]
            rays_l = rays[-1]
            ray_split = False

            for c_idx, ch in enumerate(line):
                if c_idx == 0 or c_idx == dim-1:
                    rays_n[c_idx] = rays_l[c_idx]
                    ray_split = False
                    continue

                if ch == '^' and rays_l[c_idx] > 0:
                    rays_n[c_idx-1] += rays_l[c_idx]
                    rays_n[c_idx] = 0
                    ray_split = True
                    split_counter += 1
                elif ch == '.':
                    if ray_split:
                        rays_n[c_idx] += rays_l[c_idx] + rays_l[c_idx-1]
                    else:
                        rays_n[c_idx] = rays_l[c_idx]
                    ray_split = False

            rays.append(rays_n)
    
    print(f'Part 1: {split_counter}')
    print(f'Part 2: {sum(rays[-1])}')


if __name__ == '__main__':
    main()
