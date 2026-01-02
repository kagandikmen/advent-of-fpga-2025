# AOF - Reference Solution for Day 8 (Part 1 & Part 2)
# Created:      2025-12-31
# Modified:     2026-01-02
# Author:       Kagan Dikmen

import sys

class Graph:
    
    def __init__(self, n_vertices):
        self.parents = []
        self.sizes = []
        self.num_mst = n_vertices

        for i in range(n_vertices):
            self.parents.append(i)
            self.sizes.append(1)

    def find_eldest_parent_idx(self, idx_vertex):
        while self.parents[idx_vertex] != idx_vertex:
            idx_vertex = self.parents[idx_vertex]
        return idx_vertex
    
    def merge(self, a_idx, b_idx):
        ep_a_idx = self.find_eldest_parent_idx(a_idx)
        ep_b_idx = self.find_eldest_parent_idx(b_idx)
        
        if ep_a_idx == ep_b_idx:
            return False
        
        self.parents[ep_b_idx] = ep_a_idx
        self.sizes[ep_a_idx] += self.sizes[ep_b_idx]
        self.num_mst -= 1

        return True

def sq_distance(a, b):
    result = 0
    for d in range(3):
        result += (b[d] - a[d])**2
    return result

def main():
    input_file = sys.argv[1]
    boxes = []
    edges = []

    with open(input_file, 'r') as f:
        for line in f:
            line = line.strip().split(',')
            boxes.append([int(p) for p in line])
    
    num_boxes = len(boxes)

    for i in range(num_boxes):
        b = boxes[i]
        for j in range(i+1, num_boxes):
            d = sq_distance(b, boxes[j])
            edges.append((d, i, j))

    def return_distance(t):
        return t[0]

    edges.sort(key = return_distance)

    graph_1 = Graph(num_boxes)
    for idx in range(50):
        graph_1.merge(edges[idx][1], edges[idx][2])

    tree_sizes = [0 for _ in range(num_boxes)]
    for idx in range(num_boxes):
        ep_idx = graph_1.find_eldest_parent_idx(idx)
        tree_sizes[ep_idx] += 1
    
    tree_sizes.sort()
    part_1 = tree_sizes[-1] * tree_sizes[-2] * tree_sizes[-3]

    graph_2 = Graph(num_boxes)
    last_i = -1
    last_j = -1
    for edge in edges:
        d, i, j = edge
        did_merge = graph_2.merge(i, j)
        if did_merge:
            last_i, last_j = i, j
    
    part_2 = boxes[last_i][0] * boxes[last_j][0]

    print(f'Part 1: {part_1}')
    print(f'Part 2: {part_2}')


if __name__ == '__main__':
    main()
