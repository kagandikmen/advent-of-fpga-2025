# AOF - Reference Solution for Day 11 (Part 1 & Part 2)
# Created:      2026-01-03
# Modified:     2026-01-03
# Author:       Kagan Dikmen

import sys

def build_adj(edges):
    adj = {}
    for u, v in edges:
        if u not in adj:
            adj[u] = []
        adj[u].append(v)
    return adj

def count_paths(adj, start, end):

    memo_p1 = {}
    memo_p2 = {}
    
    def dfs_normal(u):
        if u == end:
            return 1
        if u in memo_p1:
            return memo_p1[u]
        total = 0
        for v in adj.get(u, []):
            total += dfs_normal(v)
        memo_p1[u] = total
        return total
    
    def dfs_part2(u, seen_dac, seen_fft):
        if u == 'dac':
            seen_dac = True
        
        if u == 'fft':
            seen_fft = True

        search_term = (u, seen_dac, seen_fft)
        if search_term in memo_p2:
            return memo_p2[search_term]

        if u == end:
            return 1 if seen_dac and seen_fft else 0
        
        total = 0
        for v in adj.get(u, []):
            total += dfs_part2(v, seen_dac, seen_fft)
        
        memo_p2[search_term] = total
        return total
    
    return dfs_normal(start), dfs_part2(start, False, False)


def main():
    input_file = sys.argv[1]

    graph = set()
    
    with open(input_file, 'r') as f:
        for line in f:
            source_node, target_nodes = line.strip().split(':')
            target_nodes_list = target_nodes.strip().split()
            for tn in target_nodes_list:
                graph.add((source_node, tn))
    
    adj = build_adj(graph)
    p1, _ = count_paths(adj, 'you', 'out')
    _, p2 = count_paths(adj, 'svr', 'out')

    print(p1)
    print(p2)


if __name__ == '__main__':
    main()
