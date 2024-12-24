import networkx as nx

with open("day23/test.txt") as file:
    edges = []
    for line in file.readlines():
        line = line.strip()
        nodes = line.split("-")
        edges.append((nodes[0], nodes[1]))

from itertools import combinations

G = nx.Graph()
G.add_edges_from(edges)
nodes = list(G.nodes())
triplets = combinations(nodes, 3)

def is_clique(triplet, graph):
    return all(graph.has_edge(u, v) for u, v in combinations(triplet, 2))

cliques_of_size_3 = [triplet for triplet in triplets if is_clique(triplet, G)]
cliques_of_size_3_sorted: tuple[str,str,str] = sorted(cliques_of_size_3)

cliques_with_t = []
for cs in cliques_of_size_3_sorted:
    (a, b, c) = cs
    if a.startswith("t") or b.startswith("t") or c.startswith("t"):
        cliques_with_t.append(cs)

print(len(cliques_with_t))
