import networkx as nx

with open("day23/input.txt") as file:
    edges = []
    for line in file.readlines():
        line = line.strip()
        nodes = line.split("-")
        edges.append((nodes[0], nodes[1]))


G = nx.Graph()
G.add_edges_from(edges)

cliques = list(nx.find_cliques(G))
max_clique = ",".join(sorted(max(cliques, key=lambda x: len(x))))
print(max_clique)
