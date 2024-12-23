import heapq
from pprint import pprint as pprint

with open("day20/input.txt") as file:
    grid = file.read()
    grid = [list(line) for line in grid.strip().split("\n")]


start = None
end = None
for y, r in enumerate(grid):
    for x, c in enumerate(r):
        if c == "S":
            start = (x, y)
        if c == "E":
            end = (x, y)

if start is None or end is None:
    raise Exception("Start or end not found")

directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]
distances = [[999999999 for _ in r] for r in grid]
visited = set()
unvisited = [(0, start)]
previous = {}

while len(unvisited) > 0:
    distance, (x, y) = heapq.heappop(unvisited)

    if (x, y) in visited:
        continue

    visited.add((x, y))

    if distance < distances[y][x]:
        distances[y][x] = distance

    for d, (dx, dy) in enumerate(directions):
        (nx, ny) = x + dx, y + dy

        if (
            nx >= 0
            and nx < len(grid[0])
            and ny >= 0
            and ny < len(grid)
            and grid[ny][nx] != "#"
        ):
            n_distance = distance + 1
            if n_distance < distances[ny][nx]:
                heapq.heappush(unvisited, (n_distance, (nx, ny)))
                previous[((nx, ny), d)] = (x, y)

distance = distances[end[1]][end[0]]
print(distance)


def part1(grid, cheats):
    directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]
    distances = [[999999999 for _ in r] for r in grid]
    visited = set()
    unvisited = [(0, start)]
    previous = {}

    while len(unvisited) > 0:
        distance, (x, y) = heapq.heappop(unvisited)

        if (x, y) in visited:
            continue

        visited.add((x, y))

        if distance < distances[y][x]:
            distances[y][x] = distance

        for d, (dx, dy) in enumerate(directions):
            (nx, ny) = x + dx, y + dy

            if (
                nx >= 0
                and nx < len(grid[0])
                and ny >= 0
                and ny < len(grid)
                and grid[ny][nx] != "#"
            ):
                n_distance = distance + 1
                if n_distance < distances[ny][nx]:
                    heapq.heappush(unvisited, (n_distance, (nx, ny)))
                    previous[((nx, ny), d)] = (x, y)

    distance = distances[end[1]][end[0]]
    cheats.append(9416 - distance)
    return cheats


cheats = []
for y, r in enumerate(grid):
    for x, c in enumerate(r):
        if grid[y][x] == "#":
            grid2 = [[x for x in y] for y in grid]
            grid2[y][x] = "."
            part1(grid2, cheats)

at_least = []
for c in cheats:
    if c >= 100:
        at_least.append(c)


print(len(at_least))
