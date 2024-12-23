import heapq
from pprint import pprint as pprint

with open("./input.txt") as file:
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
distances = [[[999999999 for _ in directions] for _ in r] for r in grid]
visited = set()
unvisited = [(0, start, 1)]
previous = {}

while len(unvisited) > 0:
    distance, (x, y), direction = heapq.heappop(unvisited)

    if ((x, y), direction) in visited:
        continue

    visited.add(((x, y), direction))

    if distance < distances[y][x][direction]:
        distances[y][x][direction] = distance

    for d, (dx, dy) in enumerate(directions):
        (nx, ny) = x + dx, y + dy

        if (
            nx >= 0
            and nx < len(grid[0])
            and ny >= 0
            and ny < len(grid)
            and grid[ny][nx] != "#"
        ):
            cost = 0
            d_diff = abs(direction - d)
            if d_diff == 0:
                cost = 1
            elif d_diff == 2:
                cost = 2001
            else:
                cost = 1001
            n_distance = distance + cost

            if n_distance < distances[ny][nx][d]:
                heapq.heappush(unvisited, (n_distance, (nx, ny), d))
                previous[((nx, ny), d)] = ((x, y), direction)

end_directions = distances[end[1]][end[0]]
min_distance = min(end_directions)
print(min_distance)
