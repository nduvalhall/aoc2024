import heapq
from pprint import pprint as pprint

with open("day18/grid.txt") as file:
    grid = file.read()
    grid = [list(line) for line in grid.strip().split("\n")]

grid = [["." for _ in range(71)] for _ in range(71)]

coordinates = []
with open("day18/input.txt") as file:
    for line in file.readlines():
        line = line.strip()
        nums = line.split(",")
        coordinates.append((int(nums[0]), (int(nums[1]))))


def part2(i):
    for x, y in coordinates[:i]:
        grid[y][x] = "#"

    start = (0, 0)
    end = (len(grid) - 1, len(grid[0]) - 1)

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
    if distance == 999999999:
        print(coordinates[i - 1])
        return 1


for i in range(1024, len(coordinates)):
    e = part2(i)
    if e == 1:
        break
