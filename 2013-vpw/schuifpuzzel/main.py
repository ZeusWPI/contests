from sys import stdin
from collections import deque

P = [(-1, 0), (0, 1), (1, 0), (0, -1)]

def handle_case():

    height, width = map(int, stdin.readline().split())
    startstr = stdin.readline()
    endstr = stdin.readline()
    puzzle = list()
    target = list()
    for r in range(height):
        puzzle.append(list())
        target.append(list())
        for c in range(width):
            puzzle[r].append(startstr[r * width + c])
            target[r].append(endstr[r * width + c])
            if puzzle[r][c] == ' ':
                empty = (r, c)

    r, c = empty
    steps = 0
    puzzlesdeque = deque()
    latest = -1
    while(puzzle != target):
        if steps == 15:
            steps == 14
            break
        for i in range(4):
            if latest == (i + 2) % 4:
                continue
            (dr, dc) = P[i]
            if 0 <= r + dr < height and 0 <= c + dc < width:
                newpuzzle = list()
                for row in puzzle: newpuzzle.append(list(row))
                newpuzzle[r + dr][c + dc] = puzzle[r][c]
                newpuzzle[r][c] = puzzle[r + dr][c + dc]
                puzzlesdeque.append((i, (r + dr, c + dc), steps + 1, newpuzzle))
        (latest, (r, c), steps, puzzle) = puzzlesdeque.popleft()

    print(steps)


if __name__ == "__main__":
    cases = int(stdin.readline())
    for i in range(cases):
        handle_case()
