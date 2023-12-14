from copy import deepcopy

ans: int = 0

with open('input.txt') as f:
    mirror = list(map(lambda line: list(line), f.readlines()))

def slide_north(mirror):
    for i in range(1, len(mirror)):
        for j in range(len(mirror[0])):
            if mirror[i][j] == "O":
                l: int = i
                while l > 0:
                    if mirror[l - 1][j] == ".":
                        l -= 1
                    else:
                        break
                if l != i:
                    mirror[l][j] = "O"
                    mirror[i][j] = "."

def slide_west(mirror):
    for i in range(len(mirror)):
        for j in range(1, len(mirror[0])):
            if mirror[i][j] == "O":
                l: int = j
                while l > 0:
                    if mirror[i][l - 1] == ".":
                        l -= 1
                    else:
                        break
                if l != j:
                    mirror[i][l] = "O"
                    mirror[i][j] = "."

def slide_south(mirror):
    for i in range(len(mirror) - 2, -1, -1):
        for j in range(len(mirror[0])):
            if mirror[i][j] == "O":
                l: int = i
                while l < len(mirror) - 1:
                    if mirror[l + 1][j] == ".":
                        l += 1
                    else:
                        break
                if l != i:
                    mirror[l][j] = "O"
                    mirror[i][j] = "."

def slide_east(mirror):
    for i in range(len(mirror)):
        for j in range(len(mirror[0]) - 2, -1, -1):
            if mirror[i][j] == "O":
                l: int = j
                while l < len(mirror[0]) - 1:
                    if mirror[i][l + 1] == ".":
                        l += 1
                    else:
                        break
                if l != j:
                    mirror[i][l] = "O"
                    mirror[i][j] = "."

# slide everything north west south east
last_mirror = deepcopy(mirror)
previous_mirrors = [last_mirror]
slide_north(mirror)
slide_west(mirror)
slide_south(mirror)
slide_east(mirror)
while True:
    last_mirror = deepcopy(mirror)
    previous_mirrors.append(last_mirror)
    if last_mirror in previous_mirrors[:-1]:
        break
    slide_north(mirror)
    slide_west(mirror)
    slide_south(mirror)
    slide_east(mirror)

# what is the cycle length?
idx_of_first_mirror = previous_mirrors.index(mirror)
cycle_length = len(previous_mirrors) - idx_of_first_mirror - 1

for _ in range((1000000000-idx_of_first_mirror) % cycle_length):
    mirror = previous_mirrors[-1]
    slide_north(mirror)
    slide_west(mirror)
    slide_south(mirror)
    slide_east(mirror)

for i in range(len(mirror)):
    for j in range(len(mirror[0])):
        if mirror[i][j] == "O":
            ans += len(mirror) - i

print(ans)
