#!/bin/python3

width = 10
height = 10

total = width * height
counter = 1

begin = (4, 4)
current = (4, 4)
current_width = 1
dx = 0
dy = 0

print(f"s([4, 4], ' ').")

while counter < total:
    if current == begin:
        current = current[0], current[1] + 1
        begin = current
        current_width += 2
        dx = -1
        dy = 0

    if current[0] in range(10) and current[1] in range(10):
        x, y = current
        print(f"s([{x}, {y}], ' ').")
        counter += 1

    if current == (4 - current_width//2, 4 + current_width//2):
        dx = 0
        dy = -1

    if current == (4 + current_width//2, 4 - current_width//2):
        dx = 0
        dy = 1

    if current == (4 - current_width//2, 4 - current_width//2):
        dx = 1
        dy = 0

    if current == (4 + current_width//2, 4 + current_width//2):
        dx = -1
        dy = 0

    current = current[0] + dx, current[1] + dy

