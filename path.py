# define a 10x10 grid
grid = [[' ' for i in range(10)] for j in range(10)]

# define the start and end positions
grid[0][0] = 'S'
grid[9][9] = 'E'

# define the positions of the obstacles
grid[9][7] = 'O'
grid[8][7] = 'O'
grid[6][7] = 'O'
grid[6][8] = 'O'


# print(grid)
for i in range(10):
    print(grid[i])
