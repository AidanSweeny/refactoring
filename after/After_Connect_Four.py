'''
name: Naser Al Madi
file: .py
data: 9/22/2020
course: CS151 fall
description: 
'''

import turtle


def make_window(window_title, bgcolor, size):
	''' this function creates a screen object and returns it '''

	window = turtle.getscreen() # Set the window size
	window.title(window_title)
	window.bgcolor(bgcolor)
	window.setup(*size)
	window.tracer(0) #turns off screen updates for the window Speeds up the game
	return window


def make_turtle(shape, color, stretch_size, size):
    ''' creates a turtle and sets initial position '''

    turt = turtle.Turtle()
    turt.speed(0)    # Speed of animation, 0 is max
    turt.shape(shape)
    turt.color(color)
    turt.shapesize(*stretch_size) 
    turt.penup()
    turt.goto(*size) # Start position
    return turt


def draw_grid(grid, turt, x_pos, y_pos, tile_size):
    ''' draws a grid at x, y with a specific tile_size '''

    place_turtle(turt, (x_pos, y_pos))

    for row in range(len(grid)):
        for col in range(len(grid[row])):
            place_turtle(turt, (x_pos + col * tile_size, y_pos -row * tile_size))

            draw_dot(grid, turt, tile_size,  grid[row][col])

def place_turtle(turt, pos):
    turt.up()
    turt.goto(*pos)
    turt.down()

def draw_dot(grid, turt, tile_size, player):
    player_color = {1 : "red", 2 : "yellow", 0 : "white"}
    # draw color based of player value
    turt.dot(tile_size-5, player_color[player])


def check_win(grid, player, last_row, last_col):
    ''' checks the winner in the grid
    returns true if player won
    returns false if player lost
     '''
    rows = grid[last_row]
    cols = []
    for i in range(len(grid)):
        cols.append(grid[i][last_col])

    count = 0
    # Check rows
    for i in rows:
        if i == player:
            count += 1
        else:
            count = 0
        if count == 4:
            return True
    
    # Check cols
    for i in cols:
        if i == player:
            count += 1
        else:
            count = 0
        if count == 4:
            return True

    # check diagonal
    for row in range(2):
        for col in range(len(grid[0])):
            if col + 3 < len(grid[row]):
                if grid[row][col] == player\
                   and grid[row+1][col+1] == player\
                   and grid[row+2][col+2] == player\
                   and grid[row+3][col+3] == player:
                   return True 
            
            if col - 3 >= 0:
                if grid[row][col] == player\
                   and grid[row+1][col-1] == player\
                   and grid[row+2][col-2] == player\
                   and grid[row+3][col-3] == player:
                   return True 

def play(x_pos, y_pos):
    ''' '''
    global turn
    row = int(abs((y_pos - y_offset - 25) // (50) + 1))
    col = int(abs((x_pos - x_offset - 25) // (50) + 1))
    print(row, col)
    grid[row][col] = turn
    draw_grid(grid, my_turtle, x_offset, y_offset, tile_size)
    window.update()

    if check_win(grid, turn, row, col):
        print("player " + str(turn) + " won")

    if turn == 1:
        turn = 2
    else:
        turn = 1

def main():
    ''' the main function where the game events take place '''

    global turn

    draw_grid(grid, my_turtle, x_offset, y_offset, tile_size)

    while True:
        selected_row = int(input("enter row, player "+ str(turn) +": "))
        selected_col = int(input("enter col, player "+ str(turn) +": "))
        play(selected_row, selected_col)
        
    # window.exitonclick()

if __name__ == "__main__":
    # setting up the window
    window = make_window("Connect 4", "light sky blue", (800, 600))

    # the grid
    grid = []

    for rows in range(5):
        grid.append([0]*7)

    # drawing_turtle
    my_turtle = make_turtle('classic', "white", (1, 1), (0, 0))

    x_offset = -150
    y_offset = 200
    tile_size = 50

    turn = 1

    window.onscreenclick(play)
    window.listen()
    main()
