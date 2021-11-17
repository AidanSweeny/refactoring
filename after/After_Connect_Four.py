'''
name: Naser Al Madi
file: .py
data: 9/22/2020
course: CS151 fall
description: 
'''

from _typeshed import Self
import turtle

class Window():
    def __init__(turtle,window_title,bgcolor,size):
        self.window_title=window_title
        self.bgcolor=bgcolor
        self.size=size
        self.turtle=turtle

    def make_window():
        ''' this function creates a screen object and returns it '''

        window = self.turtle.getscreen() # Set the window size
        window.title(self.window_title)
        window.bgcolor(self.bgcolor)
        window.setup(self.size)
        window.tracer(0) #turns off screen updates for the window Speeds up the game
        return window

class Turtle():

    def __init__(turtle,shape,color,stretch_size,size):
        this.turtle=turtle
        this.color=color
        this.stretch_size=stretch_size
        this.size=size

    def make_turtle():
        ''' creates a turtle and sets initial position '''

        turt = this.turtle.Turtle()
        turt.speed(0)    # Speed of animation, 0 is max
        turt.shape(self.shape)
        turt.color(Self.color)
        turt.shapesize(*self.stretch_size) 
        turt.penup()
        turt.goto(*self.size) # Start position
        return turt

class Grid():
    def __init__(turtle,nrow,ncol,tile_size):
        self.turt=turtle
        self.grid=[]
        self.tile_size=tile_size
        self.player_color = {1 : "red", 2 : "yellow", 0 : "white"}

        for rows in range(nrow):
            self.grid.append([0]*ncol)
    
    def draw_grid( x_pos, y_pos):
        ''' draws a grid at x, y with a specific tile_size '''

        place_turtle(x_pos, y_pos)

        for row in range(len(self.grid)):
            for col in range(len(self.grid[row])):
                place_turtle( (x_pos + col * self.tile_size, y_pos -row * self.tile_size))

                draw_dot(self.grid,self.tile_size,  self.grid[row][col])

    def place_turtle( pos):
        self.turt.up()
        self.turt.goto(*pos)
        self.turt.down()

    def draw_dot(player):
        # draw color based of player value
        self.turt.dot(self.tile_size-5, self.player_color[player])


    def check_win( player, last_row, last_col):
        ''' checks the winner in the grid
        returns true if player won
        returns false if player lost
        '''
        rows = grid[last_row]
        cols = []
        for i in range(len(self.grid)):
            cols.append(self.grid[i][last_col])

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
            for col in range(len(self.grid[0])):
                if col + 3 < len(self.grid[row]):
                    if self.grid[row][col] == player\
                    and self.grid[row+1][col+1] == player\
                    and self.grid[row+2][col+2] == player\
                    and self.grid[row+3][col+3] == player:
                    return True 
                
                if col - 3 >= 0:
                    if self.grid[row][col] == player\
                    and self.grid[row+1][col-1] == player\
                    and self.grid[row+2][col-2] == player\
                    and self.grid[row+3][col-3] == player:
                    return True 

    def play(x_pos, y_pos):
        ''' '''
        global turn
        row = int(abs((y_pos - self.y_offset - 25) // (50) + 1))
        col = int(abs((x_pos - self.x_offset - 25) // (50) + 1))
        print(row, col)
        grid[row][col] = turn
        draw_grid(self.x_offset, self.y_offset)
        window.update()

        if check_win(turn, row, col):
            print("player " + str(turn) + " won")

        if turn == 1:
            turn = 2
        else:
            turn = 1


class Game():
    def __init__(x_offset=-150,y_offset=200,tile_size=50,window_length=800,window_width=600,nrow=5,ncol=7):
    # setting up the window

        window = make_window("Connect 4", "light sky blue", (window_length, window_width))



        my_turtle = make_turtle('classic', "white", (1, 1), (0, 0))


        window.onscreenclick(play)
        window.listen()

        draw_grid( x_offset, y_offset)

        while True: 
            selected_row = int(input("enter row, player "+ str(turn) +": "))
            selected_col = int(input("enter col, player "+ str(turn) +": "))
            play(selected_row, selected_col)
``