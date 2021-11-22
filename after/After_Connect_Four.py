'''
name: Naser Al Madi
file: .py
data: 9/22/2020
course: CS151 fall
description: 
'''

import turtle

class Window():
    def __init__(self,window_title,bgcolor,size):
        self.window_title=window_title
        self.bgcolor=bgcolor
        self.size=size

    def make_window(self):
        ''' this function creates a screen object and returns it '''

        window = turtle.getscreen() # Set the window size
        window.title(self.window_title)
        window.bgcolor(self.bgcolor)
        window.setup(*self.size)
        window.tracer(0) #turns off screen updates for the window Speeds up the game
        return window
        
class Turtle():

    def __init__(self,shape,color,stretch_size,size):
        self.turt=turtle.Turtle
        self.color=color
        self.stretch_size=stretch_size
        self.size=size
        self.shape=shape

    def make_turtle(self):
        ''' creates a turtle and sets initial position '''

        self.turt = turtle.Turtle()
        self.turt.speed(0)    # Speed of animation, 0 is max
        self.turt.shape(self.shape)
        self.turt.color(self.color)
        self.turt.shapesize(*self.stretch_size) 
        self.turt.penup()
        self.turt.goto(*self.size) # Start position
        
    def get_turtle(self):
        return self.turt

class Grid():
    def __init__(self,window,turtle,nrow,ncol,x_offset,y_offset,tile_size):
        self.window=window
        self.turt=turtle
        self.grid=[]
        self.tile_size=tile_size
        self.player_color = {1 : "red", 2 : "yellow", 0 : "white"}
        self.turn=1
        self.x_offset=x_offset
        self.y_offset=y_offset
        for rows in range(nrow):
            self.grid.append([0]*ncol)
    
    def draw_grid(self):
        ''' draws a grid at x, y with a specific tile_size '''

        self.place_turtle((self.x_offset, self.y_offset))

        for row in range(len(self.grid)):
            for col in range(len(self.grid[row])):
                self.place_turtle( (self.x_offset + col * self.tile_size, self.y_offset -row * self.tile_size))

                self.draw_dot(self.grid[row][col])

    def place_turtle(self, pos):
        self.turt.up()
        self.turt.goto(*pos)
        self.turt.down()

    def draw_dot(self,player):
        # draw color based of player value
        self.turt.dot(self.tile_size-5, self.player_color[player])


    def check_win(self,player, last_row, last_col):
        ''' checks the winner in the grid
        returns true if player won
        returns false if player lost
        '''
        rows = self.grid[last_row]
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

    def play(self):
        ''' '''
        selected_row = int(input("enter row, player "+ str(self.turn) +": "))
        selected_col = int(input("enter col, player "+ str(self.turn) +": "))
        row = int(abs((selected_col - self.y_offset - 25) // (50) + 1))
        col = int(abs((selected_row - self.x_offset - 25) // (50) + 1))
        print(row, col)
        self.grid[row][col] = self.turn
        self.draw_grid()
        self.window.update()

        if self.check_win(self.turn, row, col):
            print("player " + str(self.turn) + " won")

        if self.turn == 1:
            self.turn = 2
        else:
            self.turn = 1
    def initialize(self):
        self.window.onscreenclick(self.play)
        self.window.listen()

def main():
    # setting up the window
    window=Window("Connect 4", "light sky blue",(800, 600)).make_window()
    turtle=Turtle('classic', "white", (1, 1), (0, 0) )
    turtle.make_turtle()
    turtle=turtle.get_turtle()
    grid=Grid(window,turtle,5,7,-150,200,50)    
    grid.initialize()
    grid.draw_grid()   

    while True: 

        grid.play()
if __name__ == "__main__":
	main()
