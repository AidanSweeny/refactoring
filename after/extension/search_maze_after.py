
import turtle
import time
from playsound import playsound
from multiprocessing import Process



class Grid():
    def __init__(self,window,turt,tile_size,x_offset,y_offset):
        self.steps=0
        self.window=window
        self.grid=[]
        self.turt=turt
        self.tile_size=tile_size
        self.x_offset=x_offset
        self.y_offset=y_offset
        self.position_dot_map={"X":['grey',"black"],"S":['grey',"yellow"],"E":['grey',"red"],'P':['grey',"royalblue"],'T':['grey', "light blue"],'D':[('gainsboro', "gray")]}
    def draw_dot(self,type):
        if type in self.position_dot_map.keys():
            color=self.position_dot_map[type]
            self.turt.color(color[0],color[1])
        else:
            self.turt.color( 'grey', "white")
        self.turt.stamp()

    def draw_grid(self):
        ''' draws a grid at x_pos, y_pos with a specific tile_size '''

        # turn off tracer for fast drawing
        self.window.tracer(False)
        
        # move turtle to initial drawing position
        self.turt.up()
        self.turt.goto(self.x_offset, self.y_offset)
        self.turt.down()

        # go over every cell in the grid
        for row in range(len(self.grid)):
            for col in range(len(self.grid[row])):
                
                # move turtle to the position of the cell in the grid
                self.turt.up()
                self.turt.goto(self.x_offset + col * self.tile_size, self.y_offset -row * self.tile_size)
                self.turt.down()

                # if the cell is an obstacle (X) draw a black dot
                self.draw_dot(self.grid[row][col])
            
        
        # turn tracer back on
        self.window.tracer(True)


    def find_start(self):
        ''' finds the start position (S) in the grid
        returns a tuple of start row and col
        '''

        # go over every cell in the grid
        for row in range(len(self.grid)):
            for col in range(len(self.grid[0])):

                # cell at row, col is 'S' return row and col as a tuple
                if self.grid[row][col] == 'S':
                    return (row, col)



    def read_grid(self,file_name):
        ''' reads a maze file and initializes a gird with its contents '''

        # create an empty grid (an empty list called grid)
        self.grid = []

        # open the text file
        file = open(file_name)

        # read a line from the file
        line = file.readline()

        # replace \n with nothing
        line = line.replace('\n', '')

        while line:
            # split the line into tokens
            tokens = line.split(',')

            # add the tokens to the grid as a single row
            self.grid.append(tokens)

            line = file.readline()
            
            # replace \n with nothing
            line = line.replace('\n', '')


    def search_from(self, row, col):
        ''' recursive function to search the grid for the end (E) '''


        self.steps += 1

        # make sure row and col are valid points on the grid
        if row < 0 or col < 0 or row == len(self.grid) or col == len(self.grid[0]):
            # return False if not valid
            return False

        # check that the grid cell at row and col is not obstacle, tried, or deadend
        if self.grid[row][col] in ['X','T','D']: 
            # return False if obstacle, tried, or deadend
            return False

        # If end is found at row, col return True
        if self.grid[row][col] == 'E':
            return True
        
        # If the cell at row, col is not the start cell, mark the cell as tried (T)
        if self.grid[row][col] != 'S':
            self.grid[row][col] = 'T'

        # draw the grid
        self.draw_grid()

        # pause the program for a short duration, try 0.5 and 0.01 seconds
        time.sleep(0.25)

        # recursively search differnt directions adjacent to current row, col (up, down, left, right)
        found = (self.search_from(row-1, col)
                or self.search_from( row+1, col)
                or self.search_from( row, col-1)
                or self.search_from(row, col+1)
                )

        # if any of the 4 directions returns True, mark the cel at row, col as part of the path and return True
        if found and self.grid[row][col] != 'S':
            self.grid[row][col] = 'P'
            return True
        # else, if the cell at row, col is not the start cell (S), mark it as a deadend
        elif self.grid[row][col] != 'S':
            self.grid[row][col] = 'D'
        
        self.draw_grid()
   
   

    def path_length(self):
        path = []
        for row in range(len(self.grid)):
            for col in range(len(self.grid[0])):
                if self.grid[row][col] == 'P':
                    path.append((row, col))
        return(len(path))

def background_music():
    ''' plays tetris music in the background '''
    playsound('Tetris.mp3') 

def main():
    ''' reads a maze file and sets the search parameters '''
    turt = turtle.Turtle()
    window = turtle.getscreen()
    window.bgcolor('slate gray')
    turtle.hideturtle()
    turt.hideturtle()
    turt.shape('square')
    turt.shapesize(2.5, 2.5)

def main():    
    turt = turtle.Turtle()
    window = turtle.getscreen()
    window.bgcolor('slate gray')
    turtle.hideturtle()
    turt.hideturtle()
    turt.shape('square')
    turt.shapesize(2.5, 2.5)
    # read maze file and create playground grid
    grid=Grid(window,turt,50,-150,200)
    grid.read_grid("maze2.txt")

        # find start position
    row, col = grid.find_start()

        # call the search function, it takes the grid, row, column, and steps
    grid.search_from( row, col)

        # print path length
    print('path length:', grid.path_length())
        # draw the final grid
    grid.draw_grid()
        
        # pause the grid drawing for 4 seconds
    time.sleep(4)

        # print the number of steps taken to find the path
    print("number of steps taken to reach answer:", steps)

    # create a turtle and a window for drawing

if __name__ == "__main__":
    p = Process(target=background_music, args=())
    p.start()
    main()
    p.terminate()

