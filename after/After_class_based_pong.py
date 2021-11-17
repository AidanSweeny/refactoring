'''
name: Naser Al Madi
file: pong.py
data: 9/20/2020
course: CS151 fall
description: simple implementation of the game Pong using python 3 turtles.
'''

import turtle

class Turtle:
    def __init__(self, shape,color,stretch_size,size):
        self.color=color
        self.stretch_size=stretch_size
        self.size=size
        self.shape=shape
        self.turtle = turtle.Turtle()
        self.turtle.speed(0)    # Speed of animation, 0 is max
        self.turtle.shape(self.shape)
        self.turtle.color(self.color)
        self.turtle.shapesize(*self.stretch_size) 
        self.turtle.penup()
        self.turtle.goto(*self.size) # Start position

    def get_size(self):
        return self.size
    
    def get_turtle(self):
        return self.turtle


class Paddle:
    # implements a Pong game paddle

    def __init__(self, turtle):
        ''' initializes a paddle with a position '''
        self.turt = turtle
        size = self.turt.get_size()
        self.x_position = size[0]
        self.y_position = size[1]


# ONE METHOD
    def up(self):
        y = self.turt.get_turtle().ycor() + 20
        self.turt.get_turtle().sety(y)
        self.y_position = y

    def down(self):
        y = self.turt.get_turtle().ycor() - 20#Get the current y coordinate          #add 20px could also be y=y+20
        self.turt.get_turtle().sety(y)    #move the paddle to the new y position
        self.y_position = y


    def get_xcor(self):
        ''' returns turtle x_cord '''
        return self.turt.get_turtle().xcor()

    
    def get_ycor(self):
        ''' returns turtle y_cord '''
        return self.turt.get_turtle().ycor()


class Ball:
    # implements a Pong game ball

    def __init__(self, turtle):
        ''' intializes a ball with default direction and position '''

        self.turt = turtle
        self.ball_speed_x = 0.0925 #speed in x direction
        self.ball_speed_y = 0.0925 #speed in y direction
        self.x_position = 0
        self.y_position = 0


    def move(self):
        ''' moves the ball in x and y directions '''

        # Move the ball
        self.turt.get_turtle().setx(self.turt.get_turtle().xcor() + self.ball_speed_x)
        self.turt.get_turtle().sety(self.turt.get_turtle().ycor() + self.ball_speed_y)

        self.x_position = self.turt.get_turtle().xcor()
        self.y_position = self.turt.get_turtle().ycor()

        # Top and bottom
        if self.turt.get_turtle().ycor() > 290:
            self.turt.get_turtle().sety(290)
            self.ball_speed_y *= -1

        elif self.turt.get_turtle().ycor() < -290:
            self.turt.get_turtle().sety(-290)
            self.ball_speed_y *= -1

    
    def xcor(self):
        ''' returns turtle x_cord '''
        return self.turt.get_turtle().xcor()

    
    def ycor(self):
        ''' returns turtle y_cord '''
        return self.turt.get_turtle().ycor()


    def goto(self, x_pos, y_pos):
        ''' moves ball to new x, y positions '''
        self.turt.get_turtle().goto(x_pos, y_pos)
        self.x_position = x_pos
        self.y_position = y_pos


    def setx(self, x_cor):
        ''' sets the ball x position '''
        self.turt.get_turtle().setx(x_cor)
        self.x_position = x_cor



def make_window(window_title, bgcolor, width, height):
    '''this function creates a screen object and returns it'''

    window = turtle.getscreen() #Set the window size
    window.title(window_title)
    window.bgcolor(bgcolor)
    window.setup(width, height)
    window.tracer(0) #turns off screen updates for the window Speeds up the game
    return window

def checkKeys(window, paddle_1, paddle_2):
    window.listen() #Listen for keyboard input
    window.onkeypress(paddle_1.up, "w") #when you press w run paddle_a_up
    window.onkeypress(paddle_1.down, "s")
    window.onkeypress(paddle_2.up, "Up")
    window.onkeypress(paddle_2.down, "Down")    

def main():
    ''' the main function where the game events take place '''

    window = make_window("Pong - A CS151 Reproduction!", "black", 800, 600)

    # Score
    score_player1 = 0
    score_player2 = 0

    paddle_1 = Paddle(Turtle("square", "white", (5, 1), (-350, 0)))
    paddle_2 = Paddle(Turtle("square", "white", (5, 1), (350, 0)))

    # ball
    ball = Ball(Turtle("square", "white", (1, 1), (0, 0)))

    # Pen
    pen = Turtle("square", "white", (1, 1), (0, 260)).get_turtle()
    pen.write("Player A: 0  Player B: 0", align="center", font=("Courier", 24, "normal"))
    pen.hideturtle()

    checkKeys(window, paddle_1, paddle_2)
    # Main game loop
    while True:
        window.update() #This is the update to offset the wn.tracer(0)

        ball.move()

        # Border checking    
        # Left and right
        if ball.xcor() > 350:
            score_player1 += 1
            pen.clear()
            pen.write("Player A: "+ str(score_player1) + "  Player B: "+ str(score_player2), align="center", font=("Courier", 24, "normal"))
            ball.goto(0, 0)
            ball.ball_speed_x *= -1

        elif ball.xcor() < -350:
            score_player2 += 1
            pen.clear()
            pen.write("Player A: "+ str(score_player1) + "  Player B: "+ str(score_player2), align="center", font=("Courier", 24, "normal"))
            ball.goto(0, 0)
            ball.ball_speed_x *= -1

        # Paddle and ball collisions
        if ball.xcor() < -340 and ball.xcor() > -350 and ball.ycor() < paddle_1.get_ycor() + 50 and ball.ycor() > paddle_1.get_ycor() - 50:
            ball.setx(-340)
            ball.ball_speed_x *= -1.5
        
        elif ball.xcor() > 340 and ball.xcor() < 350 and ball.ycor() < paddle_2.get_ycor() + 50 and ball.ycor() > paddle_2.get_ycor() - 50:
            ball.setx(340)
            ball.ball_speed_x *= -1.5




if __name__ == "__main__":
	main()