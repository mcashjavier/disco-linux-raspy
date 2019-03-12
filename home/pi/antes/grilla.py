#grilla marcos

import random, pygame, sys
from pygame.locals import *

FPS = 3#15
WINDOWWIDTH = 480#640
WINDOWHEIGHT = 320#480
CELLSIZE = 20
assert WINDOWWIDTH % CELLSIZE == 0, "Window width must be a multiple of cell size."
assert WINDOWHEIGHT % CELLSIZE == 0, "Window height must be a multiple of cell size."
CELLWIDTH = int(WINDOWWIDTH / CELLSIZE)
CELLHEIGHT = int(WINDOWHEIGHT / CELLSIZE)

#             R    G    B
WHITE     = (255, 255, 255)
BLACK     = (  0,   0,   0)
RED       = (255,   0,   0)
GREEN     = (  0, 255,   0)
DARKGREEN = (  0, 155,   0)
DARKGRAY  = ( 140, 140,  140)
BLUE      = (0,0,255)
YELLOW    = (0,140,140)
BGCOLOR = BLACK #GREEN  color de fondo de todo

UP = 'up'
DOWN = 'down'
LEFT = 'left'
RIGHT = 'right'

HEAD = 0 # syntactic sugar: index of the worm's head

def main():
    global FPSCLOCK, DISPLAYSURF, BASICFONT

    pygame.init()
    FPSCLOCK = pygame.time.Clock()
    DISPLAYSURF = pygame.display.set_mode((WINDOWWIDTH, WINDOWHEIGHT))
    #BASICFONT = pygame.font.Font('freesansbold.ttf', 20) #letra del score
    pygame.display.set_caption('grilla Marcos')

 
    while True:
        runGame()

def runGame():
     while True: # main game loop
        for event in pygame.event.get(): # event handling loop
            if event.type == UP:
               print ('a')

        drawGrid()
        pygame.display.update()
        FPSCLOCK.tick(FPS)
 
def drawGrid():
    for x in range(0, WINDOWWIDTH, CELLSIZE): # draw vertical lines
        pygame.draw.line(DISPLAYSURF, YELLOW, (x, 0), (x, WINDOWHEIGHT))
    for y in range(0, WINDOWHEIGHT, CELLSIZE): # draw horizontal lines
        pygame.draw.line(DISPLAYSURF, YELLOW, (0, y), (WINDOWWIDTH, y))


if __name__ == '__main__':
    main()
