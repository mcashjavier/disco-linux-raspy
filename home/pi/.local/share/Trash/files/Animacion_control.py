import pygame, sys
from pygame.locals import *
import tkinter
import RPi.GPIO as IO
import time
IO.setwarnings(False)
IO.setmode(IO.BCM) #anda con los pines GPIO18
#IO.setmode(IO.BOARD) #anda con los pines
IO.setup(4,IO.IN,pull_up_down=IO.PUD_UP)
x=0
salto=False
    
pygame.init()

FPS = 30 # frames per second setting
fpsClock = pygame.time.Clock()

# set up the window
DISPLAYSURF = pygame.display.set_mode((400, 300), 0, 32)
pygame.display.set_caption('Animation')

WHITE = (255, 255, 255)
catImg = pygame.image.load('cat.png')
catx = 10
caty = 10
direction = 'right'

def interrupcion_flanco_asc(channel):
        global salto
        global catx
        global caty
        global direction
        salto=True
        caty=10
        catx=10
        direction='right'
        print ('canal %s activo GPIO4',channel)
        #print ('Marcos Vetta')
    
#IO.add_event_detect(4,IO.RISING,interrupcion_flanco_asc)
#IO.add_event_detect(4,IO.BOTH,interrupcion_flanco_asc)
IO.add_event_detect(4,IO.FALLING,interrupcion_flanco_asc,bouncetime=1000)

while True: # the main game loop
    DISPLAYSURF.fill(WHITE)
    
#    if salto==True:
#        salto=False
#        catx=10
#        caty+=5
    if direction == 'right':
        catx += 5
        if catx == 280:
            direction = 'down'
    elif direction == 'down':
        caty += 5
        if caty == 220:
            direction = 'left'
    elif direction == 'left':
        catx -= 5
        if catx == 10:
            direction = 'up'
    elif direction == 'up':
        caty -= 5
        if caty == 10:
            direction = 'right'

    DISPLAYSURF.blit(catImg, (catx, caty))

    for event in pygame.event.get():
        if event.type == QUIT:
            pygame.quit()
            sys.exit()

    pygame.display.update()
    fpsClock.tick(FPS)
