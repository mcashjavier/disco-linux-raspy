
#Entrada por el pin 7-->GPIO4
import tkinter
import RPi.GPIO as IO
import time
IO.setwarnings(False)
IO.setmode(IO.BCM) #anda con los pines GPIO18
#IO.setmode(IO.BOARD) #anda con los pines
IO.setup(4,IO.IN,pull_up_down=IO.PUD_UP)
x=0

while x==0:
    entrada=IO.input(4)
    print('el puerto esta en',entrada)
#    if input()=='e':
#        break

