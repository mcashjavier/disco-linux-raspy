
import tkinter
import RPi.GPIO as IO
import time, sys

IO.setmode(IO.BCM)
IO.setup(27,IO.IN)
tiempo1=0
tiempo2=0
a=0
while a<2:
    while IO.input(27)==1:
            #print('1')
            tiempo1=time.time()
            a=a+1
    while IO.input(27)==0:        
            #print('0')
            tiempo2=time.time()
            a=a+1
       
         
print('tiempo=',(tiempo2-tiempo1))
