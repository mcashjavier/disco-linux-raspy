
import tkinter
import RPi.GPIO as IO
import time, sys

IO.setmode(IO.BCM)
IO.setup(27,IO.IN)
tiempo1=0
tiempo2=0
unos=0
ceros=0
a=0
x=0
#while a<2:
tiempoini=time.time()
while x < 10:
    while IO.input(27)==1:
            #print('1')
            tiempo1=time.time()
            x=tiempo1-tiempoini
            print('x=',x)
            if x>2:
                print('break')
                break
    unos=unos+1
    while IO.input(27)==0:        
            #print('0')
            tiempo2=time.time()
            x=tiempo2-tiempoini
            if x>2:
                #x=0
                break
    ceros=ceros+1
       

print('unos',unos)
print('ceros',ceros)         
print('tiempo=',(tiempoini-tiempo1))
