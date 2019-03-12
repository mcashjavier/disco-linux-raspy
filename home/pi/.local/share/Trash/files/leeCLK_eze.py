from datetime import datetime,timedelta,date,time
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

e0=1
e1=0
clock0=0
clock0final=0
clock1=0
clock1final=0



while a==0:
    inicio=time.time()
    
    if IO.input(27)==0:
        
        while a==0:
            final=time.time()
            if IO.input(27)==1:
                clock0final=time.time()
                x=(clock0final-clock0)
                e0=0
                if e1==0:
                    e1=1        
                    unos=unos+1
                    clock1=time.time()
            if IO.input(27)==0:
                clock1final=time.time()
                y=(clock1final-clock1)
                e1=0
                if e0==0:
                    e0=1    
                    ceros=ceros+1
                    clock0=time.time()
                  
            if (final-inicio) >=1:
                print(unos)
                print(ceros)
                print(x)
                print(y)
                #print(clock0final-clock0)
                #print(clock1final-clock1)
                unos=0
                ceros=0
                break

    #£if tiempofinal < ahora:
     #       print(unos)+print(ceros)
     #       break

