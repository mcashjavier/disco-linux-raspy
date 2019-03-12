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
     e1=e1+1
     #if IO.input(13)==0:
      #   while a==0:
     print(IO.input(27))
