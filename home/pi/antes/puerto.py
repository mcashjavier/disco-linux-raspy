
#!/usr/bin/env python
#puerto.py

#import time
#import pigpio
#pi=pigpio.pi()
#pi.set_mode (18,pigpio.OUTPUT) #pin 18 como salida
#pi.set_servo_pulsewidth (18,1300)
#pi.stop()
import tkinter
import RPi.GPIO as IO
import time
IO.setwarnings(False)
IO.setmode(IO.BCM) #anda con los pines GPIO18
#IO.setmode(IO.BOARD) #anda con los pines
IO.setup(18,IO.OUT)
x=0
while x==0:
    entrada=input("ingrese valor del pin:")
    if entrada=='0':
        IO.output(18,False)
        print('el puerto esta en',False)
    if entrada=='1':
        IO.output(18,True)
        print('el puerto esta en',True)
    if entrada=='e':
        print('Sale',True)
        break

