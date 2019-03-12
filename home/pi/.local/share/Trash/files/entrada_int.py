
#Entrada por el pin 7-->GPIO4
import tkinter
import RPi.GPIO as IO
import time
IO.setwarnings(False)
IO.setmode(IO.BCM) #anda con los pines GPIO18
#IO.setmode(IO.BOARD) #anda con los pines
IO.setup(4,IO.IN,pull_up_down=IO.PUD_UP)
x=0

    
def interrupcion_flanco_asc(channel):
        print ('canal %s activo GPIO4',channel)
        print ('Marcos Vetta')
    
#IO.add_event_detect(4,IO.RISING,interrupcion_flanco_asc)
#IO.add_event_detect(4,IO.BOTH,interrupcion_flanco_asc)
IO.add_event_detect(4,IO.FALLING,interrupcion_flanco_asc,bouncetime=500)

while x==0:
    #entrada=IO.input(4)
    print('el puerto esta activo ----')
    time.sleep(1)
