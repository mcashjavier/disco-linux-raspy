#Programa para lectora TTL
#DATA IN por el pin 7-->GPIO4
#CLK IN por el pin 13-->GPIO27
#CARD PRESENT IN por el pin 15-->GPI22
import tkinter
import RPi.GPIO as IO

import time


IO.setwarnings(True)
IO.setmode(IO.BCM) #anda con los pines GPIOxx
#IO.setmode(IO.BOARD) #anda con los pines de placa

#IO.setup(4,IO.IN,pull_up_down=IO.PUD_UP)
IO.setup(27,IO.IN,pull_up_down=IO.PUD_UP)
#IO.setup(22,IO.IN,pull_up_down=IO.PUD_UP)
#IO.setup(22,IO.IN)
#IO.setup(27,IO.IN)

Hab_CLK=True
contador=0
x=0

    
#def inte_DATA(channel):
#        print ('canal %s activo GPIO4',channel)
##        print ('Marcos Vetta')
#        os.system('clear')

#def inte_CLK(channel):
##        global Hab_CLK
#        global contador
#        if channel==27:
#            contador=contador+1    
#            print ('CLK ',channel,IO.input(27),contador)
#            #IO.cleanup()
##        print('CONT',contador)
#        os.system('clear')
 #      if channel==4:
 #           print ('DATA %d %d',channel,IO.input(4))
  #      elif channel==22:
  #          print('CARD present %d %d',channel,IO.input(22))
                
         
        
def inte_CARDPRESENT(channel):
#        global Hab_CLK
#        global contador
#        if channel==22:
#            contador=contador+1    
#        Hab_CLK=True
            print ('CARD PRESENTE:',channel,contador)
   
#        else:
#                print('otro pin')   

#IO.add_event_detect(4,IO.RISING,interrupcion_flanco_asc)
#IO.add_event_detect(4,IO.BOTH,interrupcion_flanco_asc)
#IO.add_event_detect(4,IO.FALLING,callback=inte_DATA)
#IO.add_event_detect(27,IO.RISING,callback=inte_CLK)
#IO.add_event_callback(27,inte_CLK)
IO.add_event_detect(27,IO.FALLING,callback=inte_CARDPRESENT,bouncetime=1)
#IO.add_event_detect(27,IO.FALLING,inte_CLK,bouncetime=500)

for x in range (0,500):
    #entrada=IO.input(4)
    #print('.')
    #time.sleep(1)
    if IO.input(27)>0.5:
            print('input=',IO.input(27))
    else:
            print('input=',I0.input(27))
#    time.sleep(0.5)    
