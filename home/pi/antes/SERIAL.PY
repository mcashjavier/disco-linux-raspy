#!/usr/bin/env python

import time
import serial

#ser=serial.Serial(port='/dev/ttyAMA0',
ser=serial.Serial(port='/dev/serial0',              
                  baudrate=9600,
                  parity=serial.PARITY_NONE,
                  stopbits=serial.STOPBITS_ONE,
                  bytesize=serial.EIGHTBITS,
                  timeout=0
                  )

try:
    ser.write(bytes(bytearray([0x3C])))
    #    while 1:
    #        x=ser.readline(15)
    #        #if len(x)==15:
    #        print (x)
            
        
finally:
    ser.close()
