from tkinter import *
import serial 
import sys
import numpy as np




serie = serial.Serial('/dev/serial0', 9600, timeout=0, writeTimeout=0)

def sensor_tem():
  
    if serie.getByteSize:

        dato = serie.readline()
        senso = dato#[0:20] #dato[0:1]ttyUSB0
        #sensor.after(20, sensor_tem)
        #s=len(dato)
        #if dato:
            #for x in dato:
                #readingt.set(ord(dato[0:x]))     
        print(dato)
        sensor.after(10, sensor_tem)    

ventana = Tk()
ventana.geometry("600x300+0+0")
ventana.title("Emulador de cajero *Python E.E.C")

tit_sensor = Label(ventana, text = "LECTURA:")
tit_sensor.place(x = 100, y = 70)

readingt = StringVar()

sensor = Label(ventana, textvariable = readingt)
sensor.place(x= 100, y = 100)

def habx():
    serie.write(chr(86))
    
def desx():
    serie.write(chr(85))

def estx():
    serie.write(chr(89))    

btn_estado = Button(ventana, text = "Estado", command = estx)
btn_estado.place(x = 100, y = 130)
btn_bill = Button(ventana, text = "Habilitar billetero", command = habx)
btn_bill.place(x = 100, y = 160)
btn_bill = Button(ventana, text = "Deshabilitar billetero", command = desx)
btn_bill.place(x = 100, y = 190)

sensor.after(10, sensor_tem)
ventana.mainloop()
