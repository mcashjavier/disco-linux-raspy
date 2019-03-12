import threading
from tkinter import *
from tkinter.font import Font
import socket
import sys
import os
import json
import serial
import binascii
import pygame
import webbrowser

from datetime import datetime,timedelta
import time

global rojo_dos_mundos

global infoObject
global screen
global acumulador
global SinNada
global EntrarTimer
global qcolor
global TimeOut
global llego1
global EnvioPuerto

global confirmacion
global ctk
global tarjeta
global PagandoTk
global HTimeOut

global DosMundosc

global lcolor1
global lcolor2
global lcolor3
global xa
global jluces
global apb


global ColorVerde
global ColorRojo

apb=0
jluces=0
xa=0
pygame.mixer.init()
SinNada=True
HTimeOut=datetime.now()
acumulador=''
qcolor=0
PagandoTk=False
TimeOut=False
tarjeta=''
DosMundosc=0

#------Luces 2 mundos Def------
M1=[0x2A,0X49,0x24,0X97,0x49,0x24,0x97]
M0=[0x26]
M2=[0x2A,0x49,0x24,0x92,0xE9,0x24,0x97]
M3=[0x2A,0x49,0x24,0x92,0x5D,0x24,0x97]
M4=[0x2A,0x49,0x24,0x92,0x4B,0xA4,0x97]
M5=[0x2A,0x49,0x24,0x92,0x49,0x74,0x97]
M6=[0x2A,0x49,0x24,0x92,0x49,0x2E,0x97]
M7=[0x2A,0x49,0x24,0x92,0x49,0x25,0xD7]
M8=[0x2A,0x49,0x24,0x92,0x49,0x24,0xBF]
M9=[0x2A,0x49,0x24,0x92,0x49,0x24,0x97]
M10=[0x2A,0x49,0x24,0x97,0x49,0x24,0x92]
M11=[0x2A,0x00,0x00,0x00,0x00,0x00,0x07]
rojo_dos_mundos=bytearray(M1)
rojo_dos_mundos1=bytearray(M2)
rojo_dos_mundos2=bytearray(M3)
rojo_dos_mundos3=bytearray(M4)
rojo_dos_mundos4=bytearray(M5)
rojo_dos_mundos5=bytearray(M6)
rojo_dos_mundos6=bytearray(M7)
rojo_dos_mundos7=bytearray(M8)
rojo_dos_mundos8=bytearray(M9)
rojo_dos_mundos9=bytearray(M10)
blanco_giro=bytearray(M11)
#------------------------------

def DosMundos():
    global confirmacion
    global DosMundosc
    if len(tarjeta)==0:    
        if DosMundosc==0 and confirmacion==True:
            serie.write(bytes(rojo_dos_mundos))
            DosMundosc=1
            #confirmacion=False
        if DosMundosc==1 and confirmacion==True:
            serie.write(bytes(rojo_dos_mundos1))
            DosMundosc=2
            #confirmacion=False
        if DosMundosc==2 and confirmacion==True:
            serie.write(bytes(rojo_dos_mundos2))
            DosMundosc=3
            #confirmacion=False
        if DosMundosc==3 and confirmacion==True:
            serie.write(bytes(rojo_dos_mundos3))
            DosMundosc=4
            #confirmacion=False
        if DosMundosc==4:
            serie.write(bytes(rojo_dos_mundos4))
            DosMundosc=5
            #confirmacion=False
        if DosMundosc==5 and confirmacion==True:
            serie.write(bytes(rojo_dos_mundos5))
            DosMundosc=6
            #confirmacion=False
        if DosMundosc==6 and confirmacion==True:
            serie.write(bytes(rojo_dos_mundos6))
            DosMundosc=7
            #confirmacion=False
        if DosMundosc==7 and confirmacion==True:
            serie.write(bytes(rojo_dos_mundos7))
            DosMundosc=8
            #confirmacion=False
        if DosMundosc==8 and confirmacion==True:
            serie.write(bytes(rojo_dos_mundos8))
            DosMundosc=9
            #confirmacion=False
        if DosMundosc==9 and confirmacion==True:
            serie.write(bytes(rojo_dos_mundos9))
            DosMundosc=10
            #confirmacion=False
        if DosMundosc==10 and confirmacion==True:
            serie.write(bytes(blanco_giro))
            DosMundosc=0
            #confirmacion=False
    

def inicio():
    global confirmacion
    global ctk
    global tarjeta
    global PagandoTk
    global TimeOut
    global EnvioPuerto
    global punto
    global ColorVerde
    global ColorRojo
    
    TimeOut=False
    PagandoT=False
    tarjeta=''
    ctk=0
    xa=0
    punto=0
    confirmacion=True
    global llego1
    llego1=''
    EnvioPuerto=''
    pygame.init()
    pygame.font.init()
    ColorVerde=bytearray()#[0x25,0x92,0x49,0x24,0x92,0x49,0x27])#bytes(a)
    ColorVerde.append(0x25)
    ColorVerde.append(0x92)
    ColorVerde.append(0x49)
    ColorVerde.append(0x24)
    ColorVerde.append(0x92)
    ColorVerde.append(0x49)
    ColorVerde.append(0x27)
    ColorRojo=bytearray()#([0x25,0x49,0x24,0x92,0x49,0x24,0x97])#bytes(a)
    ColorRojo.append(0x25)
    ColorRojo.append(0x49)
    ColorRojo.append(0x24)
    ColorRojo.append(0x92)
    ColorRojo.append(0x49)
    ColorRojo.append(0x24)
    ColorRojo.append(0x97)

inicio()




def TiempoTk():
    global ctk
    global TimeOut
    global EntrarTimer
    #global HTimeOut
    global Ahora
    global EnvioPuerto
    EntrarTimer=False
    TimeOut=False
    
    while True:
        
        #if int(ctk)>0:
        ##print(PagandoTk)
        ##print(ctk)
        ##print(SinNada)

        

        #Ahora=datetime.now()
        if PagandoTk==False and int(ctk)>0:
            Ahora=datetime.now()
               
            #if TimeOut==False:
            #    TimeOut=False
            
            if Ahora >= HTimeOut and TimeOut==False:
                
                if PagandoTk==False and int(ctk)>0:
                    
                    TimeOut=True
                    ###print('Termino tiempo de Tk')
                    ###print('Envia Tks')
                    ###print(ctk)
                    #if confirmacion==True:
                    #    serie.write(activot)
                    #while len(EnvioPuerto)>0:
                    #    a=1
                    #while EnvioPuerto!='':
                    #    b=1
                    #EnvioPuerto=activot
        #if Ahora < HTimeOut:
        #        Timeout=False
        

def resize_image(event):
    new_width = event.width
    new_height = event.height
    image = copy_of_image.resize((new_width, new_height))
    photo = ImageTk.PhotoImage(image)
    label.config(image = photo)
    label.image = photo #avoid garbage collection


def cargo():
    #global confirmacion
    global termineConsulta
    global estatarjeta
    
    termineConsulta=False
    estatarjeta=False
    confirmacion=True

    #[0x29,0x10]->pago lento
    #[0x29,0x22]->pago rapido
    pcorto=bytearray()#[0x29,0x10])#,0x00,0x00,0x00,0x00,0x00])#pago rapido-coin rapido
    pcorto.append(0x29)
    pcorto.append(0x10)
    serie.write(pcorto)
    serie.flushOutput()
    time.sleep(1)
    pcorto1=bytearray()#[0x30,0x01,0x35])#,0x00,0x00,0x00,0x00]) #habilita pago
    pcorto1.append(0x30)
    pcorto1.append(0x01)
    pcorto1.append(0x35)
 #   pcorto.append(0x10)
    serie.write(pcorto1)
    serie.flushOutput()
    time.sleep(1)
    serie.write(activot)
    serie.flushOutput()
    time.sleep(1)
    
    

serie = serial.Serial('/dev/serial0', 9600, timeout=0.2, writeTimeout=0)
#seriet = serial.Serial('/dev/serial0', 9600, timeout=0.2, writeTimeout=0)

rojo=bytearray()
rojo.append(0x24)
rojo.append(0x92)
rojo.append(0x49)
rojo.append(0x24)
rojo.append(0x92)
rojo.append(0x49)
rojo.append(0x27)

rojo1=bytearray()
rojo1.append(0x24)
rojo1.append(0x49)
rojo1.append(0x24)
rojo1.append(0x92)
rojo1.append(0x49)
rojo1.append(0x27)
rojo1.append(0x92)

rojo2=bytearray()
rojo2.append(0x24)
rojo2.append(0x24)
rojo2.append(0x92)
rojo2.append(0x49)
rojo2.append(0x27)
rojo2.append(0x92)
rojo2.append(0x49)

activot=bytearray()#([0x26])#,0x00,0x00,0x00,0x00,0x00,0x00])
activot.append(0x26)
#activot.append(0x00)
#activot.append(0x00)
#activot.append(0x00)

desactivot=bytearray()#([0x27])#,0x00,0x00,0x00,0x00,0x00,0x00])
desactivot.append(0x27)
#activot.append(0x00)
#activot.append(0x00)
#activot.append(0x00)


verde=bytearray()
verde.append(0x24)
verde.append(0x80)
verde.append(0x00)
verde.append(0x00)
verde.append(0x80)
verde.append(0x00)
verde.append(0x00)

verde1=bytearray()
verde1.append(0x24)
verde1.append(0x10)
verde1.append(0x00)
verde1.append(0x00)
verde1.append(0x00)
verde1.append(0x00)
verde1.append(0x00)

verde2=bytearray()
verde2.append(0x24)
verde2.append(0x02)
verde2.append(0x00)
verde2.append(0x00)
verde2.append(0x00)
verde2.append(0x00)
verde2.append(0x00)

verde3=bytearray()
verde3.append(0x24)
verde3.append(0x00)
verde3.append(0x40)
verde3.append(0x00)
verde3.append(0x00)
verde3.append(0x00)
verde3.append(0x00)

verde4=bytearray()
verde4.append(0x24)
verde4.append(0x00)
verde4.append(0x08)
verde4.append(0x00)
verde4.append(0x00)
verde4.append(0x00)
verde4.append(0x00)

verde5=bytearray()
verde5.append(0x24)
verde5.append(0x00)
verde5.append(0x01)
verde5.append(0x00)
verde5.append(0x00)
verde5.append(0x00)
verde5.append(0x00)

verde5=bytearray()
verde5.append(0x24)
verde5.append(0x00)
verde5.append(0x01)
verde5.append(0x00)
verde5.append(0x00)
verde5.append(0x00)
verde5.append(0x00)

verde6=bytearray()
verde6.append(0x24)
verde6.append(0x00)
verde6.append(0x00)
verde6.append(0x20)
verde6.append(0x00)
verde6.append(0x00)
verde6.append(0x00)

verde7=bytearray()
verde7.append(0x24)
verde7.append(0x00)
verde7.append(0x00)
verde7.append(0x04)
verde7.append(0x00)
verde7.append(0x00)
verde7.append(0x00)

verde8=bytearray()
verde8.append(0x24)
verde8.append(0x00)
verde8.append(0x00)
verde8.append(0x00)
verde8.append(0x80)
verde8.append(0x00)
verde8.append(0x00)

verde9=bytearray()
verde9.append(0x24)
verde9.append(0x00)
verde9.append(0x00)
verde9.append(0x00)
verde9.append(0x10)
verde9.append(0x00)
verde9.append(0x00)

verde10=bytearray()
verde10.append(0x24)
verde10.append(0x00)
verde10.append(0x00)
verde10.append(0x00)
verde10.append(0x02)
verde10.append(0x00)
verde10.append(0x00)

verde11=bytearray()
verde11.append(0x24)
verde11.append(0x00)
verde11.append(0x40)
verde11.append(0x00)
verde11.append(0x00)
verde11.append(0x40)
verde11.append(0x00)

ee=bytearray()
ee.append(0x24)
ee.append(0x00)
ee.append(0x00)
ee.append(0x00)
ee.append(0x00)
ee.append(0x00)
ee.append(0x00)

serie.write(ee)
time.sleep(1)
#serie.write(activot)
#time.sleep(.300)





cargo()


def tipo4():
    if True:
        serie.write(bytearray([0x24,0x01,0x00,0x00,0x00,0x00,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x10,0x00,0x00,0x00,0x00,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x11,0x00,0x00,0x00,0x00,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x01,0x00,0x00,0x00,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x10,0x00,0x00,0x00,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x11,0x00,0x00,0x00,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x00,0x01,0x00,0x00,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x00,0x10,0x00,0x00,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x00,0x11,0x00,0x00,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x00,0x00,0x01,0x00,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x00,0x00,0x10,0x00,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x00,0x00,0x11,0x00,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x00,0x00,0x00,0x01,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x00,0x00,0x00,0x10,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x00,0x00,0x00,0x11,0x00]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x00,0x00,0x00,0x00,0x01]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x00,0x00,0x00,0x00,0x10]))
        time.sleep(00.080)
        serie.write(bytearray([0x24,0x00,0x00,0x00,0x00,0x00,0x11]))
        time.sleep(00.080)

def tipo3():
    if True:
        serie.write(bytearray([0x24,0x01,0x00,0x10,0x00,0x00,0x01]))
        time.sleep(00.100)
        serie.write(bytearray([0x24,0x00,0x10,0x00,0x01,0x00,0x10]))
        time.sleep(00.100)
        serie.write(bytearray([0x24,0x01,0x00,0x10,0x00,0x00,0x01]))
        time.sleep(00.100)
        serie.write(bytearray([0x24,0x00,0x10,0x00,0x01,0x00,0x10]))
        time.sleep(00.100)
        serie.write(bytearray([0x24,0x01,0x00,0x10,0x00,0x00,0x01]))
        time.sleep(00.100)
        serie.write(bytearray([0x24,0x00,0x10,0x00,0x01,0x00,0x10]))
        time.sleep(00.100)
        serie.write(bytearray([0x24,0x01,0x00,0x10,0x00,0x00,0x01]))
        time.sleep(00.100)
        serie.write(bytearray([0x24,0x00,0x10,0x00,0x01,0x00,0x10]))
        time.sleep(00.100)
        
        
        

def tipo2():
    if True:
        serie.write(verde)
        time.sleep(00.070)
        serie.write(verde11)
        time.sleep(00.070)
        #serie.write(verde)
        #time.sleep(00.070)
        #serie.write(verde11)
        #time.sleep(00.070)
        #serie.write(verde)
        #time.sleep(00.070)
        #serie.write(verde11)
        #time.sleep(00.070)
        #serie.write(verde)
        #time.sleep(00.070)
        #serie.write(verde11)
        #time.sleep(00.070)
def tipo1():
    if True:

        ##confirmacion=False
        serie.write(verde)
        #while confirmacion==False:
        #obtengoOk()
        ###print(confirmacion)
        time.sleep(00.070)
        serie.write(verde1)
        time.sleep(00.070)
        serie.write(verde2)
        time.sleep(00.070)
        serie.write(verde3)
        time.sleep(00.070)
        serie.write(verde4)
        time.sleep(00.070)
        serie.write(verde5)
        time.sleep(00.070)
        serie.write(verde6)
        time.sleep(00.070)
        serie.write(verde7)
        time.sleep(00.070)
        serie.write(verde8)
        time.sleep(00.070)
        serie.write(verde9)
        time.sleep(00.070)
        serie.write(verde10)
        time.sleep(00.070)
        serie.write(verde11)
        time.sleep(00.070)
        serie.write(verde11)
        time.sleep(00.070)
        serie.write(verde10)
        time.sleep(00.070)
        serie.write(verde9)
        time.sleep(00.070)
        serie.write(verde8)
        time.sleep(00.070)
        serie.write(verde7)
        time.sleep(00.070)
        serie.write(verde6)
        time.sleep(00.070)
        serie.write(verde5)
        time.sleep(00.070)
        serie.write(verde4)
        time.sleep(00.070)
        serie.write(verde3)
        time.sleep(00.070)
        serie.write(verde2)
        time.sleep(00.070)
        serie.write(verde1)
        time.sleep(00.070)
        serie.write(verde)
        time.sleep(00.070)    
        dato = serie.readline(15)
        #if dato.decode('utf-8')=='U':
        #    confirmacion=True
        #if len(dato.decode('utf-8'))>14: 
            ##print(dato.decode('utf-8'))


def GiroIndividual(aa,bb,cc,direccion):
    global confirmacion
    global ctk
    global tarjeta
    global lcolor1
    global lcolor2
    global lcolor3
    
    
    a=""
    if True:    
        a24='00100100'
        #color='000000000000000000000000000000000000000000000000'
        xx=0
        
        if xa==0:
            lcolor1=aa
            lcolor2=bb
            lcolor3=cc
        else:
            lcolor1=lcolor1+3
            lcolor2=lcolor2+3
            lcolor3=lcolor3+3
            
        color=''
        
        ##confirmacion=False
        if xa<16:
            ###print(xa)
            
            while xx< 48:

                if xx==(lcolor1)-1:
                    if direccion=='d': 
                        color=color+'1'
                    else:
                        color='1'+color
                else:
                    if direccion=='d': 
                        color=color+'0'
                    else:
                        color='0'+color
                if xx==(lcolor2)-1:
                    if direccion=='d': 
                        color=color+'1'
                    else:
                        color='1'+color
                else:
                    if direccion=='d': 
                        color=color+'0'
                    else:
                        color='0'+color
                if xx==(lcolor3)-1:
                    if direccion=='d': 
                        color=color+'1'
                    else:
                        color='1'+color
                else:
                    if direccion=='d': 
                        color=color+'0'
                    else:
                        color='0'+color
                xx=xx+3
                
                #color=color+lcolor1+lcolor2+lcolor3
            ##print(color)    
            envio=a24+color
            a=hex(int(envio,2))
            a=bytearray.fromhex(a[2:16])
            ###print(a)
            if confirmacion==True:
                #confirmacion=False
                #serie.flushInput()
            
                
                serie.write(bytes(a))
            #else:
                #break

            #if lenn(tarjeta)>0:
            #    time.sleep(00.100)
            #    return
            #while confirmacion==False:
                
           #     
             #   color=''
                #llego=serie.read(1).decode('utf-8')
                #serie.flushInput()
                ###print(llego)    
                #if llego=='U':
                        #confirmacion
                #        llego=''
                #        confirmacion=True
                #if len(llego)>14:
                    #reciboTarjeta
                #    p=llego.find(';',0)
                #    h=15+p
                #    if p>=0:
                #        ctk=0
                #        ##print(llego[p:h])        
                #    confirmacion=True
                #    llego=''
                #if llego=='@':   
                #    llego=''
                #    ctk=ctk+1
                #    ##print(ctk)
                #    confirmacion=True
                #if len(llego)>0 and confirmacion==False:
                    #llego=serie.readline(15).decode('utf-8')
                    #serie.flushInput()
                #    #confirmacion=False
                #    tarjeta=tarjeta+llego
                #    llego=''
                 #   ##print(tarjeta)
                 #   if len(tarjeta)>14:
                 #       ##print(tarjeta)
                        #tarjeta=''
                #        confirmacion=True
              
            
            #time.sleep(00.060)
                
            #color=''
            #xx=0
            #lcolor1=lcolor1+3
            #lcolor2=lcolor2+3
            #lcolor3=lcolor3+3
            ###print(lcolor1)
            
            confirmacion=True
            ##print('entre')
            #GiroIndividual(aa,bb,cc,direccion)



#---------------------------------------

#---------------------------------------
def GiroEspecial(cc,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,d):
    global confirmacion

    if True:
        if cc=='24':
            a24='00100100'
        if cc=='25':
            a24='00100101'
        if cc=='28':
            a24='00101000' 
        #color='000000000000000000000000000000000000000000000000'
        xx=0
        #If d=='d':
        color=c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16
        #else:
        #    color=c16+c15+c14+c13+c12+c11+c10+c9+c8+c7+c6+c5+c4+c3+c2+c1
        

            ##print(color)    
        envio=a24+color
        a=hex(int(envio,2))
        a=bytearray.fromhex(a[2:16])
            ###print(a)
        if confirmacion==True:
            #confirmacion=False
            serie.write(bytes(a))

            #confirmacion=True


#----------------------------------------
def GiroPared(aa,bb,cc,direccion,cual):
    global confirmacion
    global ctk
    global tarjeta
    global lcolor1
    global lcolor2
    global lcolor3

    
    color=''
    a=""
    if True:    
        a24='00100100'
        #color='000000000000000000000000000000000000000000000000'

        xx=0

        if cual==1:
            cual=12
        if cual==2:
            cual=24
        if cual==3:
            cual=36
        if cual==4:
            cual=48
        
        #if xa==0:
        lcolor1=aa
        lcolor2=bb
        lcolor3=cc
        #else:
        #    lcolor1=lcolor1+3
        #    lcolor2=lcolor2+3
        #    lcolor3=lcolor3+3
            
        color=''
        
        ##confirmacion=False
        if True:
            ###print(xa)
            
            while xx< cual:

                if xx==(lcolor1)-1:
                    color=color+'1'
                else:
                    color=color+'0'
                if xx==(lcolor2)-1:
                    color=color+'1'
                else:
                    color=color+'0'
                if xx==(lcolor3)-1:
                    color=color+'1'
                else:
                    color=color+'0'
                    
                
                xx=xx+3
                lcolor1=lcolor1+3
                lcolor2=lcolor2+3
                lcolor3=lcolor3+3
                
        if cual==12:
            e=0
            while e<36:
                color=color+'0'
                e=e+1
        if cual==24:
            e=0
            while e<24:
                color=color+'0'
                e=e+1
        if cual==36:
            e=0
            while e<12:
                color=color+'0'
                e=e+1
                
        ##print(color)
        envio=a24+color
        a=hex(int(envio,2))
        a=bytearray.fromhex(a[2:16])

        if confirmacion==True:
            #confirmacion=False
            serie.write(bytes(a))

            confirmacion=True

#----------------------------------------
def ColorFijo(b,r,g):
    #global confirmacion
    global ctk
    global tarjeta
    global EnvioPuerto

    a=""
    if True:    
        a24='00100100' #termina en 00=24 / 01=25
        color='000000000000000000000000000000000000000000000000'
        xx=0
        
        
        lcolor1=b
        lcolor2=r
        lcolor3=g
        color=''
        xa=0
        while xx<48:
            ###print(xa)
            
            if xx==(lcolor1)-1:                
                    color=color+'1'
            else:
                    color=color+'0'
            if xx==(lcolor2)-1:
                color=color+'1'
            else:
                    color=color+'0'
            if xx==(lcolor3)-1:
                color=color+'1'
            else:
                color=color+'0'
            xx=xx+3
            lcolor1=lcolor1+3
            lcolor2=lcolor2+3
            lcolor3=lcolor3+3
        envio=''
        a=''
        envio=str(a24) + str(color)
        #print(len(color))
        #print (len(a24))
        #print(len(envio))

        #Anda 1
        a=hex(int(envio,2))
        a=bytearray.fromhex(a[2:16])

        #Anda 2
        #a=bytearray([int(envio[i:i+8],2)for i in range(0,len(envio),8)])

        #a=int(envio,2).to_bytes(8,'big')
                                 
        
        
        #print(a)
        #if confirmacion==True:
        #if PagandoTk==False:
        ##confirmacion=False
          #serie.flushInput()
        #serie.write(bytes(a))
        #while len(EnvioPuerto)>0:
        #    a=1
        
        
        while EnvioPuerto!='':
            b=1
        #time.sleep(.050)    
        #print("envia luz colorfijo")
            
        EnvioPuerto=a#bytearray([0x28,0x10,0x10,0x10,0x10,0x10,0x01])#bytes(a)
        while EnvioPuerto!='':
                    b=1
        
            

def luz2017(cual):
    global EnvioPuerto
    global punto
    global apb
    if apb=='':
        apb=0
    if cual=='paseo2':
        while EnvioPuerto!='' :
            b=1
        if punto==0: #and tarjeta=='':
            ##print("yo1")
            #EnvioPuerto=bytearray([0x25,0x11,0x11,0x11,0x11,0x11,0x11])#bytes(a)
            EnvioPuerto=ColorRojo#ColorRojo#bytearray([0x25,0xFF,0xFF,0xFF,0xFF,0xFF,0xF8])#bytes(a)
    if cual=='paseo':
        while EnvioPuerto!='':
            b=1
        if punto==0:# and tarjeta=='':
            ##print("yo2")
            EnvioPuerto=bytearray([0x25,0xFF,0xFF,0xFF,0xFF,0xFF,0xF8])#bytearray([0x28,0x10,0x11,0x01,0x02,0x05,0x09])#bytes(a)
        
    if cual=='verde':
        while EnvioPuerto!='':
            b=1
        #punto=0    
        EnvioPuerto=ColorVerde
    if cual=='pagoblanco':
        while EnvioPuerto!='':
            b=1
        #punto=0    
        EnvioPuerto=bytearray([0x24,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF])
    if cual=='pagando':
        while EnvioPuerto!='':
            b=1
        #punto=0    
        EnvioPuerto=bytearray([0x25,0x92,0x49,0x24,0x92,0x49,0x27])
    if cual=='terminopago':
        while EnvioPuerto!='':
            b=1
       # punto=0    
        EnvioPuerto=ColorVerde   
        
        
        #while EnvioPuerto!='':
        #    b=1
    if cual=='rojo':
        while EnvioPuerto!='':
            b=1
        #punto=0
        EnvioPuerto=ColorRojo
        
        #while EnvioPue
    if cual=='apagado':
        while EnvioPuerto!='':
            b=1
        EnvioPuerto=bytearray([0x24,0x00,0x00,0x00,0x00,0x00,0x00])#bytes(a)
        #while EnvioPuerto!='':
        #    b=1

 
        
                    
def colorfijo2(p,c1,c2,c3,c4):
    global EnvioPuerto
    #global confirmacion
    global ctk
    global tarjeta

    #a=""
    if True:
        if p=='24':
            a24='00100100'
        if p=='25':
            a24='00100101'
        if p=='28':
            a24='00101000'   
        #a24='00101000' #termina en 00=24 / 01=25
        color='000000000000000000000000000000000000000000000000'
        xx=0
        
        
        
        color=''
        xa=0
        while xx<48:
            ###print(xa)
            if xx<12:
                color=color+c1
            if xx<24:
                color=color+c2
            if xx<36:
                color=color+c3
            if xx<48:
                color=color+c4

            xx=xx+3
            
        envio=a24+color
        a=hex(int(envio,2))
        
        a=bytearray.fromhex(a[2:16])
            ###print(a)
        #if confirmacion==True:
        #    #confirmacion=False
            #serie.flushInput()
        #    serie.write(bytes(a))
        #while len(EnvioPuerto)>0:
        #    a=1
        #EnvioPuerto=bytes(a)
        while EnvioPuerto!='':
            b=1
        #time.sleep(.050)    
        #print("envia luz #colorfijo2")
        if punto==0:   
            EnvioPuerto=a#bytes(a)

def mprueba():
    global xa
    global jluces
    #xa=0
    if True:
        ##print(xa)
        if confirmacion==True and PagandoTk==False and len(tarjeta)==0:
            if jluces==0:
                #GiroIndividual(1,1,1,'d')
                #GiroPared(0,1,1,'d',1)
                GiroEspecial('25',
                             '001',
                             '000',
                             '000',
                             '000',
                             '000',
                             '010',
                             '000',
                             '000',
                             '000',
                             '100',
                             '000',
                             '000',
                             '000',
                             '000',
                             '000',
                             '000','d')
                xa=xa+1
                if xa>=1:
                    xa=0
                    jluces=jluces+1
                   
            if confirmacion==True and PagandoTk==False and len(tarjeta)==0:
                if jluces==1:
                    #GiroEspecial('25',
                    #             '011',
                    #             '011',
                    #             '011',
                    #             '011',
                    #             '011',
                    #             '011',
                    #             '011',
                    #             '011',
                    #             '011',
                    #             '010',
                    #             '010',
                    #             '100',
                    #             '100',
                    #             '001',
                    #             '001',
                    #             '011','d')
                    
                    GiroPared(1,0,1,'i',4)
                    
                    xa=xa+1
                    if xa>=8:
                        xa=0
                        jluces=jluces+1
            if confirmacion==True and PagandoTk==False and len(tarjeta)==0:
                if jluces==2:
                    GiroEspecial('25',
                                 '001',
                                 '000',
                                 '000',
                                 '000',
                                 '000',
                                 '010',
                                 '000',
                                 '000',
                                 '000',
                                 '000',
                                 '100',
                                 '000',
                                 '000',
                                 '000',
                                 '000',
                                 '000','d')
                    xa=xa+1
                    if xa>=1:
                        xa=0
                        jluces=jluces+1
            if confirmacion==True and PagandoTk==False and len(tarjeta)==0:
                if jluces==3:
                    GiroIndividual(0,0,1,'d')
                    
                    xa=xa+1
                    if xa>=1:
                        xa=0
                        jluces=jluces+1
            if confirmacion==True and PagandoTk==False:
                if jluces==5:
                    GiroEspecial('25',
                                 '010',
                                 '010',
                                 '000',
                                 '000',
                                 '100',
                                 '100',
                                 '000',
                                 '000',
                                 '010',
                                 '010',
                                 '000',
                                 '000',
                                 '000',
                                 '000',
                                 '000',
                                 '000','d')
                    xa=xa+1
                    if xa>=1:
                        xa=0
                        jluces=jluces+1    
            if confirmacion==True and PagandoTk==False and len(tarjeta)==0:
                if jluces==4:
                    #GiroIndividual(1,0,0,'i')
                    #colorfijo2('28','000','000','000','001')
                    xa=xa+1
                    if xa>=1:
                        xa=0
                        jluces=jluces+1
            if confirmacion==True and PagandoTk==False and len(tarjeta)==0:
                if jluces==6:
                    DosMundos()
                    xa=xa+1
                    if xa>=10:
                        xa=0
                        jluces=0
                    

def EscuchoPuerto():
    global acumulador
    global PagandoTk
    global ctk
    global TimeOut
    global confirmacion
    global EntrarTimer
    global HTimeOut
    global SinNada
    global apb
    global tarjeta
    global EnvioPuerto
    global punto
    global recibi
    global luz
    #global screen
    tarjeta=''
    EntrarTimer=False
   
    
    apb=0
    acumulador=''
    recibi=''
    luz='no'
    while True:
        a=serie.read(1).decode('utf-8')
            
        if len(a)>0:
            ##print(a)
            recibi=recibi+a
            acumulador=acumulador+a
        if a=='U':
            confirmacion=True
        if  recibi.find('U',0)>-1:
            ##print("confirmo")
            confirmacion=True
            #EnvioPuerto=''
        if a==";" and tarjeta=='':
            if punto==0:
                #while EnvioPuerto!='':
                #    b=1
                EnvioPuerto=desactivot
                #while EnvioPuerto!='':
                #    b=1
            punto=1
            
        ar=recibi.find('@',0)
        if ar>-1:
             if PagandoTk==False:
                 #if confirmacion==True:
                 
             #    EnvioPuerto=desactivot
                 #serie.write(desactivot)
                 EntrarTimer=False
             PagandoTk=True
             TimeOut=False
             esto=recibi
             x=0
             while x< len(esto):
                 esta=esto.find("@",x,x+1)
                 if esta>-1:
                     ctk=ctk+1
                 x=x+1
             
             HTimeOut=datetime.now()+timedelta(seconds=4)
             #colorfijo2('24','100','000','100','000')   
             
             
                 
             
             #HTimeOut=datetime.now()+timedelta(seconds=3)
             
            #noviembre2017
             recibi=''
             acumulador=''
             HTimeOut=datetime.now()+timedelta(seconds=3)
             #if confirmacion==True:
             #    colorfijo2('25','100','000','100','000')
             
             PagandoTk=False
             ##print(ctk)
        recibi=''
        
def PuertoSerie():
    global acumulador
    global PagandoTk
    global ctk
    global TimeOut
    global confirmacion
    global EntrarTimer
    global HTimeOut
    global SinNada
    global apb
    global tarjeta
    global EnvioPuerto
    #global screen
    tarjeta=''
    EntrarTimer=False
    apb=0
    
    
    #acumulador=''
    while True:
        #a=serie.read(1).decode('utf-8')
        ##print(a)    
        #if len(a)>0:
        a=''        
        #    acumulador=acumulador+a
        #if acumulador=='U':
            ##print(acumulador)
        #    confirmacion=True
            #EnvioPuerto=''
            #EnvioPuerto=''
       #     acumulador=''#noviembre2017
       #     if PagandoTk==False:
                #a=''
       # if acumulador.find('U',0)>-1 :
            ##print(acumulador)
            #EnvioPuerto=''
       #     confirmacion=True
            #EnvioPuerto=''
      #      if PagandoTk==False:
       #         a=''    
            #acumulador=''
            #a=''#noviembre2017
        
        
        
       # if acumulador=='U':
      #      confirmacion=True
            #EnvioPuerto=''
        #    acumulador=''#noviembre2017
            #if PagandoTk==False:
        #    a=''
       # if acumulador.find('U',0)>-1:
        #    confirmacion=True
            #EnvioPuerto=''
            #acumulador=''
            #a=''
             
        if len(acumulador)>0: #len(a)==0 and 
            #Corazon(acumulador)
            #acumulador=''
            #tarjeta=tarjeta+a
            ###print(tarjeta)
            ##print(acumulador)
            
            ###print(acumulador)    
            pp=acumulador.find('?',0)
            if pp>0:

                p=acumulador.find(';',0)
                h=p+15
                
                #[p:h]
                
                mtar=acumulador[p:h]
                #print(mtar)              
                if len(mtar)>=15:
                    #MuestraTarjeta(mtar)
                    tarjeta=mtar
                    acumulador=''
               
        if acumulador=='' and PagandoTk==True :
            ##print('entro1')
            PagandoTk=False
            #apb=0
            #while EnvioPuerto!='':
            #    b=1
            #EnvioPuerto=activot
            #serie.write(activot)
            #time.sleep(0.015)

            #noviembre2017
            #confirmacion=True
            #ColorFijo(1,0,0)
            #confirmacion=True
            #SinNada=True
            ##confirmacion=False
            
            #EntrarTimer=True
            b=1
        if PagandoTk==False and int(ctk)>0 and TimeOut==True:
            FinalizoTks()
            ctk=0
            if ctk==0:
                
                #GiroIndividual(1,0,1,'d')
                ##print('entro')
                
                #confirmacion=True
                #SinNada=True
              
                
                #if confirmacion==True and ctk==0:
                #    #confirmacion=False
                #    GiroIndividual(0,0,1,'i')

                #if confirmacion==True and ctk==0:
                #    #confirmacion=False
                #    ColorFijo(1,0,0)
                #if confirmacion==True and ctk==0:
                #    #confirmacion=False
                #    ColorFijo(1,0,0)
                #if confirmacion==True and ctk==0:
                #    #confirmacion=False
                #    ColorFijo(1,0,0)
                
                #if confirmacion==True and ctk==0:
                #    #confirmacion=False
                #    ColorFijo(0,0,0)
                TerminoPago()
                
        #if  confirmacion==True and PagandoTk==False and int(ctk)==0:

            

             
def Pantalla():
    #aca esta loop visual
    #pygame.display.update()

    while True:
        #totalpagar=ctk
        for event in pygame.event.get():
            pygame.mouse.set_visible(1)
            if event.type == pygame.QUIT:
                 pygame.quit()
                 sys.exit()
                 break
            elif event.type == pygame.KEYDOWN:
                ##print(event.key)
                if event.key == 27:
                    pygame.quit()
                    sys.exit()
                    quit()
                    break
            
            
        #------ PYGAME------
        
        
                       
        
        

       # #screen.fill((0,0,0))
       # #pygame.display.flip()
       # #---------------------------------------------

def TerminoPago():
    global EnvioPuerto
    
    #while EnvioPuerto!='':
    #    b=1
    EnvioPuerto=activot        
    PantFull()
    EnvioPuerto=activot
    #confirmacion=True

def PagoTks():
    global HTimeOut
    #HTimeOut=datetime.now()+timedelta(seconds=3)
    #TSaldo.config(text="Paying E-Tks",fg="blue")
    #Saldo.config(text=ctk,fg="green")
    #TVip.config(text="")
    #Vip.config(text="")
    HTimeOut=datetime.now()+timedelta(seconds=3)

def FinalizoTks():
    global ctk
    global PagandoTk
    global TimeOut
##    global EnvioPuerto
##    while EnvioPuerto!='':
##        b=1
##    EnvioPuerto=bytearray([0x30,0x00,0x35])
    if ctk>0:
        screen.fill((0,0,0))
        bg = pygame.image.load("fondo.jpg")
        screen.blit(bg,(0,0))
        #myfont = pygame.font.SysFont("monospace", 45)
        myfont =pygame.font.Font("super.ttf",50)
        #label = myfont.render("YOU WIN "+ str(ctk) + " Tks!", 1, (0,180,0))
        label = myfont.render("YOU WIN!!!", 1, (0,255,20))
        screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
        myfont =pygame.font.Font("super.ttf",70)
        label1 = myfont.render(str(ctk), 1, (0,255,20))
        screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
        
        pygame.display.flip() 
        pygame.display.update()
        ctk=0
        PagandoTk=False
        TimeOut=False
        colorfijo2('25','100','100','100','101')
##    while EnvioPuerto!='':
##        b=1
##    EnvioPuerto=bytearray([0x30,0x01,0x35])
    
        time.sleep(1.5)
        PantFull()



    


def MuestroPagoTk():
    global contador
    global totalpagar
    global ctk
    global apb
    global confirmacion
    global luz
    contador=0
    totalpagar=0
    luz=0
    apb=1
    #pygame.mixer.music.load("/home/pi/Desktop/etk.mp3")
    
    #while pygame.mixer.music.get_busy() == True:
    #    continue
    
    while True: 
            
            if ctk>0 and TimeOut==False:

                

                
                #pygame.mixer.music.play()
                #time.sleep(1)
                
                #screen.fill(white)
                #if ctk<5:
                screen.fill((0,0,0))
                bg = pygame.image.load("fraspitk.jpg")

                
                screen.blit(bg,(0,0))
                #myfont = pygame.font.SysFont("monospace", 55)
                myfont =pygame.font.Font("super.ttf",42)
                label = myfont.render("Paying Tks", 1, (255,255,255))
                screen.blit(label,(((infoObject.current_w - label.get_width()) / 2),70))
                #myfont = pygame.font.SysFont("monospace", 90)
                myfont =pygame.font.Font("super.ttf",95)
                esto= str(ctk)
                label1 = myfont.render(esto, 1, (0,255,20))
                screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label1.get_height()) / 2)+55))#(label, (((infoObject.current_w - label.get_width()) / 2),160 ))
                pygame.display.flip() 
                pygame.display.update()

                if apb==1 and luz==0:
                    luz=1
                    colorfijo2('24','100','000','100','000')
                    apb=0
                if apb==0 and luz==0:
                    luz=1
                    colorfijo2('24','000','100','000','100')
                    apb=1
                    
                luz=0    
                

            

def PantFull():
                    #infoObject = pygame.display.Info()
                    #screen=pygame.display.set_mode((infoObject.current_w, infoObject.current_h))
                    screen.fill((0,0,0))
                    #pygame.display.update()
                    bg = pygame.image.load("fraspi1.jpg")
                    #time.sleep(.050)
                    screen.blit(bg,(0,0))
                    




                        # initialize font; must be called after 'pygame.init()' to avoid 'Font not Initialized' error
                    myfont =pygame.font.Font("super.ttf",30) #pygame.font.SysFont("monospace", 35)

                        # render text
                    label = myfont.render("swipe your card", 1, (255,255,255))
                    screen.blit(label,(((infoObject.current_w - label.get_width()) / 2)-50,40))#(label, (((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))

                    #myfont = pygame.font.SysFont("monospace", 45)
                    myfont =pygame.font.Font("super.ttf",37)

                    label1 = myfont.render("$10.00", 1, (255,255,255))
                    screen.blit(label1,(275,135))

                    label2 = myfont.render("$9.25", 1, (255,255,255))
                    screen.blit(label2,(275,235))

                    pygame.display.flip() 
                    pygame.display.update()

def MuestraTarjeta():
    #global confirmacion
    global EnvioPuerto
    global tarjeta
    global punto
    global haytarjeta
    haytarjeta=0
    while True:
        #if punto==1 and haytarjeta==0 and tarjeta:
        #        EnvioPuerto=''
                #while EnvioPuerto!='':
                #    a=1
                #time.sleep(.050)
        #        haytarjeta=1
                ##print("desactive")
                #punto=0
       #         EnvioPuerto=desactivot
                #time.sleep(.050)
                #punto=1
                #while EnvioPuerto!='':
                #    b=1
                    
        if tarjeta!='':

                
                
                pygame.font.init
                myfont =pygame.font.Font("super.ttf",37)
                #myfont = pygame.font.SysFont("monospace", 40)
                #bg = pygame.image.load("fondot.jpg")
                #screen.blit(bg,(0,0))
                #esto="One moment please"
                #label = myfont.render(esto, 1, (255,0,0))
                #screen.blit(label, (((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))
                #pygame.display.flip() 
                #pygame.display.update()
                #time.sleep(1)
                
                #ColorFijo(0,0,0)
                #luz2017('apagado')
                #confirmacion=True
                #ColorFijo(0,0,0)
                #time.sleep(.500)
                
                #confirmacion=True
                #confirmacion=True
                #confirmacion=True
                ##colorfijo2('25','100','000','000','000')
                #ColorFijo(1,0,0)
                
                
                
                
                
                #screen.fill(white)
                #ColorFijo(1,0,0)
                bg = pygame.image.load("fondot.jpg")
                screen.blit(bg,(0,0))
                #tarjeta=tarj
                esto=str(tarjeta)+str(' Ok')
                label = myfont.render(esto, 1, (0,255,20))
                screen.blit(label, (((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))
               
                pygame.display.flip() 
                pygame.display.update()
                #EnvioPuerto=desactivot
                luz2017('verde')
                
                #time.sleep(1)
                
                time.sleep(2)
                #while len(EnvioPuerto)>0:
                #    a=1
                
                
                #time.sleep(.250)
                PantFull()
               
                   
                ##print("envie activar")
                #EnvioPuerto=activot
                while EnvioPuerto!='':
                    a=1
                time.sleep(.050)    
                ##print("active")
                EnvioPuerto=activot
                while EnvioPuerto!='':
                    a=1
                 
                tarjeta=''
                punto=0
                haytarjeta=0
                #luz2017('paseo2')

def MiLuz():
    global apb
    if apb=='':
        apb=0
    while True:
       if EnvioPuerto=='' and tarjeta=='' and punto==0 and ctk==0:
           #luz2017('paseo')
           colorfijo2('24','101','001','001','101')
           time.sleep(2)
       if EnvioPuerto=='' and tarjeta=='' and punto==0 and ctk==0:
           colorfijo2('24','001','101','101','001')
           time.sleep(2)
       if int(ctk)>0 and TimeOut==False:
           #luz2017('pagando')
           #colorfijo2('25','100','000','100','000')
           b=1
#       if punto==1:
#           luz2017('verde')


                
def EnviarSerie():
    global EnvioPuerto
    global confirmacion
    global ya
    global hasta
    global envie
    global reintento
    reintento=0
    envie=0
    hasta=datetime.now()#+timedelta(milliseconds=1000)
    while True:
        ya=datetime.now()
        #
        
        if  confirmacion==True and EnvioPuerto!='' and envie==0: 
            ##print("envio")
            envie=1
            #print(bytes(EnvioPuerto))
            reintento=0
            hasta=datetime.now()+timedelta(milliseconds=1000)
            confirmacion=False
            serie.write(bytes(EnvioPuerto))
            serie.flush()
            #EnvioPuerto=''
   
        
        if envie==1 and confirmacion==True:
            envie=0
            EnvioPuerto=''
        
        if confirmacion==True and ya > hasta and  EnvioPuerto!='' and reintento< 3 and envie==1: 
             #confirmacion=True
             #EnvioPuerto=''
             #print("reintento")
             time.sleep(1)
             #print(bytes(EnvioPuerto))
             reintento=reintento+1
             envie=1
             hasta=datetime.now()+timedelta(milliseconds=1800)
             confirmacion=False
             serie.write(bytes(EnvioPuerto))
             serie.flush()
             #EnvioPuerto=''

        if confirmacion==False and ya > hasta:
            confirmacion=True

        if reintento>=3:
            EnvioPuerto=''
            confirmacion=True
        
#----Config General-----
#pygame.mouse.set_visible(settings.mouse_enabled)
pygame.mouse.set_visible(0)

infoObject = pygame.display.Info()
screen=pygame.display.set_mode((infoObject.current_w, infoObject.current_h))                
myfont = pygame.font.SysFont("monospace", 40)
PantFull()
pygame.display.toggle_fullscreen()
#programafinal-----------------------------


#HILOS
TimeOutTk=threading.Thread(target=TiempoTk)
TimeOutTk.start()
escuchar=threading.Thread(target=EscuchoPuerto)
escuchar.start()
mserie=threading.Thread(target=PuertoSerie)
mserie.start()
mpantalla=threading.Thread(target=Pantalla)
mpantalla.start()
tpago=threading.Thread(target=MuestroPagoTk)
tpago.start()
tenvio=threading.Thread(target=EnviarSerie)
tenvio.start()#MuestraTarjeta
btarjeta=threading.Thread(target=MuestraTarjeta)
btarjeta.start()
while EnvioPuerto!='':
    b=1
EnvioPuerto=activot

#luces

luz=threading.Thread(target=MiLuz)#MiLuz
luz.start()
#------------------------------------------




    

#ventana.mainloop()






