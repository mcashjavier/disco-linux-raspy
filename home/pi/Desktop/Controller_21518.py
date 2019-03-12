#!/usr/bin/python3
import threading
#qr
import pyqrcode

from gpiozero import CPUTemperature
from tkinter import *
from tkinter.font import Font
import subprocess
import socket
import sys
import os
import json
import serial
import binascii
import pygame
import webbrowser
import io

from datetime import datetime,timedelta
import time

global tarjetalista
global controlluces
global rojo_dos_mundos
global UltTar
global infoObject
global screen
global acumulador
global SinNada
global EntrarTimer
global qcolor
global TimeOut
global llego1
global EnvioPuerto
global ConfirmacionCoin
global ConfirmacionTarjeta
global confirmacionMudo

global LabelPase
global LabelPagando
global LabelPago
#global sock

global MiIp
global MiMac

global PantallaPago

global confirmacion
global ctk
global tarjeta
global PagandoTk
global HTimeOut
global TerminoOperancion

global DosMundosc

global lcolor1
global lcolor2
global lcolor3
global xa
global jluces
global apb


global ColorVerde
global ColorRojo
global NumMaq
global IP
global PUERTO
global PV
global PN
NumMaq='XXXX'
IP='192.168.1.219'
PUERTO='5000'
MiIp='000.000.000.000'
MiMac='00:00:00:00:00:00'
LabelPase='SWIPE YOUR CARD'
LabelPagando='Paying Tks'
LabelPago='You Win'
PN='-'
PV='-'
confirmacionMudo=True

#sock.settimeout(1.0)
ConfirmacionTarjeta=True
tarjetalista=0
TerminoOperacion=1
PantallaPago=0

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
controlluces=1

#------Luces 2 mundos Def------
M1=[0x25,0X49,0x24,0X97,0x49,0x24,0x97]
M0=[0x26]
M2=[0x25,0x49,0x24,0x92,0xE9,0x24,0x97]
M3=[0x25,0x49,0x24,0x92,0x5D,0x24,0x97]
M4=[0x25,0x49,0x24,0x92,0x4B,0xA4,0x97]
M5=[0x25,0x49,0x24,0x92,0x49,0x74,0x97]
M6=[0x25,0x49,0x24,0x92,0x49,0x2E,0x97]
M7=[0x25,0x49,0x24,0x92,0x49,0x25,0xD7]
M8=[0x25,0x49,0x24,0x92,0x49,0x24,0xBF]
M9=[0x25,0x49,0x24,0x92,0x49,0x24,0x97]
M10=[0x25,0x49,0x24,0x97,0x49,0x24,0x92]
M11=[0x25,0x00,0x00,0x00,0x00,0x00,0x07]
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
os.system('setterm -cursor off')
#------------------------------

#para qr 2018
def saveQRCodePNG(Nombre,Tam,Que):
        try:
            os.remove(Nombre+".png")
        except:
            b=1
        fullfilename = "/home/pi/"+Nombre
        objectQRCode = pyqrcode.create(Que)
        with open(fullfilename + ".png", 'wb') as fstream:
            objectQRCode.png(fstream, scale=Tam)
        # same as above
        objectQRCode.png(fullfilename + ".png", scale=Tam)
        # in-memory stream is also supported
        buffer = io.BytesIO()
        objectQRCode.png(buffer)
        # do whatever you want with buffer.getvalue()
        print("Qr Creado Ok") 

#
def listen():
    global PN
    global PV
    global IP
    global NumMaq
    global tarjeta
    global controlluces
    global EnvioPuerto
    global ConfirmacionTarjeta
    global punto
    connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    connection.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    connection.bind(('', 5001))#0.0.0.0
    connection.listen(1)
    cpu=CPUTemperature()
    while True:
        current_connection, address = connection.accept()
        while True:
            data = current_connection.recv(66)



            if data:
                #current_connection.send(data)
                traje=data.decode('utf8')
                print(traje)
                EnvioNo=''
                if traje[:3]=='300':
                        
                    controlluces=0
                    EnvioPuerto=''
            
                    serie.flush()
                    time.sleep(0.100)
                    ConfirmacionTarjeta=False
                    while ConfirmacionTarjeta==False:
                        serie.write(bytes(activot))
                        time.sleep(0.100)
                    #tarjeta=traje[7:21]+'?'
                    print('llego 300 ' + str(tarjeta))
                    EnvioNo='103'
                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                    #time.sleep(0.8)
                    punto=1
                    tarjeta=traje[7:21]+'?'
                if traje[:3]=='Num':
                    time.sleep(0.3)
                    NumMaq=traje[3:7]
                    EnvioNo='NumMaq Changed OK to '+str(NumMaq)
                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                    time.sleep(3.0)
                if traje[:3]=='log':
                    EnvioNo='Controller: My Mac is '+str(MiMac)+ '\r\n'
                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                    EnvioNo='Controller: My Ip is '+str(MiIp)+ '\r\n'
                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                    EnvioNo='Controller: My Temperature is '+str(cpu.temperature)+ '\r\n'
                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                    EnvioNo='Controller: Master Ip is '+str(IP)+ '\r\n'
                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                    EnvioNo='Controller: MaqNumber is '+str(NumMaq)+ '\r\n'
                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                    EnvioNo='Controller: Normal Price is '+str(PN)+ '\r\n'
                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                    EnvioNo='Controller: Vip Price is '+str(PV)+ '\r\n'
                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))

                    time.sleep(10.0)
                    #return
                elif traje[:8]=='masterip':
                    EnvioNo='Controller: Master Ip is '+str(IP)+ '\r\n'
                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                    #IP=traje[6:]
                    #print(str(IP))
                    #EnvioNo='Controller: Master IP Changed OK to '+str(IP) 
                    #current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                    time.sleep(5.0)
                    #return
                elif traje[:6]=='master':
                    EnvioNo='Controller: Prev Ip '+str(IP)+ '\r\n'
                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                    IP=traje[6:]
                    print(str(IP))
                    EnvioNo='Controller: Master IP Changed OK to '+str(IP) + '\r\n'
                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                    time.sleep(5.0)
                    #return
                elif traje[:4]=='wifi':
                  
                  EnvioNo='Preparando Conexion Wifi...'
                  current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                  os.system("sudo python3 /home/pi/ControllerConfig/configw.py -f")
        
                elif traje[:7]=='hotspot':
                  
                  EnvioNo='Preparando Hotspot Wifi...'
                  current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                  os.system("sudo python3 /home/pi/ControllerConfig/configh.py -f")

                elif traje[0:17]==MiMac:
                  print('entro')
                  NumMaq=traje[21:25]
                  PN=traje[25:32]
                  PV=traje[32:39]
                  PantFull()
                  EnvioNo='103'
                  current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                  
                else:
                  EnvioNo=str(ObtenerMac('wlan0'))
                  current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                  
            time.sleep(0.010)      
            if EnvioNo!='':
              current_connection.close()
              break


def ObtenerMac(interface='eth0'):
  global MiIp
  global MiMac
  # Return the MAC address of the specified interface
  try:
    str = open('/sys/class/net/%s/address' %interface).read()
    gw = os.popen("ip -4 route show default").read().split()
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.connect((gw[2], 0))
    ipaddr = s.getsockname()[0]
    MiIp=ipaddr
    gateway = gw[2]
    host = socket.gethostname()
    MiMac=str[0:17]
    msj=subprocess.check_output("hostname -I",shell=True).decode('utf8')
    MiIp=msj.strip()
    return MiIp#str[0:17]+ ','+ ipaddr + ','+ NumMaq
  except:
    msj=subprocess.check_output("hostname -I",shell=True).decode('utf8')
    MiIp=msj.strip()      
    str = "00:00:00:00:00:00"
    return 'NO'#str[0:17]+ ','+ ipaddr + ','+ NumMaq

def Debitar():
    global tarjeta
    global NumMaq
    global balance
    global EnvioPuerto
    global UltTar
    global controlluces
    global tarjetalista
    global TerminoOperacion
    global confirmacion
    global ConfirmacionCoin
    global ConfirmacionTarjeta
    global LabelPase
    global LabelPagando
    global LabelPago
    global confirmacionMudo
    ConfirmacionCoin=0
    ConfirmacionTarjeta=False
    print('llego con tarjeta '+str(tarjeta))
    if tarjeta !='':# and TerminoOperacion==1:
        #print(tarjeta)
        tarjeta2018=tarjeta
        print(tarjeta2018)
        balance=''
        try:
                   # Create a TCP/IP socket
                    print('previo a conectar')
                    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                    server_address = (str(IP),5000)#192.168.2.50 -- IAAPA
                    print('connecting to %s port %s' % server_address)
                    sock.settimeout(5.0)
                    sock.connect(server_address)
                    message = MiMac+','+MiIp+','+NumMaq +'*'+tarjeta2018[:-1] + str(PN) + str(PV) + '99' #tarjeta.get() #'C;1921000005618'
                    

                    # Connect the socket to the port where the server is listening

                    print('paso conexion')
                    

                    
                
                        # Send data
                    #message = NumMaq +'*'+tarjeta[:-1] + '1.00   1.00   99' #tarjeta.get() #'C;1921000005618'
                    #print('sending "%s"' % message)
                    sock.settimeout(1.0)
                    sock.sendall(message.encode(encoding='UTF-8',errors='ignore'))

                    # Look for the response
                    amount_received = 0
                    amount_expected = 1#len(message)
                    data1=''
                    data=''
                    yat=datetime.now()
                    hastat=datetime.now()+timedelta(milliseconds=2000)
                    #amount_received < amount_expected and
                    while hastat > yat and data=='':
                        data = sock.recv(48)
                        #print(data)
                        #amount_received += len(data)
                        #encoded
                        #data_string = json.dumps(data.decode('utf8'))

                        #Decoded
                        #decoded = json.loads("{'credito': '75.1'}")
                    data1=data.decode('utf8')
                    print(str(len(data1)))
                        ##print(data1.lstrip("{"))
                        

                    if len(data1)!=48:
                            #Saldo.config(text ="New card")
                            #bonus.config(text ="")
                            #tk.config(text ="")
                            balance='Invalid Transaction'
                            screen.fill((0,0,0))
                            pygame.font.init
                            myfont =pygame.font.Font("super.ttf",35)

                            bg = pygame.image.load("fondot.jpg")
                            screen.blit(bg,(0,0))

                            esto=balance#data#str(tarjeta)+str(' Ok')
                            #label = myfont.render(esto, 1, (0,255,20))
                            #screen.blit(label, (((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))

                            label = myfont.render("Network Error", 1, (0,255,20))
                            screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
                            myfont =pygame.font.Font("super.ttf",25)
                            label1 = myfont.render(esto, 1, (0,255,20))
                            screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                           
                            pygame.display.flip() 
                            pygame.display.update()
                            luz2017('rojo')
                            time.sleep(1)
                    elif data1[1:2]=='0':#data1=='sin' or data1=='SIN':
                            #Saldo.config(text ="New card")
                            #bonus.config(text ="")
                            #tk.config(text ="")
                            print("Entro a sin saldo")
                            #while tarjetalista==0:
                            #    b=1

                            
                            #balance='[Recharge for Play Again]'
                            screen.fill((0,0,0))
                            pygame.font.init
                            myfont =pygame.font.Font("super.ttf",35)

                            bg = pygame.image.load("fondot.jpg")
                            screen.blit(bg,(0,0))
                            balance=data1[14:-2].lstrip()
                            print(data1)

                            esto=balance#data#str(tarjeta)+str(' Ok')
                            
                            esto1=balance#[2:].lstrip()
                            
                            esto1=esto1.rstrip()
                            print(esto1)
                            #label = myfont.render(esto, 1, (0,255,20))
                            #screen.blit(label, (((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))
                            if esto1[:5]=='Saldo':# or esto[1:6]=='Saldo':
                                LabelPase='Pase Su Tarjeta'
                                LabelPagando='Pagando Tks'
                                LabelPago='Ganaste!!!'
                            else:
                                LabelPase='Swipe Your Card'
                                LabelPagando='Paying Tks'
                                LabelPago='You Win!!!'
                            print(esto+' Eze')
                            if esto1[:5]=='Saldo':# or esto[1:6]=='Saldo':
                                label = myfont.render("SIN SALDO      ", 1, (255,255,255))
                                linea1=esto[:16].lstrip()
                                linea2=esto[16:].lstrip()
                            elif esto[:2]=='Su':
                                label = myfont.render("SIN SALDO      ", 1, (255,255,255))
                                linea1=esto[:15].lstrip()
                                linea2=esto[15:].lstrip()
                            else:
                                label = myfont.render("NO BALANCE     ", 1, (255,255,255))
                                linea1=esto[:16].lstrip()
                                linea2=esto[16:].lstrip()
                            
                            
                            linea3='Tickets: '+ str(int(data1[10:14]))
                            #if len(linea1)<20:
                            #    while len(linea1)<20:
                            #            linea1=linea1 + ' '
                            if len(linea2)<len(linea1):
                                while len(linea2)<len(linea1):
                                        linea2=linea2 + ' '
                            if len(linea3)<len(linea1):
                                while len(linea3)<len(linea1):
                                        linea3=linea3 + ' '
                            
                            #pygame.event.wait()
                            #while pygame.mixer.music.get_busy():
                            #print("Playing...")
                            #time.sleep(500)
                            
                            screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-115))
                            myfont =pygame.font.Font("super.ttf",30)
                            label1 = myfont.render(linea1, 1, (0,255,0))
                            screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-30))
                            myfont =pygame.font.Font("super.ttf",30)
                            label1 = myfont.render(linea2, 1, (0,255,0))
                            screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+15))
                            #print(esto[1:3])
                            if esto[:2]=='Su':
                                LabelPase='Pase Su Tarjeta'
                                LabelPagando='Pagando Tks'
                                LabelPago='Ganaste!!!'
                            else:
                                LabelPase='Swipe Your Card'
                                LabelPagando='Paying Tks'
                                LabelPago='You Win !!!'
                            if esto[:4].lstrip() !='Card' and esto[:2].lstrip()!='Su' and esto[:3]!='Hay' and esto[:7].lstrip()!='Tarjeta':
                                myfont =pygame.font.Font("super.ttf",30)
                                label2 = myfont.render(linea3, 1, (0,255,0))
                                screen.blit(label2,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+60))
                           
                            pygame.display.flip() 
                            pygame.display.update()
                            #while EnvioPuerto!='':
                            #    b=1
                            #luz2017('rojo')
                            #ColorRojo
                            serie.flush()

                            EnvioPuerto=''
                            #time.sleep(.200)
                            EnvioPuerto=''

                            #while tarjetalista==0:
                            #    b=1
                            #serie.write(bytes(bytearray([0x25,0x49,0x24,0x92,0x49,0x24,0x97])))
                            #tarjetalista=0
                            
                            #serie.flush()
                            #EnvioPuerto=''#ColorRojo
                            #serie.flushOutput()
                            #sock.sendall(str(ColorRojo))
                            #time.sleep(.200)
                            if len(data1)==48:
                                balance=data1
                                mok='103'
                                sock.sendall(mok.encode(encoding='UTF-8',errors='ignore'))
                                #confirmacion=False
                                confirmacionMudo=False
                                while confirmacionMudo==False:
                                    serie.flush()
                                    serie.write(bytes([0x3C]))
                                    time.sleep(0.150)
                                pygame.mixer.music.load('/home/pi/Desktop/rojo.mp3')
                                pygame.mixer.music.play()
                                controlluces=0
                                while controlluces==0:
                                        serie.write(bytes(ColorRojo))
                                        time.sleep(1.100)
                                
                                
                                #time.sleep(1)
                                #os.system("omxplayer -b /home/pi/Desktop/rojo.mp3")
                                
                                #time.sleep(1.00)
                                #while confirmacion==False:
                                #    b=1
                            #tarjetalista=0
                    elif data1[1:2]=='1':#len(data1)>10:
                            UltTar=tarjeta2018
                            print("Entro con saldo")


                            ee=bytearray()
                            ee.append(0x2A)
                            ee.append(0x92)
                            ee.append(0x49)
                            ee.append(0x24)
                            ee.append(0x92)
                            ee.append(0x49)
                            ee.append(0x27)
                            serie.flush()

                            EnvioPuerto=''

                            #while tarjetalista==0:
                            #    print('esperando tarjetalista')
                            #    b=1

                            
                            balance=data1[14:-2].lstrip()

                            screen.fill((0,0,0))
                            pygame.font.init
                            myfont =pygame.font.Font("super.ttf",35)

                            bg = pygame.image.load("fondot.jpg")
                            screen.blit(bg,(0,0))

                            esto=balance#data#str(tarjeta)+str(' Ok')
                            #label = myfont.render(esto, 1, (0,255,20))
                            #screen.blit(label, (((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))
                            #print(esto[1:6])

                            if esto[:5]=='Saldo':# or esto[1:6]=='Saldo':
                                LabelPase='Pase Su Tarjeta'
                                LabelPagando='Pagando Tks'
                                LabelPago='Ganaste!!!'
                            else:
                                LabelPase='Swipe Your Card'
                                LabelPagando='Paying Tks'
                                LabelPago='You Win!!!'
                            #print(str(esto[2:7]))
                            if esto[:5]=='Saldo':# or esto[1:6]=='Saldo':
                                label = myfont.render("JUEGUE!!!      ", 1, (255,255,255))
                            else:
                                label = myfont.render("PLAYER OK      ", 1, (255,255,255))    

                            linea1=esto[:16].lstrip()
                            linea2=esto[16:].lstrip()
                            linea3='Tickets: '+ str(int(data1[10:14]))
                            #if len(linea1)<20:
                            #    while len(linea1)<20:
                            #            linea1=linea1 + ' '
                            if len(linea2)<len(linea1):
                                while len(linea2)<len(linea1):
                                        linea2=linea2 + ' '
                            if len(linea3)<len(linea1):
                                while len(linea3)<len(linea1):
                                        linea3=linea3 + ' '
                            
                            #time.sleep(300)
                                    #clock.tick(100)
                            #confirmacionMudo=False
                            #while confirmacionMudo==False:
                            serie.flush()
                            #serie.write(bytes([0x3A]))
                            #time.sleep(0.100)
                            screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-115))
                            myfont =pygame.font.Font("super.ttf",30)
                            label1 = myfont.render(linea1, 1, (0,255,0))
                            screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-30))
                            myfont =pygame.font.Font("super.ttf",30)
                            label1 = myfont.render(linea2, 1, (0,255,0))
                            screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+15))
                            myfont =pygame.font.Font("super.ttf",30)
                            label2 = myfont.render(linea3, 1, (0,255,0))
                            screen.blit(label2,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+60))                            
                            
                            pygame.display.flip() 
                            pygame.display.update()
                            #time.sleep(.030)
                            
                            
                            #time.sleep(1)
                            #luz2017('verde')
                            #while EnvioPuerto!='':
                            #    b=1

                            #confirmacion=True
                            #EnviarPuerto=ee
                            confirmacionMudo=False
                            while confirmacionMudo==False:
                                    serie.flush()
                                    serie.write(bytes([0x3C]))
                                    time.sleep(0.100)
                            pygame.mixer.music.load('/home/pi/Desktop/verde1.mp3')
                            pygame.mixer.music.play()
                            
                            
                            print("Playing...")
                            if len(data1)==48:
                                ConfirmacionCoin=0
                                while ConfirmacionCoin==0:
                                    print("EnviandoCoin")
                                    serie.write(bytes(ee))
                                    time.sleep(1.1)
                                
                                #os.system("omxplayer -b /home/pi/Desktop/verde1.mp3")
                                
                                #time.sleep(1)

                                balance=data1
                                mok='103'
                                sock.sendall(mok.encode(encoding='UTF-8',errors='ignore'))


                            
                            
                            tarjetalista=0
                            time.sleep(0.1)
                            
                            
                       
                            
                           
                               


                            

                            ##print(python_obj[4]["credito"])
                            ##print(python_obj[5]["bonus"])
                            ##print(python_obj[6]["tk"])
                        
                        
                        #pets = set(data_string)
                        ##print(json.dumps(pets, default=jdefault))
                        #esto=decoded
                        #print ("Tenemos "+ decoded["credito"])
                        #esto=json.dumps(data_string, default=jdefault)#str(decoded['credito'])
                        ##print('received "%s"' % esto)
                       # Saldo.text=
        except:
            print('error al conectar')
            serie.write(bytearray(bytes([0x25,0x6D,0xB6,0xDB,0x6D,0xB6,0xDF])))
            balance='Network Error'
            screen.fill((0,0,0))
            pygame.font.init
            myfont =pygame.font.Font("super.ttf",35)

            bg = pygame.image.load("fondot.jpg")
            screen.blit(bg,(0,0))

            esto=balance#data#str(tarjeta)+str(' Ok')
                            #label = myfont.render(esto, 1, (0,255,20))
                            #screen.blit(label, (((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))

            label = myfont.render("Network Error", 1, (0,255,20))
            screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
            myfont =pygame.font.Font("super.ttf",25)
            label1 = myfont.render(esto, 1, (0,255,20))
            screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                           
            pygame.display.flip() 
            pygame.display.update()
            #luz2017('rojo')
            time.sleep(1)
            

            
            #sock.close()
            #Debitar()
            #return
            b=1

        finally:
            #tarjeta=''
            #punto=0
            
            print('closing socket')
            sock.close()
            

            
            time.sleep(1.0)
            #EnvioPuerto=activot    
            #TerminoOperacion=1
            
            PantFull()
            EnvioPuerto=''
            
            serie.flush()
            time.sleep(0.100)
            ConfirmacionTarjeta=False
            while ConfirmacionTarjeta==False:
                serie.write(bytes(activot))
                time.sleep(0.100)
            #EnvioPuerto=''
            #serie.flush()
            #while confirmacion==False:
            #time.sleep(0.010)
            #controlluces=0
            #while controlluces==0:
            #serie.write(bytearray([0x25,0x09,0x00,0x09,0x00,0x49,0xF8]))#bytearray([0x28,0x10,0x11,0x01,0x02,0x05,0x09])#bytes(a)
            #time.sleep(1.00)
            #time.sleep(1.00)
            #serie.flush()#Ouput()
            #time.sleep(1)

            #serie.flush()#Ouput()
            #time.sleep(.030)
            #serie.write(bytes(activot))
            #time.sleep(.020)
            
            tarjetalista=0
            haytarjeta=0            
            punto=0
            
            
            controlluces=1
            TerminoOperacion=1
            confirmacionMudo=False
            while confirmacionMudo==False:
                    serie.flush()
                    serie.write(bytes([0x3A]))
                    time.sleep(0.150)
            print("termino")

            #serie.flush()
            #confirmacion=False
            #serie.write(bytes(activot))
            #while confirmacion==False:
            #    b=1
            #luz2017('paseo')
            #time.sleep(1)
            
            #time.sleep(0.030)
            #while EnvioPuerto!='':
            #    b=1
            #EnvioPuerto=bytearray([0x24,0x00,0x00,0x00,0x00,0x00,0x00])#bytes(a)
            
            #EnvioPuerto=bytearray([0x24,0x00,0x00,0x00,0x00,0x00,0x00])#bytes(a)
            #serie.write(EnvioPuerto1)
    
    #'else:
    #'    Saldo.config(text = 'Sin tarjeta...')


def DosMundos():
        global confirmacion
        global DosMundosc
        print('entro dos mundos '+str(controlluces))
    #if len(tarjeta)==0:    
        if DosMundosc==0 and controlluces==1:
            serie.write(bytes(rojo_dos_mundos))
            DosMundosc=1
            return
            #confirmacion=False
        if DosMundosc==1 and controlluces==1:
            serie.write(bytes(rojo_dos_mundos1))
            DosMundosc=2
            return
            #confirmacion=False
        if DosMundosc==2 and controlluces==1:
            serie.write(bytes(rojo_dos_mundos2))
            DosMundosc=3
            return
            #confirmacion=False
        if DosMundosc==3 and controlluces==1:
            serie.write(bytes(rojo_dos_mundos3))
            DosMundosc=4
            return
            #confirmacion=False
        if DosMundosc==4 and controlluces==1:
            serie.write(bytes(rojo_dos_mundos4))
            DosMundosc=5
            return
            #confirmacion=False
        if DosMundosc==5 and controlluces==1:
            serie.write(bytes(rojo_dos_mundos5))
            DosMundosc=6
            return
            #confirmacion=False
        if DosMundosc==6 and controlluces==1:
            serie.write(bytes(rojo_dos_mundos6))
            DosMundosc=7
            return
            #confirmacion=False
        if DosMundosc==7 and controlluces==1:
            serie.write(bytes(rojo_dos_mundos7))
            DosMundosc=8
            return
            #confirmacion=False
        if DosMundosc==8 and controlluces==1:
            serie.write(bytes(rojo_dos_mundos8))
            DosMundosc=9
            return
            #confirmacion=False
        if DosMundosc==9 and controlluces==1:
            serie.write(bytes(rojo_dos_mundos9))
            DosMundosc=10
            return
            #confirmacion=False
        if DosMundosc==10 and controlluces==1:
            serie.write(bytes(blanco_giro))
            DosMundosc=0
            return
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
    global UltTar
    
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
    UltTar=''
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
        ###print(PagandoTk)
        ###print(ctk)
        ###print(SinNada)

        

        #Ahora=datetime.now()
        if PagandoTk==False and int(ctk)>0:
            Ahora=datetime.now()
               
            #if TimeOut==False:
            #    TimeOut=False
            
            if Ahora >= HTimeOut and TimeOut==False:
                
                if PagandoTk==False and int(ctk)>0:
                    
                    TimeOut=True
                    ####print('Termino tiempo de Tk')
                    ####print('Envia Tks')
                    ####print(ctk)
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
    
    
    

serie = serial.Serial('/dev/serial0', 9600, timeout=0.2, writeTimeout=0)
#seriet = serial.Serial('/dev/serial0', 9600, timeout=0.2, writeTimeout=0)

activot=bytearray()#([0x26])#,0x00,0x00,0x00,0x00,0x00,0x00])
activot.append(0x26)
#activot.append(0x00)
#activot.append(0x00)
#activot.append(0x00)
#activot.append(0x00)
#activot.append(0x00)
#activot.append(0x00)

desactivot=bytearray()#([0x27])#,0x00,0x00,0x00,0x00,0x00,0x00])
desactivot.append(0x27)
#desactivot.append(0x00)
#desactivot.append(0x00)
#desactivot.append(0x00)
#desactivot.append(0x00)
#desactivot.append(0x00)
#desactivot.append(0x00)

pcorto=bytearray()#[0x29,0x10])#,0x00,0x00,0x00,0x00,0x00])#pago rapido-coin rapido
pcorto.append(0x29)
pcorto.append(0x10)
serie.write(pcorto)
serie.flushOutput()
time.sleep(1)
pcorto1=bytearray()#[0x30,0x01,0x35])#,0x00,0x00,0x00,0x00]) #habilita pago
pcorto1.append(0x30)
pcorto1.append(0x01)
pcorto1.append(0x27)#35 electronico // 60 fisico
 #   pcorto.append(0x10)
serie.write(pcorto1)
serie.flushOutput()
time.sleep(1)
serie.write(activot)
serie.flushOutput()
time.sleep(1)

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
        ####print(confirmacion)
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
            ###print(dato.decode('utf-8'))


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
            ####print(xa)
            
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
            ###print(color)    
            envio=a24+color
            a=hex(int(envio,2))
            a=bytearray.fromhex(a[2:16])
            ####print(a)
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
                ####print(llego)    
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
                #        ###print(llego[p:h])        
                #    confirmacion=True
                #    llego=''
                #if llego=='@':   
                #    llego=''
                #    ctk=ctk+1
                #    ###print(ctk)
                #    confirmacion=True
                #if len(llego)>0 and confirmacion==False:
                    #llego=serie.readline(15).decode('utf-8')
                    #serie.flushInput()
                #    #confirmacion=False
                #    tarjeta=tarjeta+llego
                #    llego=''
                 #   ###print(tarjeta)
                 #   if len(tarjeta)>14:
                 #       ###print(tarjeta)
                        #tarjeta=''
                #        confirmacion=True
              
            
            #time.sleep(00.060)
                
            #color=''
            #xx=0
            #lcolor1=lcolor1+3
            #lcolor2=lcolor2+3
            #lcolor3=lcolor3+3
            ####print(lcolor1)
            
            confirmacion=True
            ###print('entre')
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
        

            ###print(color)    
        envio=a24+color
        a=hex(int(envio,2))
        a=bytearray.fromhex(a[2:16])
            ####print(a)
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
            ####print(xa)
            
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
                
        ###print(color)
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
            ####print(xa)
            
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
        ##print(len(color))
        #print (len(a24))
        ##print(len(envio))

        #Anda 1
        a=hex(int(envio,2))
        a=bytearray.fromhex(a[2:16])

        #Anda 2
        #a=bytearray([int(envio[i:i+8],2)for i in range(0,len(envio),8)])

        #a=int(envio,2).to_bytes(8,'big')
                                 
        
        
        ##print(a)
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
        ##print("envia luz colorfijo")
            
        EnvioPuerto=a#bytearray([0x28,0x10,0x10,0x10,0x10,0x10,0x01])#bytes(a)
        #while EnvioPuerto!='':
        #            b=1
        
            

def luz2017(cual):
    global EnvioPuerto
    global punto
    global apb
    global controlluces
    global ConfirmacionTarjeta
    if apb=='':
        apb=0
    if cual=='paseo2':
        while EnvioPuerto!='' :
            b=1
        if punto==0: #and tarjeta=='':
            ###print("yo1")
            #EnvioPuerto=bytearray([0x25,0x11,0x11,0x11,0x11,0x11,0x11])#bytes(a)
            EnvioPuerto=ColorRojo#ColorRojo#bytearray([0x25,0xFF,0xFF,0xFF,0xFF,0xFF,0xF8])#bytes(a)
    if cual=='paseo':
        #while EnvioPuerto!='':
        #    b=1
        if controlluces==1 and EnvioPuerto=='' and punto==0 and tarjeta=='':#tarjeta=='' :#punto==0 and tarjeta=='' and ctk==0:
            #print("yo2")
            controlluces=0
            EnvioPuerto=bytearray([0x25,0x09,0x00,0x09,0x00,0x49,0xF8])#bytearray([0x28,0x10,0x11,0x01,0x02,0x05,0x09])#bytes(a)
            time.sleep(0.9)
            controlluces=1
            return
            #time.sleep(1.5)
        
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
        while EnvioPuerto!='':
            b=1
        serie.flush()
        time.sleep(0.2)
        ConfirmacionTarjeta=False
        while ConfirmacionTarjeta==False:
            serie.write(bytes(activot))
            time.sleep(0.200)
            
        
        #EnvioPuerto=activot
        #while EnvioPuerto!='':
        #    b=1
        #EnvioPuerto=bytearray([0x24,0x00,0x00,0x00,0x00,0x00,0x00])#bytes(a)
        
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
            ####print(xa)
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
            ####print(a)
        #if confirmacion==True:
        #    #confirmacion=False
        serie.flush()
        time.sleep(.050)
        serie.write(bytes(a))
        #while len(EnvioPuerto)>0:
        #    a=1
        #EnvioPuerto=bytes(a)
        #while EnvioPuerto!='':
        #    b=1
        #time.sleep(.050)    
        ##print("envia luz #colorfijo2")
        #if punto==0:   
        #    EnvioPuerto=a#bytes(a)

def mprueba():
    global xa
    global jluces
    #xa=0
    if True:
        ###print(xa)
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
    global controlluces
    global tarjetalista
    global ConfirmacionCoin
    global ConfirmacionTarjeta
    global confirmacionMudo
    #global screen
    tarjeta=''
    EntrarTimer=False
    doblev=''
    
    apb=0
    acumulador=''
    recibi=''
    luz='no'
    while True:
        try:
            a=serie.read(1).decode('utf-8')
            #serie.flushInput()    
            if len(a)>0:
                #print(a)
                recibi=recibi+a
                if recibi.find('W',0)>-1:
                    doblev='W'
                if TerminoOperacion==1:
                    acumulador=acumulador+a
            #else:
            #    if controlluces==1:
            #        luz2017('paseo')
            if a=='Z':
                ConfirmacionTarjeta=True
            if a=='Y':
                ConfirmacionCoin=1
            if a=='X':
                tarjetalista=1
            if a=='V' and ctk==0:
                controlluces=1
            if a=='U':
                confirmacion=True
            if a=='T':
                confirmacionMudo=True
                print('Mudo Ok')
            if  recibi.find('U',0)>-1:
                ###print("confirmo")
                confirmacion=True
                #EnvioPuerto=''
            if  recibi.find('V',0)>-1 and ctk==0:
                controlluces=1
            if  recibi.find('X',0)>-1:
                tarjetalista=1
            
                #return
            if a==";" and tarjeta=='':
                serie.flushOutput()
                #controlluces=0
                if punto==0:
                    #EnvioPuerto=''
                    #controlluces=1
                    #while EnvioPuerto!='':
                    b=1
                    #EnvioPuerto=desactivot
                    #while EnvioPuerto!='':
                    #    b=1
                #serie.flushOutput()
                #EnvioPuerto=''
                #serie.write(bytes(desactivot))
                punto=1
                
            ar=recibi.find('@',0)
            if ar>-1 and UltTar!='':
                 if PagandoTk==False:
                     #if confirmacion==True:
                     
                     EnvioPuerto=desactivot
                     #serie.write(desactivot)
                     EntrarTimer=False
                 PagandoTk=True
                 controlluces=0
                 TimeOut=False
                 esto=recibi
                 recibi=''
                 x=0
                 while x< len(esto):
                     esta=esto.find("@",x,x+1)
                     if esta>-1:
                         ctk=ctk+1
                     x=x+1
                 
                 HTimeOut=datetime.now()+timedelta(seconds=4)
                 #colorfijo2('24','100','000','100','000')   
                        #luz=1

                 
                     
                 
                 #HTimeOut=datetime.now()+timedelta(seconds=3)
                 
                #noviembre2017
                 recibi=''
                 #acumulador=''
                 HTimeOut=datetime.now()+timedelta(seconds=3)
                 #if confirmacion==True:
                 #    colorfijo2('25','100','000','100','000')
                 
                 PagandoTk=False
                 #break
                 ###print(ctk)
                 #-----PARA PAGO FISICO---
            ar=recibi.find('A',0)
            if ar>-1 and UltTar!='':
                 if PagandoTk==False:
                     #if confirmacion==True:
                     
                     EnvioPuerto=desactivot
                     #serie.write(desactivot)
                     EntrarTimer=False
                 PagandoTk=True
                 controlluces=0
                 TimeOut=False
                 esto=recibi
                 recibi=''
                 x=0
                 while x< len(esto):
                     esta=esto.find("A",x,x+1)
                     if esta>-1:
                         ctk=ctk+1
                     x=x+1
                 
                 HTimeOut=datetime.now()+timedelta(seconds=4)
                 #colorfijo2('24','100','000','100','000')   
                        #luz=1

                 
                     
                 
                 #HTimeOut=datetime.now()+timedelta(seconds=3)
                 
                #noviembre2017
                 recibi=''
                 #acumulador=''
                 HTimeOut=datetime.now()+timedelta(seconds=3)
                 #if confirmacion==True:
                 #    colorfijo2('25','100','000','100','000')
                 
                 PagandoTk=False
                 #break
                 ###print(ctk)
            recibi=''
            #if confirmacion==True and controlluces==0 and punto==0 and ctk==0:
            #    controlluces=1

            #if controlluces==0:
            #    MiLuz()
        except:
            print("murio serie")
            serie = serial.Serial('/dev/serial0', 9600, timeout=0.2, writeTimeout=0)
            continue
        
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
    global UltTar
    global controlluces
    global TerminoOperacion
    global ConfirmacionTarjeta
    #global screen
    tarjeta=''
    EntrarTimer=False
    apb=0
    
    
    #acumulador=''
    while True:
        a=''        

        doblev=''     
        if len(acumulador)>0: #len(a)==0 and 
   
            pp=acumulador.find('?',0)
            if pp>0:#and controlluces==0:                
                    
                    serie.flush()
                    EnvioPuerto=''
                    p=acumulador.find(';',0)
                    h=p+15
                    
                    #[p:h]
                    
                    mtar=acumulador[p:h]
                    ##print(mtar)              
                    if len(mtar)>=15 and TerminoOperacion==1:# and controlluces==0:
                        print("entra tarjeta" + str(mtar))
                        acumulador=''
                        #MuestraTarjeta(mtar)
                        controlluces=0
                        tarjeta=mtar
                         
                        
                    else:
                        #TerminoOperacion=0
                        print("NO entra tarjeta")
                        if len(mtar)>=15:
                            acumulador=''
        if acumulador=='' and PagandoTk==True :

            PagandoTk=False

            b=1
        if PagandoTk==False and int(ctk)>0 and TimeOut==True:
            FinalizoTks()
            #ctk=0
            if ctk==0:
                TerminoPago()
                
        if  acumulador.find('W',0)>-1:
            print('W')
            acumulador=''
            doblev='w'
            if tarjeta=='' and TerminoOperacion==1 and controlluces==1:
                #while EnvioPuerto!='':
                #    time.sleep(0.001)
                #controlluces=0    
                myfont =pygame.font.Font("super.ttf",35)

                bg = pygame.image.load("fondot.jpg")
                screen.blit(bg,(0,0))
                if LabelPase=='Pase Su Tarjeta':
                        TIT='TARJETA INVALIDA'
                        esto="Pase nuevamente su tarjeta"
                else:   
                        TIT='INVALID CARD'
                        esto="Slide your card again"
                
                label = myfont.render(TIT, 1, (0,255,20))
                screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
                myfont =pygame.font.Font("super.ttf",25)
                label1 = myfont.render(esto, 1, (0,255,20))
                screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                                           
                pygame.display.flip() 
                pygame.display.update()
                time.sleep(.100)
                serie.flush()
                serie.write(bytes(bytearray([0x25,0x49,0x00,0x00,0x49,0x00,0x00])))
                time.sleep(1.0)
                serie.flush()
                #time.sleep(0.030)
                #confirmacion=False
                #serie.write(bytes(activot))
                #while confirmacion==False:
                #    b=1
                serie.flush()
                time.sleep(0.100)
                ConfirmacionTarjeta=False
                    
                while ConfirmacionTarjeta==False:
                    serie.write(bytes(activot))
                    time.sleep(0.100)
                #controlluces=1
                #doblev=''
                #acumulador=''
                PantFull()
                #tarjetalista=1
            else:
                #if tarjeta=='':# and controlluces==1:
                    serie.flush()
                    time.sleep(0.500)
                    ConfirmacionTarjeta=False
                    #serie.write(bytes(activot))
                    while ConfirmacionTarjeta==False:
                        serie.write(bytes(activot))
                        time.sleep(0.100)
                        
                        
                     #controlluces=1
                    #doblev=''
                     #acumulador=''
                     #tarjetalista=1
                
        if controlluces==1:# or doblev=='w':# and tarjeta=='' and ctk==0:# and acumulador.find('W',0)==0:
            #serie.flush()
            #confirmacion=False
            #print('mando luz')
            luz2017('paseo')
            #while confirmacion==False:
            #    b=1
            #confirmacion=False
            #serie.flush()
            #serie.write(bytes(activot))
            #while confirmacion==False:
            #    b=1

            

             
def Pantalla():
    #aca esta loop visual
    #pygame.display.update()

    while True:
        #totalpagar=ctk
        for event in pygame.event.get():
            
            if event.type == pygame.QUIT:
                 pygame.mouse.set_visible(1)
                 os.system('setterm -cursor on')
                 pygame.quit()
                 quit()
                 sys.exit()
                 break
            elif event.type == pygame.KEYDOWN:
                ###print(event.key)
                if event.key == 27:
                    pygame.mouse.set_visible(1)
                    os.system('setterm -cursor on')
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
    global controlluces
    global ConfirmacionTarjeta
    while EnvioPuerto!='':
        b=1
    ConfirmacionTarjeta=False
    while ConfirmacionTarjeta==False:
        serie.write(bytes(activot))
        time.sleep(0.200)
            
        
        #EnvioPuerto=      
    PantFull()
    #EnvioPuerto=activot
    controlluces=1
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
    global ultctk
    global EnvioPuerto
    global PantallaPago
    while EnvioPuerto!='':
        b=1
    EnvioPuerto=bytearray([0x30,0x01,0x27])#3r byte es para fisico o electronico, 35 es elec y 60 fisico
    
    if ctk>0 and UltTar!='': 

        try:
                   # Create a TCP/IP socket
                    PantallaPago=1
                    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

                    # Connect the socket to the port where the server is listening
                    server_address = (str(IP),5000)#192.168.2.50 -- IAAPA
                    ##print('connecting to %s port %s' % server_address)
                    sock.settimeout(3.25)
                    sock.connect(server_address)
                    

                    
                
                        # Send data #P'+UltTar[:-1]+{0:4}.format(str(ctk))#NumMaq + tarjeta #tarjeta.get() #'C;1921000005618'
                    ttk2018=str(hex(ctk)[2:]).upper()
                    message = MiMac+','+MiIp+','+NumMaq+'#'+UltTar[:-1] + str(ttk2018).zfill(4) +'99'#NumMaq + tarjeta #tarjeta.get() #'C;1921000005618'#'0055#;47900089307660002F4'
                    #print('sending "%s"' % message)#'{0:04}'.format(str(ctk))
                    #print('{0:04}'.format(str(ctk)))
                    sock.settimeout(1.25)
                    sock.sendall(message.encode(encoding='UTF-8',errors='ignore'))

                    # Look for the response
                    amount_received = 0
                    amount_expected = 1#len(message)
                    data=''
                    yat=datetime.now()
                    hastat=datetime.now()+timedelta(milliseconds=2000)
                    #amount_received < amount_expected and
                    while  hastat > yat and data=='':
                        data = sock.recv(48)
                        #print(data)
                    if data:
                        #ctk=0
                        message='103'
                        sock.sendall(message.encode(encoding='UTF-8',errors='ignore'))
                        
                
                    

        except:
            ##print('error al conectar')
            b=1

        finally:
            #tarjeta=''
            #punto=0
            #haytarjeta=0
            ##print('closing socket')
            sock.close()
            screen.fill((0,0,0))
            bg = pygame.image.load("fondo.jpg")
            screen.blit(bg,(0,0))
            #myfont = pygame.font.SysFont("monospace", 45)
            myfont =pygame.font.Font("super.ttf",45)
            #label = myfont.render("YOU WIN "+ str(ctk) + " Tks!", 1, (0,180,0))
            label = myfont.render(LabelPago, 1, (0,255,23))
            screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
            myfont =pygame.font.Font("super.ttf",70)
            label1 = myfont.render(str(ctk), 1, (0,255,20))
            screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
            pygame.display.flip() 
            pygame.display.update()
            ultctk=0
            ctk=0
            PagandoTk=False
            TimeOut=False
            colorfijo2('28','000','000','100','100')
##    while EnvioPuerto!='':
##        b=1
##    EnvioPuerto=bytearray([0x30,0x01,0x35])
    
            time.sleep(2)
            #ctk=0
            
            while EnvioPuerto!='':
                b=1
            EnvioPuerto=bytearray([0x30,0x01,0x27])
            
            PantFull()
            
            PantallaPago=0 


    


def MuestroPagoTk():
    global contador
    global totalpagar
    global ctk
    global apb
    global confirmacion
    global luz
    global controlluces
    contador=0
    totalpagar=0
    luz=0
    apb=1
    global ultctk
    #pygame.mixer.music.load("/home/pi/Desktop/etk.mp3")
    
    #while pygame.mixer.music.get_busy() == True:
    #    continue
    utlctk=0
    while True: 
            
            if ctk>0 and TimeOut==False and UltTar!='' and PantallaPago==0: #and  and ctk>utlctk:

                if utlctk==0:
                    controlluces=1

                utlctk=ctk
                #pygame.mixer.music.play()
                #time.sleep(1)
                
                #screen.fill(white)
                #if ctk<5:
                screen.fill((0,0,0))
                bg = pygame.image.load("fraspitk.jpg")

                
                screen.blit(bg,(0,0))
                #myfont = pygame.font.SysFont("monospace", 55)
                myfont =pygame.font.Font("super.ttf",42)
                label = myfont.render(LabelPagando, 1, (255,255,255))
                screen.blit(label,(((infoObject.current_w - label.get_width()) / 2),70))
                #myfont = pygame.font.SysFont("monospace", 90)
                myfont =pygame.font.Font("super.ttf",95)
                esto= str(ctk)
                label1 = myfont.render(esto, 1, (0,255,20))
                screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label1.get_height()) / 2)+55))#(label, (((infoObject.current_w - label.get_width()) / 2),160 ))
                pygame.display.flip() 
                pygame.display.update()
                controlluces=0
                if apb==1 :#and controlluces == 1:
                    colorfijo2('24','100','111','100','111')
                     #controlluces=0
                    apb=0
                if apb==0: #and controlluces == 1:
                        #luz=1
                    colorfijo2('24','111','100','111','100')
                    #controlluces=0
                    apb=1


                
                #colorfijo2('25','000','100','000','000')    
                luz=0    
                #time.sleep(1)
                #if ctk<10:
                #    ColorFijo(1,0,0)
            

def PantFull():
                    global EnvioPuerto
                    global ctk
                    global punto
                    global tarjeta
                    global controlluces
                    global TerminoOperacion
                    global ConfirmacionTarjeta
                    global tarjetalista
            
                    #infoObject = pygame.display.Info()
                    #screen=pygame.display.set_mode((infoObject.current_w, infoObject.current_h))
                    screen.fill((0,0,0))
                    #pygame.display.update()
                    bg = pygame.image.load("fraspi1.jpg")
                    #time.sleep(.050)
                    screen.blit(bg,(0,0))
                    




                        # initialize font; must be called after 'pygame.init()' to avoid 'Font not Initialized' error
                    myfont =pygame.font.Font("super.ttf",30) #pygame.font.SysFont("monospace", 35)
                    myfont1 =pygame.font.Font("super.ttf",20)
                    myfont2 =pygame.font.Font("super.ttf",15)

                        # render text
                    label = myfont.render(LabelPase, 1, (255,255,255))
                    screen.blit(label,(((infoObject.current_w - label.get_width()) / 2)-50,40))#(label, (((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))

                    #myfont = pygame.font.SysFont("monospace", 45)
                    myfont =pygame.font.Font("super.ttf",37)
                    label1A = myfont1.render('NORMAL', 1, (0,255,0))
                    screen.blit(label1A,(275,125))
                    label1 = myfont.render(str(PN), 1, (255,255,255))
                    screen.blit(label1,(275,150))
                    label2A = myfont1.render('VIP', 1, (0,255,0))
                    screen.blit(label2A,(275,225))
                    label2 = myfont.render(str(PV), 1, (255,255,255))
                    screen.blit(label2,(275,250))
                    label3 = myfont2.render('www.magneticash.com', 1, (255,255,255))
                    screen.blit(label3,(115,305))
                    QrImg = pygame.image.load('qr.png')
                    screen.blit(QrImg, (35,115))
					
                    pygame.display.flip() 
                    pygame.display.update()

                    #while EnvioPuerto!='':
                    #    b=1
                    
                    #EnvioPuerto=activot

                    #while EnvioPuerto!='':
                    #    b=1
                    #EnvioPuerto==''
                    #tarjeta==''
                    punto==0
                    ctk==0
                    tarjeta=''
                    
                    time.sleep(0.050)
                    serie.flush()
                    ConfirmacionTarjeta=False
                    while ConfirmacionTarjeta==False:
                        serie.write(bytes(activot))
                        time.sleep(0.100)
                    TerminoOperacion=1
                    tarjetalista=1
                    #controlluces=1
                    #while EnvioPuerto!='':
                    #    b=1
                    #colorfijo2('24','000','000','000','000')
                    
                    

def MuestraTarjeta():
    #global confirmacion
    global EnvioPuerto
    global tarjeta
    global punto
    global haytarjeta
    global YaEntre
    global TerminoOperacion
    
    YaEntre=0
    while True:

                    
        if len(tarjeta)==15 and YaEntre==0 and TerminoOperacion==1:
            print('entro a funcion debitar')
            YaEntre=1
            TerminoOperacion=0
            Debitar()
            #time.sleep(4)
        if TerminoOperacion==1 and YaEntre==1:#len(tarjeta)==0
            print('NO entro a funcion debitar')
            YaEntre=0
            #tarjeta=''    
            punto=0    


def MiLuz():
    global apb
    if apb=='':
        apb=0
    while True:
        b=1
        if controlluces==0:#EnvioPuerto==''and tarjeta=='' and punto==0 and ctk==0:
            luz2017('paseo')



                
def EnviarSerie():
    global EnvioPuerto
    global confirmacion
    global ya
    global hasta
    global envie
    global reintento
    global tarjeta
    reintento=0
    envie=0
    hasta=datetime.now()#+timedelta(milliseconds=1000)
    while True:
        ya=datetime.now()
        #
        
        if  confirmacion==True and EnvioPuerto!='' and envie==0: 
            ###print("envio")
            envie=1
            ##print(bytes(EnvioPuerto))
            reintento=0
            hasta=datetime.now()+timedelta(milliseconds=1500)
            confirmacion=False
            serie.write(bytes(EnvioPuerto))
            serie.flushOutput()
            #EnvioPuerto=''
   
        
        if envie==1 and confirmacion==True:
            envie=0
            EnvioPuerto=''
            #tarjeta=''
        
        if confirmacion==True and ya > hasta and  EnvioPuerto!='' and reintento< 4 and envie==1: 
             #confirmacion=True
             #EnvioPuerto=''
             print("reintento placa")
             #time.sleep(1)
             ##print(bytes(EnvioPuerto))
             reintento=reintento+1
             envie=1
             hasta=datetime.now()+timedelta(milliseconds=2000)
             confirmacion=False
             serie.write(bytes(EnvioPuerto))
             serie.flush()#Output()
             #EnvioPuerto=''

        if confirmacion==False and ya > hasta:
            confirmacion=True

        if reintento>=2:
            EnvioPuerto=''
            confirmacion=True
        
#----Config General-----
serie.write(bytes(desactivot))
infoObject = pygame.display.Info()
screen=pygame.display.set_mode((infoObject.current_w, infoObject.current_h))
pygame.mixer.pre_init(44100, -16, 2, 2048) # setup mixer to avoid sound lag
pygame.mixer.init()
pygame.mixer.music.set_volume(0.5)

#pygame.mouse.set_visible(s)
pygame.mouse.set_visible(0)

myfont = pygame.font.SysFont("monospace", 40)
pygame.display.toggle_fullscreen()
#----INICIO 2018 E.E.C
controlluces=1
#hilos de escucha primero 2018
EscuchaConfig=threading.Thread(target=listen)
EscuchaConfig.start()
escuchar=threading.Thread(target=EscuchoPuerto)
escuchar.start()
mserie=threading.Thread(target=PuertoSerie)
mserie.start()
mpantalla=threading.Thread(target=Pantalla)
mpantalla.start()

#Empieza viendo si tiene numero sino esta en modo config
controlluces=1
msj=ObtenerMac('wlan0')
saveQRCodePNG('qrip',3,MiIp) #genero QR con numero de maquina
time.sleep(0.3)
while NumMaq=='XXXX':
      screen.fill((0,0,0))
      pygame.font.init
      myfont =pygame.font.Font("super.ttf",30)

      bg = pygame.image.load("fondot.jpg")
      screen.blit(bg,(0,0))

      esto='Access to ' + MiIp + ' or Scan QRCode'
      label = myfont.render("Configuration Mode", 1, (0,255,20))
      screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-60))
      myfont =pygame.font.Font("super.ttf",20)
      label1 = myfont.render(esto, 1, (255,255,255))
      screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-25))
      msj=subprocess.check_output("iw dev wlan0 link|grep SSID|awk '{print $2}'",shell=True).decode('utf8')
      esto=msj.strip() 
      label2 = myfont.render(esto, 1, (255,255,255))
      screen.blit(label2,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+100))
      QrImg = pygame.image.load('qrip.png')
      screen.blit(QrImg, (185,140))
      
      pygame.display.flip() 
      pygame.display.update()      
      DosMundos()
      time.sleep(1.0)

# Create a TCP/IP socket
#a=ObtenerMac('wlan0')

if PN=='-':
    try:
      #serie.write(bytes([0x25,0x49,0x24,0x92,0x49,0x24,0x97])
      message=str(ObtenerMac('wlan0'))+'$XX'
      while message=='NO$XX': 
        #print('error al conectar')
        serie.write(bytearray(bytes([0x25,0x6D,0xB6,0xDB,0x6D,0xB6,0xDF])))
        balance='Network Error'
        screen.fill((0,0,0))
        pygame.font.init
        myfont =pygame.font.Font("super.ttf",35)

        bg = pygame.image.load("fondot.jpg")
        screen.blit(bg,(0,0))

        esto=balance#data#str(tarjeta)+str(' Ok')
                                            #label = myfont.render(esto, 1, (0,255,20))
                                            #screen.blit(label, (((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))

        label = myfont.render("Network Error", 1, (0,255,20))
        screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
        myfont =pygame.font.Font("super.ttf",22)
        label1 = myfont.render('Check WIFI Connection...', 1, (0,255,20))
        screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                                           
        pygame.display.flip() 
        pygame.display.update()
                            #luz2017('rojo')
        time.sleep(1.5)
        message=str(ObtenerMac('wlan0'))+'$XX'
    except:
            b=1
            
while PN=='-':
    try:
      time.sleep(0.5)
      screen.fill((0,0,0))
      pygame.font.init
      myfont =pygame.font.Font("super.ttf",35)

      bg = pygame.image.load("fondot.jpg")
      screen.blit(bg,(0,0))

      esto=ObtenerMac('wlan0')
      label = myfont.render("Login Machine", 1, (0,255,20))
      screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
      myfont =pygame.font.Font("super.ttf",25)
      label1 = myfont.render(esto, 1, (0,255,16))
      screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                                   
      pygame.display.flip() 
      pygame.display.update()      
      DosMundos()
      sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
      server_address = (str(IP),5000)#192.168.2.50 -- IAAPA
      print('connecting to %s port %s' % server_address)
      sock.settimeout(5.0)
      sock.connect(server_address)
      
      print('paso conexion')
      sock.settimeout(5.0)
      message=MiMac+','+MiIp+','+NumMaq+'$XX'
      print(message)
      sock.sendall(message.encode(encoding='UTF-8',errors='ignore'))
      amount_received = 0
      amount_expected = 1#len(message)
      data1=''
      data=''
      while data=='':
          data = sock.recv(48)
      data=data.decode('utf8')
      NumMaq=data[3:7]
      PN=data[7:14]
      PV=data[14:21]
      print(NumMaq + "," + PN +"," + PV)
      time.sleep(1)
    except:
        time.sleep(.800)
        print("error al iniciar")
#--------------------------
saveQRCodePNG('qr',6,NumMaq) #genero QR con numero de maquina
PantFull()
#programafinal-----------------------------


#HILOS
#listen

TimeOutTk=threading.Thread(target=TiempoTk)
TimeOutTk.start()
tpago=threading.Thread(target=MuestroPagoTk)
tpago.start()
tenvio=threading.Thread(target=EnviarSerie)
tenvio.start()#MuestraTarjeta
btarjeta=threading.Thread(target=MuestraTarjeta)
btarjeta.start()

#
#while EnvioPuerto!='':
#    b=1
#EnvioPuerto=activot
ConfirmacionTarjeta=False
while ConfirmacionTarjeta==False:
    serie.write(bytes(activot))
    time.sleep(0.200)
            
confirmacionMudo=False
while confirmacionMudo==False:
        serie.write(bytes([0x3A]))
        time.sleep(0.100)        
        #EnvioPuerto=
controlluces=1
# Create a TCP/IP socket
                   
#sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

#luces

#luz=threading.Thread(target=MiLuz)#MiLuz
#luz.start()
#------------------------------------------




    

#ventana.mainloop()






