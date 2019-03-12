#!/usr/bin/python3
import threading
#qr
import pyqrcode

from gpiozero import CPUTemperature
from tkinter import *
from tkinter.font import Font
import subprocess
import socket, select
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

global trajepago
global Maximo
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
global punto 
global UsuarioConectado
UsuarioConectado=0
Maximo=0
#global sock

global doblev
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
global ledcual

global ColorVerde
global ColorRojo
global NumMaq
global IP
global PUERTO
global PV
global PN
global WIFIN
global WIFIP
global principal
principal=1
global Reemplazar
global Idioma
global LOCAL
global EMPRESA
LOCAL='0'
EMPRESA='0'
punto=0
#=== DEFINO VARIABLE IDIOMAS---
global LabelConsulta1 #1009
global LabelConsulta2 #1010
global LabelPase #1009 
global LabelPagando #1008
global LabelPago #1017
global LabelPorfavor #1003
global LabelBuscando #1004
global LabelError #1005
global LabelNR #1006
global LabelInvalida #1007
global LabelCoin #1001
global LabelNocoin #1002
global LabelInexistente #1011
global LabelTCPServer #1012
global LabelConexion #1013
global LabelReemplazo #1014
global LabelLogin #1015
global LabelAtencion #1016
global LabelInvalidCard #1018
global LabelInvalidCard2 #1019
global LabelPagoD #1020
global Multiplicador
Multiplicador=1
#---------

Reemplazar=0
ledcual=1
WIFIN=''
WIFIP=''
NumMaq='XXXX'
IP='10.0.0.2'
PUERTO='5000'
MiIp='000.000.000.000'
MiMac='00:00:00:00:00:00'
#-Labels Idiomas son 17!
LabelPase='SWIPE YOUR CARD'
LabelPagando='Paying Tks'
LabelPago='You Win'
LabelConsulta1='Swipe Your Card'
LabelConsulta2='To Check Balance'
LabelConexion='Network Error'
LabelTCPServer='Can not connect to TCPServer'
LabelNR='Repeat Number'
LabelInvalidCard='Swipe Your'
LabelInvalidCard2='Card Again'
LabelPagoD='Disable Pay'
#------------------------
PN='-'
PV='-'
confirmacionMudo=True

##sock.settimeout(1.0)
ConfirmacionTarjeta=False
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
controlluces=0
ultctk=0
doblev=''
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

#LUCES 2018 -----
global leds
global mivueltas
global completovueltas
global hvd
global hvi
hvd=5
hvi=1
completovueltas=1
mivueltas=0
leds=16

##def WifiGeneral():
##        global WIFIN
##        global WIFIP
##        global IPFINAL
##        try:
##                WIFIN='MagneticashAP'
##                WIFIP='5050505050'
##                file=open("/home/pi/ControllerConfig/red.mc",'w')
##                file.write('ctrl_interface=DIR=/var/run/wpa_supplicant GROUP=netdev'+'\n')
##                file.write('update_config=1'+'\n')
##                file.write('country=AR'+'\n')
##                file.write('\n')
##                file.write('network={'+'\n')
##                file.write('    ssid="'+str(WIFIN)+'"'+'\n')
##                file.write('    psk="'+str(WIFIP)+'"'+'\n')
##                file.write('    key_mgmt=WPA-PSK'+'\n')
##                file.write('}')
##                file.flush()
##                file.close()
##        try:
##                IPFINAL=''
##                file=open("/home/pi/ControllerConfig/mip.mc","r")
##                IPFINAL=file.readline()
##                file.close()
##        except:
##                print('error al leer ip final')
##        try:
##                file=open("/home/pi/ControllerConfig/ConWifi.conf","w")
##                file.write('option domain_name_servers, domain_name, domain_search, host_name'+'\n')
##                file.write('option classless_static_routes'+'\n')
##                file.write('option ntp_servers'+'\n')
##                file.write('require dhcp_server_identifier'+'\n')
##                file.write('nohook lookup-hostname'+'\n')
##                file.write('\n')
##                file.write('SSID '+str(WIFIN)+'\n')
##                file.write('static ip_address='+str(IPFINAL)+'\n')
##                file.write('static routers=10.0.0.1'+'\n')
##                file.write('static domain_name_servers=10.0.0.1'+'\n')
##                file.write('static domain_search=8.8.8.8')
##                file.flush()
##                file.close()
##        except: 
##                b=1
##                break
##	
##        while os.path.getsize("/home/pi/ControllerConfig/red.mc")==0:
##                time.sleep(0.5)
##		print('esperando q se guarde')
##                time.sleep(1.5)
##		#ejecuta comando de configuracion wifi - Eze.C 2018
##        try:
##                os.remove("hotspoton.mc")
##        except:
##                b=1
##        os.system("sudo python3 /home/pi/ControllerConfig/configw.py")

def GiroEspecial2018(cc,c16,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,d,vd,vi):
    global confirmacion
    global leds
    global mivueltas
    global completovueltas
    if True:
        if cc=='24':
            a24='00100100'
        if cc=='25':
            a24='00100101'
        if cc=='28':
            a24='00101000' 
        #color='000000000000000000000000000000000000000000000000'
        xx=0
        
        if d=='i' and mivueltas <= vi:                            
            if leds==16:
                color=c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16
                if mivueltas==vd:
                    completovueltas=1
                    mivueltas==0
                else:    
                    mivueltas=mivueltas+1 
            if leds==15:
                color=c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c1
            if leds==14:
                color=c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c1+c2
            if leds==13:
                color=c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c1+c2+c3
            if leds==12:
                color=c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c1+c2+c3+c4
            if leds==11:
                color=c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c1+c2+c3+c4+c5
            if leds==10:
                color=c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c1+c2+c3+c4+c5+c6
            if leds==9:
                color=c8+c9+c10+c11+c12+c13+c14+c15+c16+c1+c2+c3+c4+c5+c6+c7
            if leds==8:
                color=c9+c10+c11+c12+c13+c14+c15+c16+c1+c2+c3+c4+c5+c6+c7+c8
            if leds==7:
                color=c10+c11+c12+c13+c14+c15+c16+c1+c2+c3+c4+c5+c6+c7+c8+c9
            if leds==6:
                color=c11+c12+c13+c14+c15+c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10
            if leds==5:
                color=c12+c13+c14+c15+c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11
            if leds==4:
                color=c13+c14+c15+c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12
            if leds==3:
                color=c14+c15+c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13
            if leds==2:
                color=c15+c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14
            if leds==1:                
                color=c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15
                completovueltas=0    
        elif d=='d' and mivueltas <= vd:
            if leds==1:
                color=c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15
                completovueltas=0
            if leds==2:
                color=c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16
            if leds==3:
                color=c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c1
            if leds==4:
                color=c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c1+c2
            if leds==5:
                color=c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c1+c2+c3
            if leds==6:
                color=c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c1+c2+c3+c4+c5
            if leds==7:
                color=c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c1+c2+c3+c4+c5+c6
            if leds==8:
                color=c8+c9+c10+c11+c12+c13+c14+c15+c16+c1+c2+c3+c4+c5+c6+c7
            if leds==9:
                color=c9+c10+c11+c12+c13+c14+c15+c16+c1+c2+c3+c4+c5+c6+c7+c8
            if leds==10:
                color=c10+c11+c12+c13+c14+c15+c16+c1+c2+c3+c4+c5+c6+c7+c8+c9
            if leds==11:
                color=c11+c12+c13+c14+c15+c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10
            if leds==12:
                color=c12+c13+c14+c15+c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11
            if leds==13:
                color=c13+c14+c15+c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12
            if leds==14:
                color=c14+c15+c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13
            if leds==15:
                color=c15+c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14
            if leds==16:                
                color=c16+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15
                if mivueltas==vd:
                    completovueltas=1
                    mivueltas==0
                else:    
                    mivueltas=mivueltas+1
                
                
            
        #if leds==16:
        #    print(color+str(d))    
        envio=a24+color
        a=hex(int(envio,2))
        a=bytearray.fromhex(a[2:16])
            ####print(a)
        #if confirmacion==True:
            #confirmacion=False
        serie.write(bytes(a))
        serie.flush()
            #confirmacion=True

def Luz2018():
    LL=100
    LH=2
    #global eze
    #global recibi
    global leds
    global mivueltas
    global controlluces
    hacia='d'
    
    print('entro serie1')
    while True:
            #if recibi==1 and eze==1 and leds==16:
            #    recibi=0
            #    if hacia=='d':
            #        envio=[0x31,0x01,0x01]
            #    else:
            #        envio=[0x31,0x01,0x01]
            #    #print(str(envio))
            #    envio=bytearray(envio)
            #    serie.flush()
            #    serie.write(bytes(envio))
                
                
            #if LL<=1:
            #    if LH==1:
            #        LH=3
            #    LL=50
            #    if LH>1:
            #        LH=LH-1
            if controlluces==1  and punto==0 and tarjeta=='' and ctk==0 and TerminoOperacion==1:#if eze==1:# and recibi==0:
                controlluces=0
                serie.flush()
                #serie.write(bytes(a))
                if leds==0:
                    leds=16
                    #time.sleep(0.2)
                    if hacia=='d' and mivueltas==hvd:#and completovueltas==1
                        mivueltas=0
                        hacia='i'
                    elif hacia=='i' and  mivueltas==hvi:#and completovueltas==1
                        mivueltas=0
                        hacia='d'
                        
                GiroEspecial2018('24',
                             '001',
                             '001',
                             '001',
                             '001',
                             '100',
                             '100',
                             '100',
                             '111',
                             '111',
                             '100',
                             '100',
                             '100',
                             '010',
                             '010',
                             '010',
                             '010',hacia,hvd,hvi)
                leds=leds-1

                
             #   if LL>20:
             #       LL=LL-10
             #   else:
             #       LL=LL-1
            #q=serie.read(1).decode('utf8')
        #print(str(q))
            #if q=='U':
            #    recibi=1
            #if q=='V':
            #    eze=1
#----------------

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
    CONNECTION_LIST = []    # list of socket clients
    RECV_BUFFER = 130 # Advisable to keep it as an exponent of 2
    PORT = 5001
         
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    # this has no effect, why ?
    server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_socket.bind(("", PORT))
    server_socket.listen(10)
 
    # Add server socket to the list of readable connections
    CONNECTION_LIST.append(server_socket)
 
    print ("Chat server started on port " + str(PORT))
 
    while 1:
        # Get the list sockets which are ready to be read through select
        read_sockets,write_sockets,error_sockets = select.select(CONNECTION_LIST,[],[])
 
        for sock in read_sockets:
             
            #New connection
            if sock == server_socket:
                # Handle the case in which there is a new connection recieved through server_socket
                sockfd, addr = server_socket.accept()
                CONNECTION_LIST.append(sockfd)
                print ("Client (%s, %s) connected" % addr)
                 
            #Some incoming message from a client
            else:
                # Data recieved from client, process it
                try:
                    #In Windows, sometimes when a TCP program closes abruptly,
                    # a "Connection reset by peer" exception will be thrown
                    data = sock.recv(RECV_BUFFER)
                    # echo back the client message
                    if data:
                        #sock.send('OK ... ' + data)
                        recibiServer(sock,data)
                        print(str(data.decode('utf8')))
                 
                # client disconnected, so remove from socket list
                except:
                    #broadcast_data(sock, "Client (%s, %s) is offline" % addr)
                    print ("Client (%s, %s) is offline" % addr)
                    sock.close()
                    CONNECTION_LIST.remove(sock)
                    continue
         
    server_socket.close()

def listen1():
    global PN
    global PV
    global IP
    global NumMaq
    global tarjeta
    global controlluces
    global EnvioPuerto
    global ConfirmacionTarjeta
    global punto
    global WIFIN
    global WIFIP
    global UsuarioConectado
    global Reemplazar
    global Idioma
    global Multiplicador
    global Maximo
    global principal
    #WIFIN=''
    #WIFIP=''
    try:
        cpu=CPUTemperature()
        connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        connection.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        connection.setblocking(1)
        #connection.settimeout(3)
        connection.bind(("", 5001))
        connection.listen(1)
        print('abre listen')
    except:
        print('no se puede obtener temperatura')
    data1=''
    
    while True:
            
            try:
                paso=0
                sock, address = connection.accept()
                paso=1
                print('acepto a socket')#+ str(sock) )
                while 1:
                    data1= sock.recv(120)
                    if not data1:break
                    recibiServer(sock,data1)
                sock.shutdown(0)    
                sock.close()
                connection.shutdown(0)
                connection.close()
                connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                connection.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                connection.setblocking(1)
                #connection.settimeout(3)
                connection.bind(("", 5001))
                connection.listen(1)
            except:
                
                if paso==1:
                    print('error de socket abierto')
                    sock.close()
                else:
                    print('error de socket cerrado')
                continue
                #os.system('sudo ifconfig wlan0 down')
                #time.sleep(0.25)
                #os.system('sudo ifconfig wlan0 up')
                #EscuchaConfig.stop()
                #connection.shutdown(0)
                #connection.close()
                #connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                #connection.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                #connection.setblocking(0)
                #connection.settimeout(3)
                #connection.bind(("", 5001))
                #connection.listen(1)
                #EscuchaConfig.start()
def recibiServer(current_connection,data):
            global PN
            global PV
            global IP
            global NumMaq
            global tarjeta
            global controlluces
            global EnvioPuerto
            global ConfirmacionTarjeta
            global punto
            global WIFIN
            global WIFIP
            global UsuarioConectado
            global Reemplazar
            global Idioma
            global Multiplicador
            global Maximo
            global principal
            global trajepago
            global LOCAL
            global EMPRESA
            global IPFINAL
            try:
                    if True:
                        
                        #try:
                        #    cpu=CPUTemperature()
                        #except:
                        #    b=1
                        UsuarioConectado=1
                        time.sleep(1.0) #controlluces==1 and
                        #data=''
                        #data1=''
                        #HastaCuando=datetime.now()+timedelta(seconds=5)
                        #Ahora1=datetime.now()
                        if  TerminoOperacion==1:
                            colorfijo2('24','101','101','101','101')
                        if True:
                            #if Ahora1 > HastaCuando:
                            #    break
                            
                            
                            



                            if data:
                                #current_connection.send(data)
                                traje=data.decode('utf8')
                                print(traje)
                                EnvioNo=''
                                if traje[:3]=='lg22:':
                                    if traje[3:]=='1':
                                        #ingles
                                        try:
                                            os.remove('/home/pi/ControllerConfig/lang.mc')
                                            print('archivo de idioma borrado')
                                        except:
                                            print('no se pudo borrar archivo de idioma')
                                            
                                        file=open('/home/pi/ControllerConfig/lang.mc','w')
                                        file.write('1')
                                        file.flush()
                                        file.close()
                                        print('archivo idioma creado ok')
                                        Idioma=1
                                        EnvioNo='Language Ok - '+ str(aaa)
                                        current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                    elif traje[3:]=='2':
                                        #espaniol
                                        try:
                                            os.remove('/home/pi/ControllerConfig/lang.mc')
                                            print('archivo de idioma borrado')
                                        except:
                                            print('no se pudo borrar archivo de idioma')
                                            
                                        file=open('/home/pi/ControllerConfig/lang.mc','w')
                                        file.write('2')
                                        file.flush()
                                        file.close()
                                        print('archivo idioma creado ok')
                                        Idioma=2
                                        EnvioNo='Lenguaje Ok - '+ str(aaa)
                                        current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                elif traje[:9]=='wificlear':
                                        WIFIN=''
                                        WIFIP=''
                                elif traje[:9]=='desktopin':
                                        os.system("sudo cp /home/pi/.config/lxsession/LXDE-pi/autostart1 /home/pi/.config/lxsession/LXDE-pi/autostart")
                                        EnvioNo='ejecutando con escritorio'
                                        current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                        os.system("reboot")
                                elif traje[:10]=='desktopout':
                                        os.system("sudo cp /home/pi/.config/lxsession/LXDE-pi/autostart2 /home/pi/.config/lxsession/LXDE-pi/autostart")#"")
                                        EnvioNo='ejecutando sin escritorio'
                                        current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                        os.system("reboot")
                                elif traje[:7]=='restart':
                                        os.system("reboot")
                                elif traje[:9]=='replace:1':
                                        Reemplazar=1
                                elif traje[:7]=='pconfig':
                                    if int(NumMaq,16) > int('000F',16):
                                        try:
                                            pcorto=bytearray()#[0x29,0x10])#,0x00,0x00,0x00,0x00,0x00])#pago rapido-coin rapido
                                            aaa=str(traje[7:9]) + ' ' + str(traje[10:12])
                                            trajepago=traje[7:12]
                                            pcorto.append(0x30)
                                            pcorto.append(int(traje[7:9],16))
                                            pcorto.append(int(traje[10:12],16))
                                            #pcorto.append(0x01)
                                            serie.write(pcorto)
                                            serie.flushOutput()
                                            try:
                                                os.remove('/home/pi/ControllerConfig/pconf.mc')
                                                print('archivo de config borrado')
                                            except:
                                                print('no se pudo borrar archivo de configuracion')
                                                
                                            file=open('/home/pi/ControllerConfig/pconf.mc','w')
                                            file.write(traje)
                                            file.flush()
                                            file.close()
                                            print('archivo pago creado ok')
                                            EnvioNo='OK'
                                            current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                            if PN!="-":
                                            #if LOCAL=='0' and EMPRESA=='0':
                                                saveQRCodePNG('qr',4,NumMaq+LOCAL+'#'+EMPRESA+'@'+IPFINAL+'*'+ trajepago) #genero QR con numero de maquina
                                                PantFull()
                                            #else:
                                            #    saveQRCodePNG('qr',4,NumMaq+LOCAL+'#'+EMPRESA+'@'+IPFINAL+'*'+trajepago) #genero QR con numero de maquina
                                        except:
                                            EnvioNo='NO'
                                            current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                elif traje[len(MiMac)+len(MiIp)+2:len(MiMac)+len(MiIp)+2+3]=='300':
                                        
                                    controlluces=0
                                    EnvioPuerto=''
                            
                                    serie.flush()
                                    time.sleep(0.100)
                                    #ConfirmacionTarjeta=False
                                    #while ConfirmacionTarjeta==False:
                                    #    serie.write(bytes(activot))
                                    #    time.sleep(0.100)
                                    #tarjeta=traje[7:21]+'?'
                                    print('llego 300 ' + str(tarjeta))
                                    while principal==0 or ctk>0:
                                        b=1
                                    punto=1
                                    #principal=0
                                    EnvioNo='103'
                                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                    tarjeta=traje[7+len(MiMac)+len(MiIp)+2:21+len(MiMac)+len(MiIp)+2]+'?'
                                    print('llego celu '+ str(tarjeta))
                                    #time.sleep(0.8)
                                    
                                    print('armo tarjeta '+ tarjeta)
                                elif traje[:3]=='num':
                                    time.sleep(0.2)
                                    try:
                                        a=traje[3:7].strip()
                                        print('num ->' + str(a))
                                        if len(a)!=4:
                                            EnvioNo='NO' 
                                            current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                        
                                            numerror=1
                                        else: 
                                            #if b < hex(int(10)):
                                            a=int(traje[3:7],16)
                                            numerror=0
                                    except:
                                        EnvioNo='NO' 
                                        current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                        
                                        numerror=1
                                    if numerror==0:
                                        try:
                                                #os.remove('/home/pi/ControllerConfig/num.mc')
                                                #print('archivo num borrado ok')
                                                NumMaq=traje[3:7]
                                                file=open('/home/pi/ControllerConfig/num.mc','w')
                                                file.write(NumMaq)
                                                file.flush()
                                                file.close()
                                                print('archivo num creado ok')
                                                EnvioNo='OK'
                                                current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                                if PN !='-':
                                                    os.system("reboot")
                                        except:
                                                NumMaq=traje[3:7]
                                                os.remove('/home/pi/ControllerConfig/num.mc')
                                                print('archivo num borrado ok')
                                                file=open('/home/pi/ControllerConfig/num.mc','w')
                                                file.write(NumMaq)
                                                file.flush()
                                                file.close()
                                                print('archivo num creado ok')
                                                EnvioNo='OK'
                                                current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                            #time.sleep(3.0)
                                elif traje[:5]=='ssid:':
                                        if NumMaq=='' or NumMaq=='XXXX':
                                                EnvioNo='Enter a Valid Number Machine for this Controller before Network config \r\n'
                                                current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                                #break
                                        else:
                                            WIFIN=traje[5:]
                                            EnvioNo='OK'
                                            current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                            #if WIFIP=='':
                                            #        EnvioNo='Enter password for network '+ str(WIFIN) + '\r\n'
                                            #        current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                                
                                elif traje[:5]=='pass:':
                                        #if NumMaq=='' or NumMaq=='XXXX':
                                        #        EnvioNo='Enter a Valid Number Machine for this Controller before Network config \r\n'
                                        #        current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                                #break
                                        #if WIFIN=='':
                                        #        EnvioNo='Write a valid SSID Network before Password \r\n'
                                        #        current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                                #break
                                        #else:    
                                            WIFIP=traje[5:]
                                            EnvioNo='OK'
                                            current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                            try:
                                                    WIFIN=WIFIN.strip()
                                                    WIFIP=WIFIP.strip()
                                                    file=open("/home/pi/ControllerConfig/red.mc",'w')
                                                    file.write('ctrl_interface=DIR=/var/run/wpa_supplicant GROUP=netdev'+'\n')
                                                    file.write('update_config=1'+'\n')
                                                    file.write('country=AR'+'\n')
                                                    file.write('\n')
                                                    file.write('network={'+'\n')
                                                    file.write('    ssid="'+str(WIFIN)+'"'+'\n')
                                                    file.write('    psk="'+str(WIFIP)+'"'+'\n')
                                                    file.write('    key_mgmt=WPA-PSK'+'\n')
                                                    file.write('}')
                                                    file.flush()
                                                    file.close()

                                                    try:
                                                            IPFINAL=''
                                                            file=open("/home/pi/ControllerConfig/mip.mc","r")
                                                            IPFINAL=file.readline()
                                                            file.close()
                                                    except:
                                                            print('error al leer ip final')

                                                    file=open("/home/pi/ControllerConfig/ConWifi.conf","w")
                                                    file.write('option domain_name_servers, domain_name, domain_search, host_name'+'\n')
                                                    file.write('option classless_static_routes'+'\n')
                                                    file.write('option ntp_servers'+'\n')
                                                    file.write('require dhcp_server_identifier'+'\n')
                                                    file.write('nohook lookup-hostname'+'\n')
                                                    file.write('\n')
                                                    file.write('SSID '+str(WIFIN)+'\n')
                                                    file.write('static ip_address='+str(IPFINAL)+'\n')
                                                    file.write('static routers=10.0.0.1'+'\n')
                                                    file.write('static domain_name_servers=10.0.0.1'+'\n')
                                                    file.write('static domain_search=8.8.8.8')
                                                    file.flush()
                                                    file.close()
                                                    erw=0
                                            except: 
                                                    EnvioNo='NO'
                                                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                                    erw=1
                                                    #break
                                            

                                            if erw==0:        
                                                while os.path.getsize("/home/pi/ControllerConfig/red.mc")==0:
                                                        time.sleep(0.5)
                                                        print('esperando q se guarde')
                                                EnvioNo='Please Wait, Restarting Controller ... \r\n'
                                                current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                                time.sleep(1.5)
                                                #ejecuta comando de configuracion wifi - Eze.C 2018
                                                try:
                                                        os.remove("hotspoton.mc")
                                                except:
                                                        b=1
                                                os.system("sudo python3 /home/pi/ControllerConfig/configw.py")
                                        
                                elif traje[:3]=='log':
                                    EnvioNo='Controller: My Mac is '+str(MiMac)+ '\r\n'
                                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                    EnvioNo='Controller: My Ip is '+str(MiIp)+ '\r\n'
                                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                    #EnvioNo='Controller: My Temperature is '+str(cpu.temperature)+ '\r\n'
                                    #current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                    EnvioNo='Controller: Master Ip is '+str(IP)+ '\r\n'
                                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                    EnvioNo='Controller: MaqNumber is '+str(NumMaq)+ '\r\n'
                                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                    EnvioNo='Controller: Normal Price is '+str(PN)+ '\r\n'
                                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                    EnvioNo='Controller: Vip Price is '+str(PV)+ '\r\n'
                                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                    EnvioNo='Controller: Connected to '+str(WIFIN)+ '\r\n'
                                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))

                                    #time.sleep(10.0)
                                    #return
                                elif traje[:8]=='masterip':
                                    EnvioNo='Controller: Master Ip is '+str(IP)+ '\r\n'
                                    current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                    #IP=traje[6:]
                                    #print(str(IP))
                                    #EnvioNo='Controller: Master IP Changed OK to '+str(IP) 
                                    #current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                    #time.sleep(5.0)
                                    #return
                                elif traje[:6]=='master':
                                    #EnvioNo='Controller: Prev Ip '+str(IP)+ '\r\n'
                                    #current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                    IP=traje[6:]
                                    print(str(IP))
                                    try:
                                        os.remove("/home/pi/ControllerConfig/hip.mc")
                                    except:
                                        print('No existe archivo hip')
                                    finally:
                                        file=open("/home/pi/ControllerConfig/hip.mc","w")
                                        file.write(str(IP))
                                        file.flush()
                                        file.close()
                                        EnvioNo='OK'
                                        current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                        #time.sleep(5.0)
                                        #return
                                elif traje[:4]=='wifi':
                                  
                                  EnvioNo='Preparando Conexion Wifi...'
                                  current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                  os.system("sudo python3 /home/pi/ControllerConfig/configw.py")
                                  #current_connection.close()
                        
                                elif traje[:7]=='hotspot':
                                  
                                  EnvioNo='Preparando Hotspot Wifi...'
                                  current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                  file=open("hotspoton.mc","w")
                                  file.flush()
                                  file.close()
                                  os.system("sudo python3 /home/pi/ControllerConfig/configh.py")
                                  #current_connection.close()

                                elif traje[0:17]==MiMac:
                                  time.sleep(0.2)
                                  serie.flush()
                                  serie.write(bytes(bytearray([0x3C])))
                                  pygame.mixer.music.set_volume(0.20)
                                  pygame.mixer.music.load('/home/pi/actualizacion.mp3')
                                  pygame.mixer.music.play()
                                  print('entro actualizacion')
                                  print(str(traje))
                                  #NumMaq=traje[21:25]
                                  aa=traje.find(',')+1
                                  b=traje.find(',',aa)
                                  c=traje.find(',',b+1)
                                  EMPRESA=traje[18:b]#recibo empresa
                                  LOCAL=traje[b+1:c]#recibo local
                                  sumar=len(EMPRESA)+len(LOCAL)+2
                                  if NumMaq==traje[(21+sumar):(25+sumar)]:
                                      PN=traje[(25+sumar):(32+sumar)]
                                      PV=traje[(32+sumar):(39+sumar)]
                                      Pulso=traje[(39+sumar):(40+sumar)]
                                      Multiplicador=str(traje[(40+sumar):(47+sumar)])
                                      Multiplicador=int(float(Multiplicador.strip()))
                                      print(str(Multiplicador))
                                      Maximo=str(traje[(47+sumar):(54+sumar)])
                                      Maximo=int(float(Maximo.strip()))
                                      print(str(Maximo))
                                      if controlluces==1 and TerminoOperacion==1:
                                          colorfijo2('24','111','111','111','111')
                                          PantFull()
                                      controlluces=1
                                      EnvioNo='103'
                                      current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                      time.sleep(5.0)
                                      pygame.mixer.music.set_volume(0.90)
                                      serie.flush()
                                      serie.write(bytes(bytearray([0x3A])))
                                      
                                  
                                else:
                                  if EnvioNo=='':                        
                                          EnvioNo='NO'#str(ObtenerMac('wlan0'))
                                          current_connection.send(EnvioNo.encode(encoding='UTF-8',errors='ignore'))
                                  
                                time.sleep(0.001)
                            else:
                                    try:
                                        
                                        if not Data:#UsuarioConectado==1:
                                            UsuarioConectado=0
                                            #current_connection.close()
                                            #connection.close()
                                    except:
                                        b=1
                                    UsuarioConectado=0
                                    #break
                            if controlluces==0:
                                controlluces=1
                        if Ahora1>HastaCuando:
                            print('salio timeout listen')
                            UsuarioConectado=0
                            #current_connection.close()
                            controlluces=1
            except:
                    #try:
                        #try:
                        #    #current_connection.close()
                        #except:
                        #    b=1
                        #try:
                        #    connection.close()
                        #except:
                        #    b=1
                        
                        UsuarioConectado=0
                        print('NO abre listen')
                        #if controlluces==0:
                        controlluces=1
                    #except:
                        #UsuarioConectado=0#
                        #print('NO abre listen 2')
                        #controlluces=1
                        #b=1


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
    #MiIp=ipaddr
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
    global PagandoTk
    global TimeOut
    global LabelConsulta1
    global LabelConsulta2
    global punto
    global principal
    principal=0
    #LabelConsulta1=''
    #LabelConsulta2=''
    ConfirmacionCoin=0
    ConfirmacionTarjeta=False
    print('llego con tarjeta '+str(tarjeta))
    if True:#tarjeta !='':# and TerminoOperacion==1:
        #print(tarjeta)
        tarjeta2018=tarjeta
        print(tarjeta2018)
        #tarjeta==''
        balance=''
        treintento=0
        data=''
        buscartarjeta=1
        #while ctk>0:
        #    time.sleep(0.025)
        if True:#while treintento<11 and len(data)<46: #reintenta si no recibe respuesta del tcpserver
            try:
                   # Create a TCP/IP socket
                    while len(data)==0 and reintento<= 10 and buscartarjeta==1:
                        try:
                            print('previo a conectar')
                            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                            server_address = (str(IP),5000)#192.168.2.50 -- IAAPA
                            print('connecting to %s port %s' % server_address)
                            sock.settimeout(1.0)
                            sock.connect(server_address)
                            message = str(MiMac+','+IPFINAL+','+NumMaq +'*'+tarjeta2018[:-1] + str(PN) + str(PV) + '99') #tarjeta.get() #'C;1921000005618'
                            

                            # Connect the socket to the port where the server is listening

                            print('paso conexion')
                            
                            message=str(message)
                            sock.send(message.encode())#.encode(encoding='UTF-8',errors='ignore'))

                            data1=''
                            data=''
                            
                            print('antes de leer')
                            while data=='':#hastat > yat and data=='':
                                data = sock.recv(100)
                            print('leyo')
                            data1=data.decode('utf8')
                            if data1[:5]=='9000F':
                                buscartarjeta=1
                                serie.flush()
                                serie.write(bytearray(bytes([0x28,0x01,0x00,0x01,0x00,0x01,0x00])))
                                
                                balance=LabelPorfavor
                                
                                screen.fill((0,0,0))
                                pygame.font.init
                                myfont =pygame.font.Font("super.ttf",35)

                                bg = pygame.image.load("fondot.jpg")
                                screen.blit(bg,(0,0))

                                esto=balance#data#str(tarjeta)+str(' Ok')
                                
                                label = myfont.render(LabelBuscando, 1, (0,255,20))
                                
                                screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
                                myfont =pygame.font.Font("super.ttf",25)
                                label1 = myfont.render(esto, 1, (0,255,20))
                                screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                                pygame.display.flip()
                                time.sleep(1.0)
                                serie.flush()
                                serie.write(bytearray(bytes([0x28,0x01,0x00,0x01,0x00,0x01,0x00])))
                                time.sleep(1.0)
                                data=''
                                data1=''
                                #hex('a')#para que salte al error
                    
                            print(str(data1))
                            print("paseeee----")
                        except:
                            if data=='':
                                treintento=2
                                hex('hola')
                            if treintento==10:
                                print('error al conectar')
                                serie.write(bytearray(bytes([0x25,0x6D,0xB6,0xDB,0x6D,0xB6,0xDF])))
                                balance=LabelError
                                screen.fill((0,0,0))
                                pygame.font.init
                                myfont =pygame.font.Font("super.ttf",35)

                                bg = pygame.image.load("fondot.jpg")
                                screen.blit(bg,(0,0))

                                esto=balance#data#str(tarjeta)+str(' Ok')
                                label = myfont.render(LabelError, 1, (0,255,20))
                                screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
                                myfont =pygame.font.Font("super.ttf",25)
                                label1 = myfont.render(esto, 1, (0,255,20))
                                screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                                pygame.display.flip() 
                                time.sleep(1)
                                b=1
                                break
                            treintento=treintento+1
                    ##print("error al iniciar")
                    if len(data1)==48:
                        buscartarjeta=0
                    if data1=='999':
                              colorfijo2('25','010','010','010','111')
                              os.system("reboot")
                              time.sleep(5.0)
                              screen.fill((0,0,0))
                              pygame.font.init
                              myfont =pygame.font.Font("super.ttf",25)
                              myfont1 =pygame.font.Font("super.ttf",10)
                              bg = pygame.image.load("fondot.jpg")
                              screen.blit(bg,(0,0))
                              if Idioma==2:
                                  esto="Disable Game!!!"
                              else:
                                  esto="Juego Deshabilitado!!!"
                              label2 = myfont.render(esto, 1, (255,50,50))
                              screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))
                              #'TCP Server'
                              #if estamaquina==0:
                              #    esto="Check TCP SERVER APP"
                              #else:
                              
                              esto=LabelNR
                              
                              
                              label2 = myfont.render(esto, 1, (255,255,255))
                              screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+30))
                              #msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
                              #MiMac1=msj.strip()
                              #MiMac1=MiMac1[0:2]+MiMac1[3:5]+MiMac1[6:8]+MiMac1[9:11]+MiMac1[12:14]+MiMac1[15:17]
                              label4 = myfont1.render(LabelReemplazo, 1, (255,255,255))
                              screen.blit(label4,(((infoObject.current_w - label4.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+135))
                              label5 = myfont1.render('replace:1', 1, (255,255,255))
                              screen.blit(label5,(((infoObject.current_w - label5.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+155))
                              saveQRCodePNG('qrip',2,str(IPFINAL))
                              QrImg = pygame.image.load('qrip.png')
                              screen.blit(QrImg, (1,245))
                              pygame.display.flip()
                        

                    elif len(data1)<46 and len(data1)>5 and buscartarjeta==0:
                    
                            balance=LabelInvalida
                            screen.fill((0,0,0))
                            pygame.font.init
                            myfont =pygame.font.Font("super.ttf",35)

                            bg = pygame.image.load("fondot.jpg")
                            screen.blit(bg,(0,0))

                            esto=balance

                            label = myfont.render(LabelError, 1, (0,255,20))
                            screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
                            myfont =pygame.font.Font("super.ttf",25)
                            label1 = myfont.render(esto, 1, (0,255,20))
                            screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                            if int(ctk)==0:
                                pygame.display.flip() 
                            
                            luz2017('rojo')
                            time.sleep(1)
                            
                    elif data1[1:2]=='0' and buscartarjeta==0:#data1=='sin' or data1=='SIN':
                            
                            print("Entro a sin saldo")
                            serie.flush()
                            EnvioPuerto=''
                            
                            if len(data1)>46:
                            
                                balance=data1
                                mok='103'
                                mok=str(mok)
                                sock.sendall(mok.encode(encoding='UTF-8',errors='ignore'))
                            
                                confirmacionMudo=False
                                while confirmacionMudo==False:
                                    serie.flush()
                                    serie.write(bytes([0x3C]))
                                    time.sleep(0.150)
                                
                                pygame.mixer.music.load('/home/pi/rojo.mp3')
                                
                                pygame.mixer.music.play()
                                controlluces=0
                                t1=0
                                serie.write(bytes(ColorRojo))
                                while controlluces==0:
                                        if t1==0 or t1==10:
                                                serie.write(bytes(ColorRojo))
                                                t1=0
                                        time.sleep(0.080)
                                        t1=t1+1
                                        
                            screen.fill((0,0,0))
                            pygame.font.init
                            myfont =pygame.font.Font("super.ttf",30)

                            bg = pygame.image.load("fondot.jpg")
                            screen.blit(bg,(0,0))
                            balance=data1[14:-2]#.lstrip()
                            print(data1)

                            esto=balance#data#str(tarjeta)+str(' Ok')
                            
                            esto1=balance#[2:].lstrip()
                            
                            esto1=esto1.rstrip()
                            print(esto1)
                            
                            print(esto+' Eze')
                            
                            label = myfont.render(LabelNocoin, 1, (255,255,255))
                            linea1=esto[:16]#.lstrip()
                            linea2=esto[16:]#.lstrip()
                                                       
                            
                            
                            if len(linea2)<len(linea1):
                                while len(linea2)<len(linea1):
                                        linea2=linea2 + ' '
                            
                            
                            
                            
                            screen.blit(label,(((infoObject.current_w - label.get_width()) / 2)-50, ((infoObject.current_h - label.get_height()) / 2)-115))
                            myfont =pygame.font.Font("super.ttf",30)
                            label1 = myfont.render(linea1, 1, (0,255,0))
                            screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-30))
                            myfont =pygame.font.Font("super.ttf",30)
                            label1 = myfont.render(linea2, 1, (0,255,0))
                            screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+15))                            
                            
                            if int(ctk)==0:                           
                                pygame.display.flip() 
                            
                            serie.flush()

                            
                                
                    elif data1[1:2]=='1':#len(data1)>10:
                            UltTar=tarjeta2018
                            print("Entro con saldo")

                            if int(NumMaq,16) > int('000F',16):
                                confirmacionMudo=False
                                while confirmacionMudo==False:
                                        serie.flush()
                                        serie.write(bytes([0x3C]))
                                        time.sleep(0.100)
                            
                            
                            ee=bytearray()
                            ee.append(0x2A)
                            ee.append(0x92)
                            ee.append(0x49)
                            ee.append(0x24)
                            ee.append(0x92)
                            ee.append(0x49)
                            ee.append(0x27)
                            serie.flush()
                            
                            print("Playing...")
                            if len(data1)>46:
                                #sock.connect(server_address)
                                mok='103'
                                mok=str(mok)
                                sock.sendall(mok.encode(encoding='UTF-8',errors='ignore'))
                                ConfirmacionCoin=0
                                t1=0
                                serie.flush()
                                
                                #serie.write(bytes(ee))
                                if int(NumMaq,16) > int('000F',16):
                                    
                                    pygame.mixer.music.load('/home/pi/coin.mp3')
                                    
                                    pygame.mixer.music.play()
                                serie.flush()
                                serie.write(bytes(ee))
                                while ConfirmacionCoin==0:
                                    print("EnviandoCoin")
                                    if t1==9:
                                        serie.write(bytes(ee))
                                        t1=0
                                    time.sleep(0.100)
                                    t1=t1+1
                                balance=data1
                                


                            
                            
                                tarjetalista=0
                                #time.sleep(0.1)


                            

                            EnvioPuerto=''

                            
                            print('ya')

                            print(data1)
                            balance=data1[14:-2]#.lstrip()
                            print(balance)

                            screen.fill((0,0,0))
                            pygame.font.init
                            myfont =pygame.font.Font("super.ttf",30)

                            bg = pygame.image.load("fondot.jpg")
                            screen.blit(bg,(0,0))

                            esto=balance

                            print('paso labels')
                            if int(NumMaq,16) > int('000F',16):
                                
                                label = myfont.render(LabelCoin, 1, (255,255,255))
                                
                            else:
                                label = myfont.render("", 1, (255,255,255))    

                            linea1=esto[:16]
                            linea2=esto[16:]
                            linea3='Tickets: '+ str(int(data1[10:14]))
                            
                            if len(linea2)<len(linea1):
                                while len(linea2)<len(linea1):
                                        linea2=linea2 + ' '
                            if len(linea3)<16:
                                q=0
                                while len(linea3)<16:
                                        if q==0:
                                            linea3= ' ' + linea3
                                            q=1
                                        else:
                                            linea3= linea3 + ' '
                                            q=0
                            
                            
                            serie.flush()
                            
                            screen.blit(label,(((infoObject.current_w - label.get_width()) / 2)-50, ((infoObject.current_h - label.get_height()) / 2)-115))
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
                            
                                
                                
                                



            except:
                if treintento==2:
                    print('error al conectar')
                    serie.write(bytearray(bytes([0x25,0x6D,0xB6,0xDB,0x6D,0xB6,0xDF])))
                    balance=LabelError
                    screen.fill((0,0,0))
                    pygame.font.init
                    myfont =pygame.font.Font("super.ttf",35)

                    bg = pygame.image.load("fondot.jpg")
                    screen.blit(bg,(0,0))

                    esto=balance#data#str(tarjeta)+str(' Ok')
                    label = myfont.render(LabelError, 1, (0,255,20))
                    screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
                    myfont =pygame.font.Font("super.ttf",25)
                    label1 = myfont.render(esto, 1, (0,255,20))
                    screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                    pygame.display.flip() 
                    time.sleep(1)
                    b=1
                treintento=treintento+1

        if True:
            #tarjeta=''
            #punto=0
            
            print('closing socket')
            sock.close()
            

            #confirmacion=False
            #while confirmacion==False:
            #    serie.flush()
            #    serie.write(pcorto)
            #    time.sleep(0.050)
            time.sleep(0.5)
            #EnvioPuerto=activot    
            #TerminoOperacion=1
            
            
            EnvioPuerto=''
            
            
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
            
            
            
            controlluces=1
            #TerminoOperacion=1
            #----
            #confirmacionMudo=False
            
            #-----
            print("termino")
            
            #if ctk==0:

            if ctk==0:#True:#int(ctk)==0:#if int(ultctk) == int(ctk):
                controlluces=1
                time.sleep(2.5)
                serie.flush()
                serie.write(bytes([0x3A]))#mudo
                time.sleep(0.100)
                #TimeOut=False
                PantallaPago=0
                punto=0
                PantFull()
            else:
                controlluces=1
                punto=0
                #time.sleep(1.0)
                #serie.flush()
                #serie.write(bytes([0x3A]))#mudo
                #time.sleep(0.100)
                #TimeOut=False
                #PantallaPago=0
                PantFull()
            #    PagandoTk=False
            #    TimeOut=False                
            #    TerminoOperacion=1
            
            
            
            #while confirmacionMudo==False:

            #    time.sleep(0.150)
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
    
    
    

serie = serial.Serial('/dev/serial0', 9600, timeout=0.5, writeTimeout=0)
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

#pcorto=bytearray()#[0x29,0x10])#,0x00,0x00,0x00,0x00,0x00])#pago rapido-coin rapido
#pcorto.append(0x29)
#pcorto.append(0x02)
#serie.write(pcorto)
#serie.flushOutput()
#time.sleep(1)
#pcorto1=bytearray()#[0x30,0x01,0x35])#,0x00,0x00,0x00,0x00]) #habilita pago
#pcorto1.append(0x30)
#pcorto1.append(0x01)
#pcorto1.append(0x27)#35 electronico // 60 fisico
 #   pcorto.append(0x10)
#serie.write(pcorto1)
#serie.flushOutput()
#time.sleep(1)
#serie.write(activot)
#serie.flushOutput()
#time.sleep(1)

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

#serie.write(ee)
#time.sleep(1)
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
    serie.flush()
    if apb=='':
        apb=0
    if cual=='paseo22':
        while EnvioPuerto!='' :
            b=1
        if punto==0: #and tarjeta=='':
            ###print("yo1")
            #EnvioPuerto=bytearray([0x25,0x11,0x11,0x11,0x11,0x11,0x11])#bytes(a)
            EnvioPuerto=ColorRojo#ColorRojo#bytearray([0x25,0xFF,0xFF,0xFF,0xFF,0xFF,0xF8])#bytes(a)
    if cual=='paseo':
        #while EnvioPuerto!='':
        #    b=1
        if True:#TerminoOperacion==1 and punto==0 and tarjeta=='' and ctk==0:#tarjeta=='' :#punto==0 and tarjeta=='' and ctk==0: and EnvioPuerto==''
            #print("paseo")
            controlluces=0
            #%$\x99\'\xf2D\x92
            a=bytearray([0x25,0x49,0x29,0x27,0xF2,0x42,0x49])#bytearray([0x28,0x10,0x11,0x01,0x02,0x05,0x09])#'%$\x99\'\xf2D\x92'
            #a=bytearray([0x25,0x09,0x00,0x09,0x00,0x49,0xF8])#bytearray([0x28,0x10,0x11,0x01,0x02,0x05,0x09])#bytes(a)
            serie.write(bytes(a))
            #time.sleep(0.5)
            #controlluces=1
            return
            #time.sleep(1.5)
    if cual=='paseo2':
        if True:#TerminoOperacion==1 and punto==0 and tarjeta=='' and ctk==0:#tarjeta=='' :#punto==0 and tarjeta=='' and ctk==0: and EnvioPuerto==''
            #print("paseo2")
            controlluces=0
            #a='2$\xc9?\x92$\x91'
            a=bytearray([0x32,0x49,0x29,0x27,0xF2,0x42,0x49])
            #a=bytearray([0x32,0x09,0x00,0x09,0x00,0x49,0xF8])#bytearray([0x28,0x10,0x11,0x01,0x02,0x05,0x09])#bytes(a)
            serie.write(bytes(a))
            #time.sleep(0.5)
            #controlluces=1
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
        time.sleep(0.080)
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
    global principal
    global doblev
    global ultctk
    #global screen
    tarjeta=''
    EntrarTimer=False
    doblev=''
    
    apb=0
    acumulador=''
    recibi=''
    luz='no'
    ContadorReset=0
    while True:
        try:
            time.sleep(0.001)
            q=serie.read(1)
            a=q.decode('utf-8')
            #serie.flushInput()    
            if len(a)>0:
                #print(a)
                recibi=recibi+a
                if recibi.find('W',0)>-1:
                    #punto=1
                    
                    #serie.flush()
                    #serie.write=bytes(activot)
                    doblev='W'
                    #controlluces=1
                    #TerminoOperacion=1
                        
            if TerminoOperacion==1:
                acumulador=acumulador+a
            #else:
            #    if controlluces==1:
            #        luz2017('paseo')
            if a=='P':
                ContadorReset=ContadorReset+1
                if ContadorReset==10:
                        try:
                                #time.sleep(1.0)
                                #ColorFijo2('28','001','001','001','100')
                                os.remove("/home/pi/ControllerConfig/num.mc")
                                os.remove("/home/pi/ControllerConfig/red.mc")
                                os.remove("/home/pi/ControllerConfig/ConWifi.conf")
                                os.remove("/home/pi/hotspot.mc")
                                #os.system("sudo reboot")
                                file=open("hotspoton.mc","w")
                                file.flush()
                                file.close()
                                os.system("sudo python3 /home/pi/ControllerConfig/configh.py")
                        except:
                                 print('error to reset factory')
                                 ContadorReset=0
            if a=='Z':
                ConfirmacionTarjeta=True
                controlluces=1#prueba para q no cuelguen las luces
            if a=='Y':
                ConfirmacionCoin=1
                serie.flush()
                serie.write=bytes(activot)
            if a=='X':
                
                tarjetalista=1
            if a=='V':# and ctk==0:
                b=1
                controlluces=1
                #controlluces=1
                #if doblev=='W':
                #    doblev=''
            if a=='U':
                confirmacion=True
            if a=='T':
                confirmacionMudo=True
                print('Mudo Ok')
            if  recibi.find('U',0)>-1:
                ###print("confirmo")
                confirmacion=True
                #EnvioPuerto=''
            if  recibi.find('V',0)>-1:
                controlluces=1
            if  recibi.find('X',0)>-1:
                tarjetalista=1
                #return
##            if a==";" and tarjeta=='':
##                serie.flushOutput()
##                #controlluces=0
##                if punto==0:
##                    #EnvioPuerto=''
##                    #controlluces=1
##                    #while EnvioPuerto!='':
##                    b=1
##                    #EnvioPuerto=desactivot
##                    #while EnvioPuerto!='':
##                    #    b=1
##                #serie.flushOutput()
##                #EnvioPuerto=''
##                #serie.write(bytes(desactivot))
##                punto=1
            
            ar=recibi.find('@',0)
            if ar>-1 and UltTar!='':
                 
                 if PagandoTk==False:
                     #if confirmacion==True:
                     acumulador=''
                     if ctk>=0:
                         #confirmacionMudo=False
                         pygame.mixer.music.load('/home/pi/pagotk.mp3')
                         time.sleep(0.020)
                     #time.sleep(0.020)
                     #EnvioPuerto=desactivot
                     confirmacion=False
                     if confirmacion==False:
                         serie.flush()
                         serie.write(bytes(desactivot))
                         time.sleep(0.050)
                     
                     #if int(ctk)<5:
                     #    a=bytearray([0x24,0xDB,0x6D,0xB6,0xDB,0x6D,0xB6])
                     #    serie.write(bytes(a))
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
                 
                 #if True:
                 #    if confirmacionMudo==False:
                 #        serie.flush()
                  #       serie.write(bytes([0x3C]))
                 pygame.mixer.music.play()
                 #clock = pygame.time.Clock()
                 #while pygame.mixer.music.get_busy():
                   # check if playback has finished
                 #    clock.tick(10)
                 #HTimeOut=datetime.now()+timedelta(seconds=4)
                 #colorfijo2('24','100','000','100','000')   
                        #luz=1

                 
                     
                 
                 #HTimeOut=datetime.now()+timedelta(seconds=3)
                 
                #noviembre2017
                 recibi=''
                 #acumulador=''
                 HTimeOut=datetime.now()+timedelta(seconds=4)
                 
                 #if confirmacion==True:
                 #    colorfijo2('25','100','000','100','000')
                 if PantallaPago==0 and TerminoOperacion==1:# and tarjeta=='':#tarjetalista==1:# and TerminoOperacion==1:
                     principal=0
                     
                     screen.fill((0,0,0))
                     bg = pygame.image.load("fraspitk.jpg")

                    
                     screen.blit(bg,(0,0))
                     #myfont = pygame.font.SysFont("monospace", 55)
                     myfont =pygame.font.Font("super.ttf",42)
                     label = myfont.render(LabelPagando, 1, (255,255,255))
                     screen.blit(label,(((infoObject.current_w - label.get_width()) / 2),70))
                     #myfont = pygame.font.SysFont("monospace", 90)
                     myfont =pygame.font.Font("super.ttf",95)
                     if Maximo!=0:
                         if (int(ctk) * Multiplicador)>Maximo:
                             todostk=Maximo
                         else:
                             todostk=int(ctk) * Multiplicador
                     else:
                         todostk=int(ctk) * Multiplicador
                     esto=str(todostk)#str((ctk * Multiplicador))
                     label1 = myfont.render(esto, 1, (0,255,20))
                     screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label1.get_height()) / 2)+55))#(label, (((infoObject.current_w - label.get_width()) / 2),160 ))
                     pygame.display.update() 
                 
                 PagandoTk=False
                 #break
                 ###print(ctk)
                 #-----PARA PAGO FISICO---
            ar=recibi.find('A',0)
            if ar>-1 and UltTar!='':
                 if PagandoTk==False:
                     #if confirmacion==True:
                     acumulador=''
                     #EnvioPuerto=desactivot
                     if ctk>=0:
                         confirmacionMudo=False
                         pygame.mixer.music.load('/home/pi/pagotk.mp3')
                     confirmacion=False
                     if confirmacion==False:
                         serie.write(bytes(desactivot))
                         time.sleep(0.050)
                     #time.sleep(0.100)
                     #if int(ctk)<5:
                     #    a=bytearray([0x24,0xDB,0x6D,0xB6,0xDB,0x6D,0xB6])
                     #    serie.write(bytes(a))
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
                 #if True:
                 #    if confirmacionMudo==False:
                 #        serie.flush()
                 #        serie.write(bytes([0x3C]))
                 pygame.mixer.music.play()
##                 if Maximo!=0 and (ctk*Multiplicador)<=Maximo:
##                     ultctk=ultctk+(ctk*Multiplicador)
##                 elif Maximo!=0 and (ctk*Multiplicador)>Maximo:
##                     ultctk=ultctk+Maximo
##                 elif Maximo==0:
##                     ultctk=ultctk+(ctk*Multiplicador)

                 
                 HTimeOut=datetime.now()+timedelta(seconds=4)
                 if PantallaPago==0 and TerminoOperacion==1:# and tarjetalista==1:
                     principal=0
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
            if len(acumulador)>0 and ctk==0: #len(a)==0 and 
                qq=acumulador.find(';',0)
                if qq>0:
                    punto=1
                    #principal=0
                pp=acumulador.find('?',0)
                if pp>0:#and controlluces==0:                
                        #punto=1
                        serie.flush()
                        EnvioPuerto=''
                        p=acumulador.find(';',0)
                        h=p+15
                        
                        #[p:h]
                        
                        mtar=acumulador[p:h]
                        print(mtar)
                        #while principal==0:
                        #    time.sleep(0.010)
                        if len(mtar)>=15 and ctk==0:# and tarjeta=='':# and controlluces==0:
                            #TerminoOperacion=0
                            print("entra tarjeta" + str(mtar))
                            acumulador=''
                            #MuestraTarjeta(mtar)
                            #controlluces=0
                            #confirmacion=False
                            #while confirmacion==False:
                            #    serie.flush()
                            #    serie.write(bytes(bytearray([0x30,0x00,0x00])))
                            #    time.sleep(0.050)
                            tarjeta=mtar
                            #TerminoOperacion=0
                            #ultctk=0
                            #Debitar()
                             
                            
                        else:
                            #TerminoOperacion=0
                            print("NO entra tarjeta")
                            if len(mtar)>=15:
                                acumulador=''
                                punto=0
            if controlluces==0 and punto==0 and ctk==0 and TerminoOperacion==1:
                controlluces=1
            #if controlluces==1 and ctk==0 and punto==0 and PantallaPago==0 and PN!='-' and UsuarioConectado==0:#punto==0 and controlluces==1:
            #    controlluces=0
            #    MiLuz()
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
    doblev=''
    while True:
        a=''        
        time.sleep(0.001)
             
##        if len(acumulador)>0: #len(a)==0 and 
##   
##            pp=acumulador.find('?',0)
##            if pp>0:#and controlluces==0:                
##                    
##                    serie.flush()
##                    EnvioPuerto=''
##                    p=acumulador.find(';',0)
##                    h=p+15
##                    
##                    #[p:h]
##                    
##                    mtar=acumulador[p:h]
##                    print(mtar)              
##                    if len(mtar)>=15 and ctk==0:# and tarjeta=='':# and controlluces==0:
##                        #TerminoOperacion=0
##                        print("entra tarjeta" + str(mtar))
##                        acumulador=''
##                        #MuestraTarjeta(mtar)
##                        controlluces=0
##                        #confirmacion=False
##                        #while confirmacion==False:
##                        #    serie.flush()
##                        #    serie.write(bytes(bytearray([0x30,0x00,0x00])))
##                        #    time.sleep(0.050)
##                        tarjeta=mtar
##                         
##                        
##                    else:
##                        #TerminoOperacion=0
##                        print("NO entra tarjeta")
##                        if len(mtar)>=15:
##                            acumulador=''
        if acumulador=='' and PagandoTk==True :

            PagandoTk=False

            b=1
        if PagandoTk==False and int(ctk)>0 and TimeOut==True:
            FinalizoTks()
            #ctk=0
            #if ctk==0:
            #    TerminoPago()
                
        if  acumulador.find('W2',0)>-1:
            
            print('W')
            acumulador=''
            doblev='W'
            time.sleep(0.600)
            if UsuarioConectado==0:# and controlluces==1:#tarjeta=='' and TerminoOperacion==1:# and controlluces==1:
                TerminoOperacion=0
                #while controlluces==0:
                #    time.sleep(0.001)
                #controlluces=0
                #serie.flush()
                #serie.write(bytes([0x3C]))#sonido
                #time.sleep(0.100)
                myfont =pygame.font.Font("super.ttf",35)
                #serie.flush()
                time.sleep(0.10)
                #serie.flush()
                #serie.write(bytes(ColorRojo))
                #serie.write(bytes([0x3C]))#sonido
                time.sleep(0.100)
                bg = pygame.image.load("fondot.jpg")
                screen.blit(bg,(0,0))                                
                if LabelPase=='Pase Su Tarjeta':
                        TIT='TARJETA INVALIDA'
                        esto="Pase nuevamente su tarjeta"
                        #pygame.mixer.music.load('/home/pi/e_invalida.mp3')#i_sinsaldo.mp3')
                        #pygame.mixer.music.play()
                else:   
                        TIT='INVALID CARD'
                        esto="Slide your card again"
                        #pygame.mixer.music.load('/home/pi/i_invalida.mp3')#i_sinsaldo.mp3')
                        #pygame.mixer.music.play()
                
                label = myfont.render(LabelInvalidCard, 1, (0,255,20))
                screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
                #myfont =pygame.font.Font("super.ttf",25)
                label1 = myfont.render(LabelInvalidCard2, 1, (0,255,20))
                screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                                           
                pygame.display.flip() 
                #pygame.display.update()
                time.sleep(.100)
                
                time.sleep(0.9)
                #serie.flush()
                #while doblev!='':
                #    time.sleep(0.001)
                #time.sleep(0.030)
                #confirmacion=False
                #serie.write(bytes(activot))
                #while confirmacion==False:
                #    b=1
                #serie.flush()
                time.sleep(0.100)
                #ConfirmacionTarjeta=False
                    
                #while ConfirmacionTarjeta==False:
                #    serie.write(bytes(activot))
                #    time.sleep(0.100)
                #ConfirmacionTarjeta=False
                #serie.write(bytes(activot))
                #t3=0
                #serie.write(bytes(activot))
                #while ConfirmacionTarjeta==False:
                     #if t3==30:    
                #     serie.write(bytes(activot))
                     #        t3=0
                #     time.sleep(0.200)
                     #t3=t3+1
                #controlluces=1
                
                #acumulador=''
                #time.sleep(1.0)
                #serie.flush()
                
                #serie.write(bytes([0x3A]))#mudo
                #time.sleep(0.100)    
                PantFull()
                controlluces=1
                TerminoOperacion=1
                #doblev=''
            else:
                #if tarjeta=='':# and controlluces==1:
                    #serie.flush()
                    #time.sleep(0.500)
                    #ConfirmacionTarjeta=False
                    #serie.write(bytes(activot))
                    #t3=0
                    #serie.write(bytes(activot))
                    #while ConfirmacionTarjeta==False:
                        #if t3==30:    
                    #    serie.write(bytes(activot))
                        #        t3=0
                    #    time.sleep(0.200)
                        #t3=t3+1
                    controlluces=1
                    TerminoOperacion=1
                    #TerminoOperacion=1
                    #b=1
                        
                     #controlluces=1
                    #doblev=''
                     #acumulador=''
                     #tarjetalista=1
                
        

            

             
def Pantalla():
    #aca esta loop visual
    ##pygame.display.update()

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
    HTimeOut=datetime.now()+timedelta(seconds=4)

def FinalizoTks():
    global ctk
    global PagandoTk
    global TimeOut
    global ultctk
    global EnvioPuerto
    global PantallaPago
    global LabelPago
    global TerminoOperacion
    global controlluces
    global punto
    tkfinal=0
    #while EnvioPuerto!='':
    #    b=1
    #EnvioPuerto=bytearray([0x30,0x01,0x27])#3r byte es para fisico o electronico, 35 es elec y 60 fisico
    
    if ctk>0 and UltTar!='': 

        if True:
            PagoOk=0
            
            #if Maximo!=0 and (ctk*Multiplicador)<=Maximo:
            #    ultctk=ultctk+(ctk*Multiplicador)
            #elif Maximo!=0 and (ctk*Multiplicador)>Maximo:
            #    ultctk=ultctk+Maximo
            #elif Maximo==0:
            #    ultctk=ultctk+(ctk*Multiplicador)
            #print('Acumula tk '+str(ultctk))
                
            while PagoOk==0:
                    try:
                        #Create a TCP/IP socket
                        #prueba 2018
                        
                        #-----------
                        
                        PantallaPago=1
                        TerminoOperacion=0
                        tkfinal=ctk*Multiplicador
                        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

                        # Connect the socket to the port where the server is listening
                        server_address = (str(IP),5000)#192.168.2.50 -- IAAPA
                        ##print('connecting to %s port %s' % server_address)
                        sock.settimeout(0.3)
                        sock.connect(server_address)
                    

                    
                
                        # Send data #P'+UltTar[:-1]+{0:4}.format(str(ctk))#NumMaq + tarjeta #tarjeta.get() #'C;1921000005618'
                        
                        ttk2018=str(hex(ctk)[2:]).upper()
                        message = MiMac+','+IPFINAL+','+NumMaq+'#'+UltTar[:-1] + str(ttk2018).zfill(4) +'99'#NumMaq + tarjeta #tarjeta.get() #'C;1921000005618'#'0055#;47900089307660002F4'    
                        #print('sending "%s"' % message)#'{0:04}'.format(str(ctk))
                        #print('{0:04}'.format(str(ctk)))
                        message=str(message)
                        #sock.settimeout(1.25)
                        sock.sendall(message.encode(encoding='UTF-8',errors='ignore'))

                        # Look for the response
                        amount_received = 0
                        amount_expected = 1#len(message)
                        data=''
                        yat=datetime.now()
                        hastat=datetime.now()+timedelta(milliseconds=2000)
                        #amount_received < amount_expected and
                        while  data=='':#hastat > yat and data=='':
                            data = sock.recv(100)
                            #print(data)
                        if data:
                            #ctk=0
                            message='103'
                            message=str(message)
                            sock.sendall(message.encode(encoding='UTF-8',errors='ignore'))
                            pagueTk=ultctk
                            if Maximo!=0 and (ctk*Multiplicador)<=Maximo:
                                ultctk=ultctk+(ctk*Multiplicador)
                            elif Maximo!=0 and (ctk*Multiplicador)>Maximo:
                                ultctk=ultctk+Maximo
                            elif Maximo==0:
                                ultctk=ultctk+(ctk*Multiplicador)
                            print('Acumula tk '+str(ultctk))
                            ctk=0
                            PagoOk=1
                        b=0
                    except:
                        if Maximo!=0 and (ctk*Multiplicador)<=Maximo:
                            ultctk1=ultctk+(ctk*Multiplicador)
                        elif Maximo!=0 and (ctk*Multiplicador)>Maximo:
                            ultctk1=ultctk+Maximo
                        elif Maximo==0:
                            ultctk1=ultctk+(ctk*Multiplicador)
                        serie.write(bytearray(bytes([0x25,0x6D,0xB6,0xDB,0x6D,0xB6,0xDF])))
                        balance=str(ultctk1)#+' Tk'
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
                        myfont =pygame.font.Font("super.ttf",32)
                        label1 = myfont.render(esto, 1, (0,255,255))
                        screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                           
                        pygame.display.flip()
                        time.sleep(1)
            
                
                    

        #except:
            ##print('error al conectar')
        #    b=1

        if True:#finally:
            #tarjeta=''
            #punto=0
            #haytarjeta=0
            print('closing socket')
            serie.flush()
            serie.write(bytes(bytearray([0x3C])))
            pygame.mixer.music.load("/home/pi/tkgano.mp3")
            
            #LabelPago=data.decode('utf8')
            #print(LabelPago)
            sock.close()
            screen.fill((0,0,0))
            cancelotk=0
            que=data.decode('utf8')
            if data.decode('utf8')[1:2]=='1':
                    bg = pygame.image.load("fondo.jpg")
                    pygame.mixer.music.play()
                    colorfijo2('28','100','111','100','100')
                    cancelotk=0
            else:
                    bg = pygame.image.load("fondot.jpg")
                    #LabelPago='Disable Pay'
                    colorfijo2('28','010','011','010','011')
                    cancelotk=1
                    #ctk=0
            screen.blit(bg,(0,0))
            #myfont = pygame.font.SysFont("monospace", 45)
            myfont =pygame.font.Font("super.ttf",34)
            if cancelotk==0:
                label = myfont.render(LabelPago, 1, (0,180,0))
            else:
                label = myfont.render(LabelPagoD, 1, (0,180,0))
            #label = myfont.render(que[14:30], 1, (0,255,23))
            screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
            myfont =pygame.font.Font("super.ttf",42)
            
            #if ultctk==0:
            #    ultctk=ctk
            #    ctk=0
            #label1 = myfont.render(str(que[30:46]), 1, (0,255,20))
            #if Maximo!=0:
            #    if (int(ultctk) * Multiplicador)>Maximo:
            #        todostk=Maximo
            #    else:
            #        todostk=int(ultctk) * Multiplicador
            #else:
            
            
            if tkfinal > ultctk and Maximo>tkfinal or Maximo==0:
                todostk=tkfinal
            else:
                todostk=ultctk# * Multiplicador
            label1 = myfont.render(str(todostk), 1, (0,255,20))
            if cancelotk==0:
                screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
            pygame.display.flip() 
            #pygame.display.update()

            
            
##    while EnvioPuerto!='':
##        b=1
##    EnvioPuerto=bytearray([0x30,0x01,0x35])
    
            time.sleep(1.0)
            #ctk=0
            
            #while EnvioPuerto!='':
            #    b=1
            #EnvioPuerto=bytearray([0x30,0x01,0x27])
            controlluces=1
            if ctk==0:#True:#int(ctk)==0:#if int(ultctk) == int(ctk):
                #ctk=0
                #ultctk=0
                punto=0
                time.sleep(1.2)
                #clock = pygame.time.Clock()
                #while pygame.mixer.music.get_busy():
                    #check if playback has finished
                #    clock.tick(30)
                serie.flush()
                serie.write(bytes([0x3A]))#mudo
                time.sleep(0.100)

                controlluces=1
                PagandoTk=False
                TimeOut=False
                PantallaPago=0
                TerminoOperacion=1
                PantFull()
                punto=0
            else:
                controlluces=1
            #    PagandoTk=False
            #    TimeOut=False
            #    PantallaPago=0
            #    TerminoOperacion=1
                #time.sleep(0.2)
            
            #else:
            #    ultctk=0
            #    PagandoTk=False
            #    TimeOut=False
                PantallaPago=0
            #    TerminoOperacion=1


    


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
    ultctk=0
    #controlluces=1
    while True: 
            
            if ctk>0 and TimeOut==False and UltTar!='' and PantallaPago==0 and TerminoOperacion==1: #and  and ctk>utlctk:

                if ultctk==0:
                    controlluces=1

                

                #ultctk=ctk
                #pygame.mixer.music.play()
                #time.sleep(1)
                
                #screen.fill(white)
                #if ctk<5:
                #controlluces=0
                #if controlluces==1:
                #    controlluces=0
                #    serie.flush()
                #    serie.write=ColorVerde
                    
                    #colorfijo2('25','100','100','111','111')
                    
                #if apb==1 :#and controlluces == 1:
                #    colorfijo2('24','100','100','111','111')
                #     #controlluces=0
                #    apb=0
                #if apb==0: #and controlluces == 1:
                #        #luz=1
                #    colorfijo2('24','111','111','100','100')
                    #controlluces=0
                #    apb=1
                screen.fill((0,0,0))
                bg = pygame.image.load("fraspitk.jpg")

                
                screen.blit(bg,(0,0))
                #myfont = pygame.font.SysFont("monospace", 55)
                myfont =pygame.font.Font("super.ttf",42)
                label = myfont.render(LabelPagando, 1, (255,255,255))
                screen.blit(label,(((infoObject.current_w - label.get_width()) / 2),70))
                #myfont = pygame.font.SysFont("monospace", 90)
                myfont =pygame.font.Font("super.ttf",95)
                if Maximo!=0:
                    if (ctk*Multiplicador)>Maximo:
                        esto=str(Maximo)
                    else:
                        esto= str((ctk * Multiplicador))
                else:
                    esto= str((ctk * Multiplicador))
                print(esto)
                label1 = myfont.render(esto, 1, (0,255,20))
                screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label1.get_height()) / 2)+55))#(label, (((infoObject.current_w - label.get_width()) / 2),160 ))
                pygame.display.flip() 
                #pygame.display.update()



                
                #colorfijo2('25','000','100','000','000')    
                #luz=0    
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
                    global principal
                    
                    
            
                    #infoObject = pygame.display.Info()
                    #pygame.display.quit
                    #pygame.display.init
                    #screen=pygame.display.set_mode((infoObject.current_w, infoObject.current_h))
                    if  int(NumMaq,16) > int('000F',16):                    
                        if int(ctk)==0:
                                screen.fill((0,0,0))
                                bg = pygame.image.load("fraspi1.jpg")
                                screen.blit(bg,(0,0))                         
                                    # initialize font; must be called after 'pygame.init()' to avoid 'Font not Initialized' error
                                myfont =pygame.font.Font("super.ttf",30) #pygame.font.SysFont("monospace", 35)
                                myfont1 =pygame.font.Font("super.ttf",20)
                                myfont2 =pygame.font.Font("super.ttf",15)
                                myfont3 =pygame.font.Font("super.ttf",10)
                                #label4 = myfont3.render('SN:'+str(IPFINAL[3:]), 1, (0,255,255))
                                #screen.blit(label4,(415,310))

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
                                label4 = myfont3.render('SN:'+str(IPFINAL[3:]), 1, (0,255,255))
                                screen.blit(label4,(415,310))
                                label5 = myfont3.render(str(NumMaq), 1, (0,255,255))
                                screen.blit(label5,(1,310))
                                if EMPRESA!='0':
                                    QrImg = pygame.image.load('qr.png')
                                    screen.blit(QrImg, (40,116))
                                else:
                                    QrImg = pygame.image.load('qr.png')
                                    screen.blit(QrImg, (45,126))
                                if int(ctk)==0:                    
                                    pygame.display.flip()                                 
                                tarjeta=''
                                TerminoOperacion=1
                                tarjetalista=1
                                controlluces=1
                                time.sleep(0.500)
                                serie.flush()
                                ConfirmacionTarjeta=False
                                while ConfirmacionTarjeta==False:
                                    serie.write(bytes(activot))
                                    time.sleep(0.250)
                                principal=1
                        else:
                            controlluces=1
                            TerminoOperacion=1
                            tarjetalista=1
                            
                    else:
                            serie.flush()
                            serie.write(bytes(bytearray([0x30,0x00,0x00])))
                            screen.fill((0,0,0))
                            bg = pygame.image.load("fondot.jpg")
                            screen.blit(bg,(0,0))                         
                                # initialize font; must be called after 'pygame.init()' to avoid 'Font not Initialized' error
                            myfont =pygame.font.Font("super.ttf",30) #pygame.font.SysFont("monospace", 35)
                            myfont1 =pygame.font.Font("super.ttf",20)
                            myfont2 =pygame.font.Font("super.ttf",15)
                            myfont3 =pygame.font.Font("super.ttf",10)

                                # render text
                            label = myfont.render(LabelConsulta1, 1, (255,255,255))
                            screen.blit(label,(((infoObject.current_w - label.get_width()) / 2),135))#(label, (((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))
                            label1 = myfont.render(LabelConsulta2, 1, (255,255,255))
                            screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2),165))

                            #myfont = pygame.font.SysFont("monospace", 45)
                            
                            label3 = myfont2.render('www.magneticash.com', 1, (255,255,255))
                            screen.blit(label3,(115,305))
                            label4 = myfont3.render('SN:'+str(IPFINAL[3:]), 1, (0,255,255))
                            screen.blit(label4,(415,310))
                            #if EMPRESA!='0':
                            #    QrImg = pygame.image.load('qr.png')
                            #    screen.blit(QrImg, (40,116))
                            #else:
                            #    QrImg = pygame.image.load('qr.png')
                            #   screen.blit(QrImg, (35,110))
                            if int(ctk)==0:                    
                                pygame.display.flip()                                 
                                tarjeta=''
                                TerminoOperacion=1
                                tarjetalista=1
                                controlluces=1
                                time.sleep(0.500)
                                serie.flush()
                                ConfirmacionTarjeta=False
                                while ConfirmacionTarjeta==False:
                                    serie.write(bytes(activot))
                                    time.sleep(0.250)
                                principal=1
                            else:
                                tarjeta=''
                                controlluces=1
                                TerminoOperacion=1
                                tarjetalista=1
                    

def MuestraTarjeta():
    #global confirmacion
    global EnvioPuerto
    global tarjeta
    global punto
    global haytarjeta
    global YaEntre
    global TerminoOperacion
    global ultctk
    YaEntre=0
    while True:

##        if len(tarjeta)==15 and TerminoOperacion==1 and ctk==0:
##            print('entro a funcion debitar')
##            YaEntre=1
##            TerminoOperacion=0
##            ultctk=0
##            Debitar()
            #tarjeta=''
            #time.sleep(4)                    
        if len(tarjeta)==15 and  ctk==0 and principal==1:#YaEntre==0 and TerminoOperacion==1 and
            print('entro a funcion debitar')
            YaEntre=1
            TerminoOperacion=0
            ultctk=0
            Debitar()
            tarjeta=''
            #time.sleep(4)
        #if TerminoOperacion==1 and YaEntre==1:#len(tarjeta)==0
        #    print('NO entro a funcion debitar')
        #    YaEntre=0
        #    tarjeta=''    
        #    punto=0    


def MiLuz():
    global apb
    global ledcual
    global controlluces
    global doblev
    if apb=='':
        apb=0
    while True:
        b=1
        if doblev=='W':
                    controlluces=0
                    #time.sleep(1.0)
                    while controlluces==0:
                        serie.flush()
                        serie.write(bytes(bytearray([0x25,0x49,0x00,0x00,0x49,0x00,0x00])))
                        time.sleep(0.050)
                    doblev='W'
                    #ConfirmacionTarjeta=False
                    
                
                
                
                
                    myfont =pygame.font.Font("super.ttf",35)
                
                    #time.sleep(0.10)
                    
                    #time.sleep(0.100)
                    bg = pygame.image.load("fondot.jpg")
                    screen.blit(bg,(0,0))                                
                    if LabelPase=='Pase Su Tarjeta':
                            TIT='TARJETA INVALIDA'
                            esto="Pase nuevamente su tarjeta"
                            #pygame.mixer.music.load('/home/pi/e_invalida.mp3')#i_sinsaldo.mp3')
                            #pygame.mixer.music.play()
                    else:   
                            TIT='INVALID CARD'
                            esto="Slide your card again"
                            #pygame.mixer.music.load('/home/pi/i_invalida.mp3')#i_sinsaldo.mp3')
                            #pygame.mixer.music.play()
                    
                    label = myfont.render(LabelInvalidCard, 1, (0,255,20))
                    screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
                    #myfont =pygame.font.Font("super.ttf",25)
                    label1 = myfont.render(LabelInvalidCard2, 1, (0,255,20))
                    screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                                               
                    pygame.display.flip() 
                    #pygame.display.update()
                    
                    
                    time.sleep(1.0)
                    #serie.flush()
                    #serie.write=bytes(activot)
                    time.sleep(0.050)
                    #punto=0
                    #recibi=''
                    doblev=''
                    PantFull()
        #if  doblev=='W' and controlluces==1:
        #     ConfirmacionTarjeta=False
        #     print('w falso')
        #     while ConfirmacionTarjeta==False:
        #         serie.flush()
        #         serie.write(bytes(activot))
        #         time.sleep(0.5)
        #     doblev=''
        #     print('w verdadero')
        #    controlluces=0
        #    while controlluces==0:
                #serie.flush()
                #serie.write(bytes(bytearray([0x28,0x49,0x00,0x00,0x49,0x00,0x00])))
            #doblev=''
        #if controlluces==1 and ctk==0:#EnvioPuerto==''and tarjeta=='' and punto==0 and ctk==0:
            #print('entro a luces')
        elif punto==0 and PN!='-' and ctk==0 and principal==1 and UsuarioConectado==0:#doblev=='' and ctk==0 and punto==0 and PantallaPago==0 and PN!='-' and UsuarioConectado==0:#punto==0 and controlluces==1:
            #print('entro a luces')
            #controlluces=0
                #MiLuz()
            if ledcual==1:#TerminoOperacion==1 and ledcual==1 and controlluces==1 and UsuarioConectado==0 and ctk==0:
                    controlluces=0    
                    ledcual=2
                    serie.flush()
                    serie.write(bytes(activot))
                    time.sleep(0.100)
                    luz2017('paseo')
                    time.sleep(0.500)
                    #return    

                    
            elif ledcual==2:#elif TerminoOperacion==1 and ledcual==2 and controlluces==1 and UsuarioConectado==0 and ctk==0:
                    controlluces=0
                    ledcual=1
                    serie.flush()
                    serie.write(bytes(activot))
                    time.sleep(0.100)
                    luz2017('paseo2')
                    time.sleep(0.500)
                    #return
        #elif doblev!='':
        #    serie.flush()
        #    serie.write(bytes(bytearray([0x28,0x49,0x00,0x00,0x49,0x00,0x00])))
        #    doblev=''
            
            
                
                



                
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
    aaa=False
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
#if estahotspot==0:        
#os.system('sudo ifconfig wlan0 down')
#time.sleep(0.25)
#os.system('sudo ifconfig wlan0 up')
#time.sleep(1.0)            
serie.write(bytes(desactivot))
infoObject = pygame.display.Info()
pygame.DOUBLEBUF
pygame.HWSURFACE
pygame.OPENGL
screen=pygame.display.set_mode((infoObject.current_w, infoObject.current_h))
pygame.mixer.pre_init(44100, -16, 2, 2048) # setup mixer to avoid sound lag
pygame.mixer.init()
pygame.mixer.music.set_volume(0.60)

#pygame.mouse.set_visible(s)
pygame.mouse.set_visible(0)

myfont = pygame.font.SysFont("monospace", 40)
pygame.display.toggle_fullscreen()
#----INICIO 2018 E.E.C
controlluces=1
#hilos de escucha primero 2018
EscuchaConfig=threading.Thread(target=listen)
EscuchaConfig.start()
#time.sleep(5.0)
escuchar=threading.Thread(target=EscuchoPuerto)
escuchar.start()
mserie=threading.Thread(target=PuertoSerie)
mserie.start()
mpantalla=threading.Thread(target=Pantalla)
mpantalla.start()

#-----Muestra INICIO 2018----

serie.flush()
serie.write(bytes(bytearray([0x3C])))
time.sleep(0.2)
pygame.mixer.music.load('/home/pi/start.mp3')                                    
pygame.mixer.music.play()
colorfijo2('28','111','111','111','001')

screen.fill((0,0,0))
pygame.font.init
myfont =pygame.font.Font("super.ttf",30)
myfont1 =pygame.font.SysFont("comicsansms",30)
myfont2 =pygame.font.SysFont("comicsansms",30)

bg = pygame.image.load("fondot.jpg")
screen.blit(bg,(0,0))

esto='Starting Controller Wifi...'
label = myfont.render("MAGNETIC CASH SYSTEM", 1, (0,255,20))
screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
myfont =pygame.font.Font("super.ttf",23)
label1 = myfont.render(esto, 1, (0,255,255))
screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+20))                          
pygame.display.flip()
colorfijo2('28','111','111','111','001')
#time.sleep(3.5)
#serie.flush()
#serie.write(bytes(bytearray([0x3A])))



#serie.flush()
#pcorto=bytearray([0x30,0x00,0x00])
#serie.write(pcorto)
try:
        file=open("/home/pi/ControllerConfig/pconf.mc","r")
        traje=file.readline()
        trajepago=traje[7:12]
        file.close()        
        pcorto=bytearray()#[0x29,0x10])#,0x00,0x00,0x00,0x00,0x00])#pago rapido-coin rapido
        #aaa=str(traje[7:9]) + ' ' + str(traje[10:12])
        pcorto.append(0x30)
        pcorto.append(int(traje[7:9],16))
        pcorto.append(int(traje[10:12],16))
        serie.flush()
        serie.write(pcorto)
        time.sleep(0.2)
        serie.flush()
        serie.write(pcorto)
        time.sleep(0.2)
        #serie.flushOutput()
except:
        serie.flush()
        pcorto=bytearray([0x30,0x00,0x00])
        trajepago='00,00'
        serie.write(pcorto)
        serie.flush()
#--------------------------------------
#IpMaster:
try:
    file=open("/home/pi/ControllerConfig/hip.mc","r")
    IP=file.readline()
    file.close()
except:
           print('deja ip por defecto')

#----------------------------

#apago y prendo wifi para que levante bien LA IP! Eze.C
estahotspot=0
try:
        file=open("hotspoton.mc","r")
        file.flush()
        file.close()
        estahotspot=1
except:
        print('Archivo hotspoton inexistente')
#if estahotspot==0:        
#    os.system('sudo ifconfig wlan0 down')
    #time.sleep(1.5)
#    os.system('sudo ifconfig wlan0 up')

#-------------RECIBO IP FINAL---------------------------
tengoIP=0
leyo=0
IPFINAL=''
try:
        file=open("/home/pi/ControllerConfig/mip.mc","r")
        leyo=1
        IPFINAL=file.readline()
        file.close()
        tengoIP=1
except:
        print('Archivo mip.mc inexistente')
        tengoIP=0
if IPFINAL=='' and leyo==1:
    os.system("sudo python3 /home/pi/ControllerConfig/configv.py")
    time.sleep(5.0)
    os.system("reboot")

if tengoIP==1:
        print('Abriendo Controller...')


        
#        escuchar.terminate()
#        EscuchaConfig.terminate()
        #os.system("sudo python3 /home/pi/Controller.py -f")
        #pygame.quit()
        #sys.exit()
        
else:

#Expandir File System.------------------------
        screen.fill((0,0,0))
        pygame.font.init
        myfont =pygame.font.Font("super.ttf",30)
        myfont1 =pygame.font.SysFont("comicsansms",30)
        myfont2 =pygame.font.SysFont("comicsansms",30)

        bg = pygame.image.load("fondot.jpg")
        screen.blit(bg,(0,0))

        esto='Please Wait...'
        label = myfont.render("Memory Expansion On", 1, (0,255,20))
        screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-60))
        myfont =pygame.font.Font("super.ttf",20)
        label1 = myfont.render(esto, 1, (255,255,255))
        screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-25))
        label2 = myfont1.render(esto, 1, (255,0,0))
        screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+100))
        pygame.display.flip()    
        time.sleep(2.0)
        os.system("sudo raspi-config --expand-rootfs")
        time.sleep(10.0)
#---------------
        msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
        MiMac1=msj.strip()

        #saveQRCodePNG('qrip',3,MiIp) #genero QR con numero de maquina
        time.sleep(0.3)
        IPFINAL=''
        while IPFINAL=='':
              screen.fill((0,0,0))
              pygame.font.init
              myfont =pygame.font.Font("super.ttf",30)
              myfont1 =pygame.font.SysFont("comicsansms",30)
              myfont2 =pygame.font.SysFont("comicsansms",30)

              bg = pygame.image.load("fondot.jpg")
              screen.blit(bg,(0,0))

              esto='waiting for Final IP...'
              label = myfont.render("Initial Configuration", 1, (0,255,20))
              screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-60))
              myfont =pygame.font.Font("super.ttf",20)
              label1 = myfont.render(esto, 1, (255,255,255))
              screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-25))
              msj=subprocess.check_output("iw dev wlan0 link|grep SSID|awk '{print $2}'",shell=True).decode('utf8')
              esto=msj.strip()
              print(esto)
              if esto!='':
                      esto='Connected to ' + esto
                      label3 = myfont2.render("Searching CtrlAdm Soft", 1, (0,255,255))
              else:
                      #creo archivo de configuracion de red hotspot
                      #msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
                      #MiMac1=msj.strip()
                      #MiMac1=MiMac1[0:2]+MiMac1[3:5]+MiMac1[6:8]+MiMac1[9:11]+MiMac1[12:14]+MiMac1[15:17]
                      #print(MiMac1)
                      esto="Can't connect to Server"
                      label3 = myfont2.render("Check Wifi or Server", 1, (0,255,255))
              label2 = myfont1.render(esto, 1, (255,0,0))
              screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+100))
              screen.blit(label3,(((infoObject.current_w - label3.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+127))
              #QrImg = pygame.image.load('qrip.png')
              #screen.blit(QrImg, (185,140))
              
              pygame.display.flip() 
              #pygame.display.update()      
              #DosMundos()
              colorfijo2('24','100','101','001','010')
              time.sleep(1.0)
              try:
                      sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                      server_address = ('192.168.1.30',6000)#192.168.2.50 -- IAAPA
                      #print('connecting to %s port %s' % server_address)
                      #sock.settimeout(1.0)
                      sock.connect(server_address)
                      
                      #print('paso conexion')
                      ##sock.settimeout(3.0)
                      message=MiMac1#+','+MiIp+','+NumMaq+'$XX'
                      #print(message)
                      message=str(message)
                      sock.sendall(message.encode(encoding='UTF-8',errors='ignore'))
                      amount_received = 0
                      amount_expected = 1#len(message)
                      data1=''
                      data=''
                      while data=='':
                          data = sock.recv(16)
                      data=data.decode('utf8')
                      IPFINAL=data
                      break
              except:
                      b=1
                      continue
      
        #--------
        #Cuando sale con ip guarda en el archivo y reinicia     
        file=open("/home/pi/ControllerConfig/mip.mc","w")
        file.write(str(IPFINAL))
        file.flush()
        file.close()
        print('termino')
        screen.fill((0,0,0))
        pygame.font.init
        myfont =pygame.font.Font("super.ttf",30)
        myfont1 =pygame.font.SysFont("comicsansms",30)
        myfont2 =pygame.font.SysFont("comicsansms",40)

        bg = pygame.image.load("fondot.jpg")
        screen.blit(bg,(0,0))

        esto='IP FINAL OK'
        label = myfont.render("Restarting Controller...", 1, (0,255,20))
        screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-60))
        myfont =pygame.font.Font("super.ttf",20)
        label1 = myfont.render(esto, 1, (255,255,255))
        screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-25))
        msj=subprocess.check_output("iw dev wlan0 link|grep SSID|awk '{print $2}'",shell=True).decode('utf8')
        esto=msj.strip()
		      
        esto='Conectado con ' + esto
        label3 = myfont2.render("Ip received:"+str(IPFINAL), 1, (0,255,255))

	#              else:
		              #creo archivo de configuracion de red hotspot
	#                      msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
	#                      MiMac1=msj.strip()
		              #MiMac1=MiMac1[0:2]+MiMac1[3:5]+MiMac1[6:8]+MiMac1[9:11]+MiMac1[12:14]+MiMac1[15:17]
		              #print(MiMac1)
	#esto='Connect to Controller ' + str(MiMac1)
	#label3 = myfont2.render("Password: MagneticCash123456", 1, (255,255,255))
	#label2 = myfont1.render(esto, 1, (255,255,255))
	#screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+100))
        screen.blit(label3,(((infoObject.current_w - label3.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+127))
	#QrImg = pygame.image.load('qrip.png')
	#screen.blit(QrImg, (185,140))
		      
        pygame.display.flip() 
        #pygame.display.update()

        colorfijo2('28','100','100','100','111')
        time.sleep(2.0)
        colorfijo2('28','100','100','000','000')
        #time.sleep(1.0)


#-------------FIN IP FINAL------------------------------

#Empieza viendo si tiene numero sino esta en modo config

#---busco si existen los archivos de config de numero
#---y wifi para local
haynumero=0
haywifi=0
estahotspot=0
try:
        file=open("/home/pi/hotspoton.mc","r")
        file.flush()
        file.close()
        estahotspot=1
except:
        print('Archivo hotspoton inexistente')
        NumMaq='XXXX'
try:
        file=open("/home/pi/ControllerConfig/red.mc","r")
        file.close()
        haywifi=1
except:
        print('Archivo red.mc inexistente')
        NumMaq='XXXX'
try:
        file=open("/home/pi/ControllerConfig/num.mc","r")
        NumMaq=str(file.readline())
        file.close()
        haynumero=1
except:
        print('Archivo num.mc inexistente')
        #os.remove("/home/pi/ControllerConfig/num.mc")
        try:
            os.remove("/home/pi/ControllerConfig/red.mc")
            os.remove("/home/pi/ControllerConfig/ConWifi.conf")
            os.remove("/home/pi/hotspot.mc")
            os.remove("/home/pi/hotspoton.mc")
        except:
            b=1
        NumMaq='XXXX'

#-----
#idioma
#try:
#        file=open("/home/pi/ControllerConfig/lang.mc","r")
#        a=str(file.readline())
#        file.close()
#        a=a.strip()
#        if a=='1':
#            Idioma=1
#        elif a=='2':
#            Idioma=2
#except:
#        print('Archivo lang.mc inexistente')
#        Idioma=1


##--Me fijo configuracion de pago de tk

#---------------------------
if haywifi==0 and estahotspot==0 or estahotspot==0 and NumMaq=='XXXX':
        #genero hotspot para que configuren una red
        try:
		#me fijo si esta como hotspot ya sino lo configuro
                file=open("/home/pi/hotspoton.mc","w")
                file.flush()
                file.close()
        except:
                print('error al crear archivo hotspoton')
        print('reinciando como hotspot - sin wifi configurado...')
        os.system("sudo python3 /home/pi/ControllerConfig/configh.py")
        #time.sleep(10.0)
        while True:
                colorfijo2('28','111','001','111','001')
        

controlluces=1
msj=ObtenerMac('wlan0')
saveQRCodePNG('qrip',3,MiIp) #genero QR con numero de maquina
time.sleep(0.3)
if haywifi==0:
        colorfijo2('25','010','000','010','000')
        time.sleep(1.2)
dispositivo=''
usuario=''
cconectados=''

#--espera como hotspot ---
while cconectados=='' and haywifi==0:
      if cconectados=='':
              msj=subprocess.check_output("arp -a",shell=True).decode('utf8')
              cconectados=msj.strip()
              if cconectados!='':
                      cconectados='>> '+cconectados[:cconectados.find('at')]+' <<'
      screen.fill((0,0,0))
      pygame.font.init
      myfont =pygame.font.Font("super.ttf",27)
      myfont1 =pygame.font.SysFont("comicsansms",25)
      myfont2 =pygame.font.SysFont("comicsansms",25)

      bg = pygame.image.load("fondot.jpg")
      screen.blit(bg,(0,0))

      esto='1- CONNECT TO WIFI:'
      label = myfont.render("Configuration Mode", 1, (255,255,255))
      screen.blit(label,(((infoObject.current_w - label.get_width()) / 2)-50, ((infoObject.current_h - label.get_height()) / 2)-100))
      myfont =pygame.font.Font("super.ttf",20)
      label1 = myfont.render(esto, 1, (255,255,255))
      screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-60))
      msj=subprocess.check_output("iw dev wlan0 link|grep SSID|awk '{print $2}'",shell=True).decode('utf8')
      esto=msj.strip()
      print(esto)
      if esto!='':
              #esto= esto #
              label3 = myfont2.render("NO MACHINE NUMBER", 1, (255,50,0))
      else:
              #creo archivo de configuracion de red hotspot
              msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
              MiMac1=msj.strip()
              MiMac1=MiMac1[0:2]+MiMac1[3:5]+MiMac1[6:8]+MiMac1[9:11]+MiMac1[12:14]+MiMac1[15:17]
              #print(MiMac1)
              esto='NAME: Controller ' + str(MiMac1)
              label3 = myfont2.render("PASS: Controller12345678", 1, (255,255,255))
              label4 = myfont2.render(str(cconectados), 1, (0,255,0))
      label2 = myfont2.render(esto, 1, (255,255,255))
      screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+ 98))
      screen.blit(label3,(((infoObject.current_w - label3.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+118))
      screen.blit(label4,(((infoObject.current_w - label4.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+138))
      #genero qr con nombre de hotspot------------
      nombreh='Controller '+str(MiMac1)
      saveQRCodePNG('qrh',3,nombreh)
      QrImg = pygame.image.load('qrh.png')
      screen.blit(QrImg, (180,120))
      #QrImg = pygame.image.load('qrip.png')
      #screen.blit(QrImg, (185,140))
      
      pygame.display.flip() 
      #pygame.display.update()      
      #DosMundos()
 #     time.sleep(1.0)
#Cuando esta conectado el hotspot se pone en azul
if haywifi==0:
        colorfijo2('25','001','001','001','111')
        time.sleep(5.0)
        colorfijo2('25','101','101','101','111')
while NumMaq=='XXXX':
      screen.fill((0,0,0))
      pygame.font.init
      myfont =pygame.font.Font("super.ttf",30)
      myfont1 =pygame.font.SysFont("comicsansms",25)
      myfont2 =pygame.font.SysFont("comicsansms",25)

      bg = pygame.image.load("fondot.jpg")
      screen.blit(bg,(0,0))

      esto='Access to ' + MiIp + ' port 5001'
      label = myfont.render("Configuration Mode", 1, (0,255,20))
      screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-60))
      myfont =pygame.font.Font("super.ttf",20)
      label1 = myfont.render(esto, 1, (255,255,255))
      screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-25))
      msj=subprocess.check_output("iw dev wlan0 link|grep SSID|awk '{print $2}'",shell=True).decode('utf8')
      esto=msj.strip()
      print(esto)
      if esto!='':
              #esto='Connected to ' + esto
              label3 = myfont2.render("NO MACHINE NUMBER", 1, (255,50,0))
      else:
              #creo archivo de configuracion de red hotspot
              msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
              MiMac1=msj.strip()
              MiMac1=MiMac1[0:2]+MiMac1[3:5]+MiMac1[6:8]+MiMac1[9:11]+MiMac1[12:14]+MiMac1[15:17]
              #print(MiMac1)
              esto='2 - ENTER A MACHINE NUMBER'
              label3 = myfont2.render("Use Num Code", 1, (255,255,255))
              label4 = myfont2.render(str(NumMaq), 1, (50,255,100))
      label2 = myfont1.render(esto, 1, (255,255,255))
      screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+30))
      screen.blit(label3,(((infoObject.current_w - label3.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+60))
      screen.blit(label4,(((infoObject.current_w - label4.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+90))      
      myfont1 =pygame.font.SysFont("comicsansms",30)
      msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
      MiMac1=msj.strip()
      MiMac1=MiMac1[0:2]+MiMac1[3:5]+MiMac1[6:8]+MiMac1[9:11]+MiMac1[12:14]+MiMac1[15:17]
      label4 = myfont1.render('To Configure Connect to Wifi', 1, (255,255,255))
      screen.blit(label4,(((infoObject.current_w - label4.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+135))
      label5 = myfont1.render('>> Controller '+str(MiMac1)+' <<', 1, (255,255,255))
      screen.blit(label5,(((infoObject.current_w - label5.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+155))
      #genero qr con nombre de hotspot------------
      nombreh='Controller '+str(MiMac1)
      saveQRCodePNG('qrh',2,nombreh)
      QrImg = pygame.image.load('qrh.png')
      screen.blit(QrImg, (1,245))
      #QrImg = pygame.image.load('qrip.png')
      #screen.blit(QrImg, (185,140))
      
      pygame.display.flip() 
      #pygame.display.update()      
      #DosMundos()
pygame.display.flip()
time.sleep(0.5)
#WIFIN=''
#WIFIP=''
##if WIFIN!='':
##	screen.fill((0,0,0))
##        pygame.font.init
##        myfont =pygame.font.Font("super.ttf",30)
##
##        bg = pygame.image.load("fondot.jpg")
##        screen.blit(bg,(0,0))
##
##        esto=balance#data#str(tarjeta)+str(' Ok')
##                                            #label = myfont.render(esto, 1, (0,255,20))
##                                            #screen.blit(label, (((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))
##
##        label = myfont.render("Finishing Configuration", 1, (0,255,20))
##        screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
##        myfont =pygame.font.Font("super.ttf",22)
##        label1 = myfont.render('Restarting Controller...', 1, (0,255,20))
##        screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
##                                           
##        pygame.display.flip() 
##        #pygame.display.update()
##                            #luz2017('rojo')
##        colorfijo2('25','111','111','111','100')
##	WifiGeneral()
##	colorfijo2('28','010','010','010','010')
##	time.sleep(0.5)

if haywifi==0:
        colorfijo2('25','011','011','011','111')
while WIFIN=='' and haywifi==0 or WIFIP=='' and haywifi==0:
      screen.fill((0,0,0))
      pygame.font.init
      myfont =pygame.font.Font("super.ttf",30)
      myfont1 =pygame.font.SysFont("comicsansms",30)
      myfont2 =pygame.font.SysFont("comicsansms",30)

      bg = pygame.image.load("fondot.jpg")
      screen.blit(bg,(0,0))

      esto='Access to ' + MiIp + ' port 5001'
      label = myfont.render("Configuration Mode", 1, (0,255,20))
      screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-60))
      myfont =pygame.font.Font("super.ttf",20)
      label1 = myfont.render(esto, 1, (255,255,255))
      screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-25))
      msj=subprocess.check_output("iw dev wlan0 link|grep SSID|awk '{print $2}'",shell=True).decode('utf8')
      esto=msj.strip()
      print(esto)
      if esto!='':
              esto='Connected to ' + esto
              label3 = myfont2.render("NO MACHINE NUMBER", 1, (255,50,0))
              label4 = myfont2.render(str(WIFIN), 1, (50,255,100))
      else:
              if WIFIN=='':              
                            esto='3 - ENTER WIFI NAME:'
                            label3 = myfont2.render("Use SSID: Code", 1, (255,255,255))
                            label4 = myfont2.render(str(WIFIN), 1, (50,255,100))
              if WIFIN!='' and WIFIP=='':              
                            esto='4 - ENTER WIFI PASSWORD:'
                            label3 = myfont2.render("Use PASS: Code", 1, (255,255,255))
                            label4 = myfont2.render(str(WIFIP), 1, (50,255,100))
      label2 = myfont1.render(esto, 1, (255,255,255))
      screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+30))
      screen.blit(label3,(((infoObject.current_w - label3.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+60))
      screen.blit(label4,(((infoObject.current_w - label4.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+90))
      myfont1 =pygame.font.SysFont("comicsansms",30)
      msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
      MiMac1=msj.strip()
      MiMac1=MiMac1[0:2]+MiMac1[3:5]+MiMac1[6:8]+MiMac1[9:11]+MiMac1[12:14]+MiMac1[15:17]
      label4 = myfont1.render('To Configure Connect to Wifi', 1, (255,255,255))
      screen.blit(label4,(((infoObject.current_w - label4.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+135))
      label5 = myfont1.render('>> Controller '+str(MiMac1)+' <<', 1, (255,255,255))
      screen.blit(label5,(((infoObject.current_w - label5.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+155))
      #genero qr con nombre de hotspot------------
      nombreh='Controller '+str(MiMac1)
      saveQRCodePNG('qrh',2,nombreh)
      QrImg = pygame.image.load('qrh.png')
      screen.blit(QrImg, (1,245))
      #QrImg = pygame.image.load('qrip.png')
      #screen.blit(QrImg, (185,140))
      
      pygame.display.flip() 
      #pygame.display.update()      
      #DosMundos()
      time.sleep(0.5)
if haywifi==0:
	colorfijo2('25','100','100','100','111')
#---------------------------------------------------------------
# Create a TCP/IP socket
#a=ObtenerMac('wlan0')

if PN=='-' and haywifi==0:
    try:
      #serie.write(bytes([0x25,0x49,0x24,0x92,0x49,0x24,0x97])
      message=str(ObtenerMac('wlan0'))+'$XX'
      while message=='NO$XX': 
        #print('error al conectar')
#        serie.write(bytearray(bytes([0x25,0x6D,0xB6,0xDB,0x6D,0xB6,0xDF])))
        balance='Network Error'
        screen.fill((0,0,0))
        pygame.font.init
        myfont =pygame.font.Font("super.ttf",30)

        bg = pygame.image.load("fondot.jpg")
        screen.blit(bg,(0,0))

        esto=balance#data#str(tarjeta)+str(' Ok')
                                            #label = myfont.render(esto, 1, (0,255,20))
                                            #screen.blit(label, (((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))

        label = myfont.render("Finishing Configuration", 1, (0,255,20))
        screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
        myfont =pygame.font.Font("super.ttf",22)
        label1 = myfont.render('Restarting Controller...', 1, (0,255,20))
        screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
                                           
        pygame.display.flip() 
        #pygame.display.update()
                            #luz2017('rojo')
        colorfijo2('25','111','111','111','100')
        #time.sleep(1.2)
        message=str(ObtenerMac('wlan0'))+'$XX'
    except:
            b=1

#----Obtengo Wifi seteada -E.E.C

confirmacionMudo=False
while confirmacionMudo==False:
        serie.flush()
        serie.write(bytes([0x3A]))
        time.sleep(0.300)    
            
try:
        file=open('/home/pi/ControllerConfig/red.mc','r')
        lista=[]
        for line in file:
            lista.append(line)
        file.close()

        WIFIN=(str(lista[5][10:-2]))
except:
        #WIFIN=''
        b=1

#if estahotspot==0:
#    os.system('sudo ifconfig wlan0 down')
#    os.system('sudo ifconfig wlan0 up')
#    time.sleep(1.5)

#msj=subprocess.check_output("iw dev wlan0 link|grep SSID|awk '{print $2}'",shell=True).decode('utf8')
#esto=msj.strip()
#print(esto)
#myfont1 =pygame.font.SysFont("comicsansms",30)

#if esto=='':
##        Conecto=0
##        #esto="Can't Connect to Wifi "+str(WIFIN)
##        #label2 = myfont1.render(esto, 1, (255,0,0))
##        os.system("sudo systemctl stop dnsmasq -f")
##        os.system("sudo systemctl stop hostapd -f")
##        print("parando servicios")
##        time.sleep(0.15)
##        os.system("sudo cp /home/pi/ControllerConfig/SinWifi.conf /etc/dhcpcd.conf -f")
##        time.sleep(0.15)
##        os.system("sudo systemctl start dnsmasq -f")
##        os.system("sudo systemctl start hostapd -f")
##        time.sleep(0.15)
##else:
##        Conecto=1
tcpserver=0

cambiednsw=0
cambiednsh=0
time.sleep(0.25)

msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
MiMac=msj.strip()
estamaquina=0
contadorwifi=0
msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
MiMac1=msj.strip()
MiMac1=MiMac1[0:2]+MiMac1[3:5]+MiMac1[6:8]+MiMac1[9:11]+MiMac1[12:14]+MiMac1[15:17]
Idioma=0
while PN=='-':
    try:
        
      if True:#Conecto==0:
          msj=subprocess.check_output("iw dev wlan0 link|grep SSID|awk '{print $2}'",shell=True).decode('utf8')
          esto=msj.strip()
          if esto!='':
              Conecto=1
          else:
              Conecto=0
      if Conecto==1:
              
              esto='Connected to ' + esto
              label2 = myfont1.render(esto, 1, (0,255,0))
              if cambiednsw==0:#Conecto==0:
                      colorfijo2('25','001','000','000','001')
                      os.system("sudo systemctl stop dnsmasq -f")
                      os.system("sudo systemctl stop hostapd -f")
                      print("parando servicios")
                      #time.sleep(0.1)
                      os.system("sudo cp /home/pi/ControllerConfig/ConWifi.conf /etc/dhcpcd.conf -f")
                      #time.sleep(0.1)
                      os.system("sudo systemctl start dnsmasq -f")
                      os.system("sudo systemctl start hostapd -f")
                      cambiednsw=1
                      cambiednsh=0
      elif Conecto==0:
              screen.fill((0,0,0))
              pygame.font.init
              myfont =pygame.font.Font("super.ttf",30)
              myfont1 =pygame.font.SysFont("comicsansms",30)
              bg = pygame.image.load("fondot.jpg")
              screen.blit(bg,(0,0))       
              esto="Can't Connect to Wifi "#+str(WIFIN)
              label2 = myfont.render(esto, 1, (255,100,100))
              screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))
              #str(WIFIN)
              label2 = myfont.render(str(WIFIN), 1, (255,100,100))
              screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+40))
              msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
              MiMac1=msj.strip()
              MiMac1=MiMac1[0:2]+MiMac1[3:5]+MiMac1[6:8]+MiMac1[9:11]+MiMac1[12:14]+MiMac1[15:17]
              label4 = myfont1.render('To Configure Connect to Wifi', 1, (255,255,255))
              screen.blit(label4,(((infoObject.current_w - label4.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-65))
              label5 = myfont1.render('>> Controller '+str(MiMac1)+' <<', 1, (255,255,255))
              screen.blit(label5,(((infoObject.current_w - label5.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-45))
              saveQRCodePNG('qrh',3,'Controller '+str(MiMac1))
              QrImg = pygame.image.load('qrh.png')
              screen.blit(QrImg, (180,210))
              if cambiednsh==0:# and contadorwifi==10:#Conecto==0:
                      colorfijo2('25','010','000','000','010')
                      os.system("sudo systemctl stop dnsmasq -f")
                      os.system("sudo systemctl stop hostapd -f")
                      print("parando servicios")
                      #time.sleep(0.1)
                      os.system("sudo cp /home/pi/ControllerConfig/SinWifi.conf /etc/dhcpcd.conf -f")
                      #time.sleep(0.1)
                      os.system("sudo systemctl start dnsmasq -f")
                      os.system("sudo systemctl start hostapd -f")
                      cambiednsw=0
                      cambiednsh=1
                      contadorwifi=0
      
      pygame.display.flip()
      contadorwifi=contadorwifi+1
      if Conecto==1:
          #time.sleep(2.0)
          sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
          server_address = (str(IP),5000)#192.168.2.50 -- IAAPA
          #print('connecting to %s port %s' % server_address)
          sock.settimeout(5.0)
          sock.connect(server_address)
          tcpserver=1
          print('paso conexion '+ str(Idioma))
          ##sock.settimeout(5.0)
          if Idioma==0:
                  message=MiMac+','+IPFINAL+','+NumMaq+'@XX'
                  #print(message)
                  message=str(message)
                  sock.sendall(message.encode(encoding='UTF-8',errors='ignore'))
                  amount_received = 0
                  amount_expected = 1#len(message)
                  data1=''
                  data=''
                  while data=='':
                      data = sock.recv(800)
                  
                  data=data.decode(encoding='UTF-8',errors='ignore')
                  #print(data)
                  a=data#'{ "1001":"Insufficient Balance",  "1002":"Please Wait..",  "1003":"Buscando Tarjeta",  "1004":"Network Error",  "1005":"Repeat Number",  "1006":"Invalid Transaction",  "1007":"Paying",  "1008":"Slide Your Card",  "1009":"To Check your Balance",  "1010":"Inexistent Card",  "1011":"Check TCPServer",  "1012":"Can not Connect to Master PC",  "1013":"For Replace Send:",  "1014":"Login Machine",  "1015":"Attention",  "1016":"You Win !!!" }CA'
                  a=a.strip()
                  b=a[:len(a)-2]
                  data_string =b
                  mijson = json.loads(data_string)
                  print(str(mijson))
                  LabelConsulta1=mijson["1009"]#1009
                  LabelConsulta2=mijson["1010"] #1010
                  LabelPase=mijson["1009"] #1009 
                  LabelPagando=mijson["1008"] #1008
                  LabelPago=mijson["1017"] #1017
                  LabelPorfavor=mijson["1003"] #1003
                  LabelBuscando=mijson["1004"] #1004
                  LabelError=mijson["1005"] #1005
                  LabelNR=mijson["1006"] #1006
                  LabelInvalida=mijson["1007"] #1007
                  LabelCoin=mijson["1001"] #1001
                  LabelNocoin=mijson["1002"] #1002
                  LabelInexistente=mijson["1011"] #1011
                  LabelTCPServer=mijson["1012"] #1012
                  LabelConexion=mijson["1013"] #1013
                  LabelReemplazo=mijson["1014"] #1014
                  LabelLogin=mijson["1015"] #1015
                  LabelAtencion=mijson["1016"] #1016
                  try:
                      LabelInvalidCard=mijson["1018"]#1018
                      LabelInvalidCard2=mijson["1019"]#1018
                  except:
                      b=1
                  try:
                      LabelPagoD=mijson["1020"]#1018
                  except:
                      b=1
                  Idioma=1
                  message='103'
                  sock.sendall(message.encode(encoding='UTF-8',errors='ignore'))
          else:
                  if Reemplazar==0:
                      message=MiMac+','+IPFINAL+','+NumMaq+'$XX'
                  else:
                      message=MiMac+','+IPFINAL+','+NumMaq+'~XX'
                  print(message)
                  message=str(message)
                  sock.sendall(message.encode(encoding='UTF-8',errors='ignore'))
                  amount_received = 0
                  amount_expected = 1#len(message)
                  data1=''
                  data=''
                  while data=='':
                      data = sock.recv(120)
                  data=data.decode('utf8')
                  desde=len(MiMac)+len(MiIp)
                  print(data)
                  #NumMaq=data[7:11]
                  b=data.find(',',1)
                  c=data.find(',',b+1)
                  EMPRESA=data[0:b]#recibo empresa
                  LOCAL=data[b+1:c]#recibo local
                  sumar=len(EMPRESA)+len(LOCAL)+2
                  if data[sumar:sumar+3]=='999':
                      estamaquina=1
                      colorfijo2('25','011','011','111','011')      
                      screen.fill((0,0,0))
                      pygame.font.init
                      myfont =pygame.font.Font("super.ttf",25)
                      bg = pygame.image.load("fondot.jpg")
                      screen.blit(bg,(0,0))
                      esto=LabelAtencion
                      label2 = myfont.render(esto, 1, (255,50,50))
                      screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))
                      #'TCP Server'
                      #if estamaquina==0:
                      #    esto="Check TCP SERVER APP"
                      #else:
                      esto=LabelNR
                      label2 = myfont.render(esto, 1, (255,255,255))
                      screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+30))
                      msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
                      MiMac1=msj.strip()
                      MiMac1=MiMac1[0:2]+MiMac1[3:5]+MiMac1[6:8]+MiMac1[9:11]+MiMac1[12:14]+MiMac1[15:17]
                      label4 = myfont1.render(LabelReemplazo, 1, (255,255,255))
                      screen.blit(label4,(((infoObject.current_w - label4.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+135))
                      label5 = myfont1.render('replace:1', 1, (255,255,255))
                      screen.blit(label5,(((infoObject.current_w - label5.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+155))
                      myfont3 =pygame.font.Font("super.ttf",10)
                      label4 = myfont3.render('SN:'+str(IPFINAL[3:]), 1, (0,255,255))
                      screen.blit(label4,(415,310))
                      saveQRCodePNG('qrip',2,str(IPFINAL))
                      QrImg = pygame.image.load('qrip.png')
                      screen.blit(QrImg, (1,245))
                      pygame.display.flip()
                  if len(data)>30:    
                      PN=data[(7+sumar):(14+sumar)]
                      PV=data[(14+sumar):(21+sumar)]
                      Pulso=data[(21+sumar):(22+sumar)]
                      Multiplicador=str(data[(22+sumar):(29+sumar)])
                      Multiplicador=int(float(Multiplicador.strip()))
                      print(str(Multiplicador))
                      Maximo=str(data[(29+sumar):(36+sumar)])
                      Maximo=int(float(Maximo.strip()))
                      print(str(Maximo))
                      #PN=data[11:18]
                      #PV=data[18:25]
                      #print(EMPRESA)
                      #print(LOCAL)
                      #print(NumMaq + "," + PN +"," + PV)
                      ok1='103'
                      #message=str(message)
                      sock.sendall(ok1.encode(encoding='UTF-8',errors='ignore'))
                      #controlluces=0
                      colorfijo2('25','111','001','001','001')      
                      screen.fill((0,0,0))
                      pygame.font.init
                      myfont =pygame.font.Font("super.ttf",35)

                      bg = pygame.image.load("fondot.jpg")
                      screen.blit(bg,(0,0))

                      esto=str(NumMaq)#ObtenerMac('wlan0')
                      label = myfont.render(LabelLogin, 1, (0,255,20))
                      screen.blit(label,(((infoObject.current_w - label.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)-40))
                      myfont =pygame.font.Font("super.ttf",30)
                      label1 = myfont.render(esto, 1, (255,255,255))
                      screen.blit(label1,(((infoObject.current_w - label1.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+20))
                      myfont1 =pygame.font.SysFont("comicsansms",30)

                  
                      screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+50))
                      label3 = myfont1.render(str(IPFINAL), 1, (0,255,0))
                      screen.blit(label3,(((infoObject.current_w - label3.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+75))
                  
                      pygame.display.flip() 
                      #pygame.display.update()      
                      #DosMundos()
                  
                      #time.sleep(1)

    except:
          if contadorwifi >= 5:
        
        
              colorfijo2('25','011','011','111','011')      
              screen.fill((0,0,0))
              pygame.font.init
              myfont =pygame.font.Font("super.ttf",25)
              bg = pygame.image.load("fondot.jpg")
              screen.blit(bg,(0,0))
              esto=LabelConexion
              label2 = myfont.render(esto, 1, (255,50,50))
              screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)))
              #'TCP Server'
              if estamaquina==0:
                  esto=LabelTCPServer
              else:
                  esto=LabelNR
              label2 = myfont.render(esto, 1, (255,255,255))
              screen.blit(label2,(((infoObject.current_w - label2.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+30))
              msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
              MiMac1=msj.strip()
              MiMac1=MiMac1[0:2]+MiMac1[3:5]+MiMac1[6:8]+MiMac1[9:11]+MiMac1[12:14]+MiMac1[15:17]
              if estamaquina!=0:
                  label4 = myfont1.render(LabelReemplazo, 1, (255,255,255))
                  screen.blit(label4,(((infoObject.current_w - label4.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+135))
                  label5 = myfont1.render('replace:1', 1, (255,255,255))
                  screen.blit(label5,(((infoObject.current_w - label5.get_width()) / 2), ((infoObject.current_h - label.get_height()) / 2)+155))
              myfont3 =pygame.font.Font("super.ttf",10)
              label4 = myfont3.render('SN:'+str(IPFINAL[3:]), 1, (0,255,255))
              screen.blit(label4,(415,310))
              saveQRCodePNG('qrip',3,str(NumMaq)+'@'+str(IPFINAL))
              QrImg = pygame.image.load('qrip.png')
              screen.blit(QrImg, (180,210))
              pygame.display.flip()
              time.sleep(1.5)
              cambiednsw=0
              cambiednsh=0
              contadorwifi=0
#--------------------------
#if LOCAL=='0' and EMPRESA=='0':
#    saveQRCodePNG('qr',4,NumMaq+'@'+IPFINAL+'*'+ trajepago) #genero QR con numero de maquina
#else:
saveQRCodePNG('qr',4,NumMaq+LOCAL+'#'+EMPRESA+'@'+IPFINAL+'*'+trajepago) #genero QR con numero de maquina
PantFull()
#programafinal-----------------------------


#HILOS
#listen

TimeOutTk=threading.Thread(target=TiempoTk)
TimeOutTk.start()
#tpago=threading.Thread(target=MuestroPagoTk)
#tpago.start()
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
        serie.flush()
        serie.write(bytes(activot))
        time.sleep(0.3)
        #print('tarjeta inactiva')            
#print('tarjeta activa')
            
confirmacionMudo=False
while confirmacionMudo==False:
        serie.flush()
        serie.write(bytes([0x3A]))
        time.sleep(0.300)        
        #EnvioPuerto=
controlluces=1
TerminoOperacion=1
serie.flush
serie.write(pcorto)
# Create a TCP/IP socket
                   
#sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

#luces

luz=threading.Thread(target=MiLuz())#MiLuz #Luz2018-giro Manual
luz.start()
#EnvioPuerto=bytearray([0x25,0x09,0x00,0x09,0x00,0x49,0xF8])#bytearray([0x28,0x10,0x11,0x01,0x02,0x05,0x09])#bytes(a)
#------------------------------------------
#screen.fill((0,0,0))



    

#ventana.mainloop()






