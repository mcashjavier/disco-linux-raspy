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

serie = serial.Serial('/dev/serial0', 9600, timeout=0.2, writeTimeout=0)
M1=[0x2A,0X49,0x24,0X97,0x49,0x24,0x97]
M2=[0x26]
M3=[0x27]
a=bytearray(M1)
b=bytearray(M2)
c=bytearray(M3)
def luz():
    while True:
        print(bytes(a))
        serie.write(bytes(a))
        serie.flushOutput()
        time.sleep(1)
        #print(bytes(b))
        #serie.write(bytes(b))
        #time.sleep(1)
        #print(bytes(c))
        #serie.write(bytes(c))
        #time.sleep(1)
        
luz()

