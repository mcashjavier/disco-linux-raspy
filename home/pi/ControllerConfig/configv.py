import os
import sys
import time
import subprocess

#---Script Creado para cambiar y dar de alta wifi -E.E.C
os.system("sudo systemctl stop dnsmasq")
os.system("sudo systemctl stop hostapd")
print("parando servicios")
#time.sleep(1.0)

#creo archivo de configuracion de red hotspot
msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
MiMac=msj.strip()
print(MiMac)
MiMac=MiMac[0:2]+MiMac[3:5]+MiMac[6:8]+MiMac[9:11]+MiMac[12:14]+MiMac[15:17]
print(MiMac)
try:
	os.remove("/home/pi/hotspot.mc")
	
	print("archivo hotspot borrado Ok")
except:
	print('error al borrar archivo hotspot')
try:
	os.remove("/home/pi/hotspoton.mc")
	
	#print("archivo hotspot borrado Ok")
except:
	print('error al borrar archivo hotspoton')

try:
	os.remove("/home/pi/ControllerConfig/ledmode.mc")
	
	#print("archivo hotspot borrado Ok")
except:
	print('error al borrar archivo ledmode')

try:
	os.remove("/home/pi/ControllerConfig/mmc.mc")
	
	#print("archivo hotspot borrado Ok")
except:
	print('error al borrar archivo mmc')
	
try:
        os.remove("/home/pi/ControllerConfig/num.mc")	
	#print("archivo num borrado Ok")
except:
	print('error al borrar archivo num')
try:
        os.remove("/home/pi/ControllerConfig/hip.mc")	
	#print("archivo num borrado Ok")
except:
	print('error al borrar archivo hip.mc')

try:
        os.remove("/home/pi/ControllerConfig/pconf.mc")	
	#print("archivo num borrado Ok")
except:
	print('error al borrar archivo pconf.mc')

try:
        os.remove("/home/pi/ControllerConfig/mip.mc")
	
	#print("archivo miip borrado Ok")
except:
	print('error al borrar archivo mip')
try:
        os.remove("/home/pi/ControllerConfig/red.mc")
	
	#print("archivo red borrado Ok")
except:
	print('error al borrar archivo red')
try:
        os.remove("/home/pi/ControllerConfig/ConWifi.conf")
	
	#print("archivo red borrado Ok")
except:
	print('error al borrar archivo ConWifi')


time.sleep(0.5)

#print(str(os.path.getsize("hotspot.mc")))
try:
    #para red
    #os.system("sudo cp red.conf /etc/wpa_supplicant/wpa_supplicant.conf")
    #os.system("sudo cp ConWifi.conf /etc/dhcpcd.conf")
    
    #/etc/hostapd/hostapd.con
    #para hotspot
    os.system("sudo cp  /home/pi/ControllerConfig/wpa_vacio.mc /etc/wpa_supplicant/wpa_supplicant.conf")
    #os.system("sudo cp /home/pi/hotspot.mc /etc/hdcpcd.conf")
    #os.system("sudo cp /home/pi/hotspot.mc /etc/hostapd/hostapd.conf")
    os.system("sudo cp /home/pi/ControllerConfig/sinip.conf /etc/dhcpcd.conf")
    print("network copied Ok")
except:
    print("can't copied network file")
finally:
    b=1
    #os.system("sudo service dhcpcd restart")
#time.sleep(2.0)
#os.system("sudo dhclient -r wlan0")
#os.system("sudo ifdown wlan0")
#print("bajando red")
#time.sleep(1.0)
#os.system("sudo ifup wlan0")
#os.system("sudo dhclient -v wlan0")
#print("subiendo red")
#time.sleep(2.0)
#print("wlan reconfigurada")

#print("Reiniciando  Wifi")
#cmd='sudo ifconfig wlan0 down'
#os.system(cmd)
#print('wifi apagado')
#time.sleep(5.0)
os.system("sudo systemctl start dnsmasq")
os.system("sudo systemctl start hostapd")

#cmd='sudo ifconfig wlan0 up'
#os.system(cmd)
#print('wifi encendido')

print('Conectarse a INITCONTROLLERS y reiniciar!')    
print("End of Script")
#time.sleep(1.0)
#os.system("sudo reboot")
exit
