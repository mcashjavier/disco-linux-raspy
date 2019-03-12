import os
import sys
import time

#---Script Creado para cambiar y dar de alta wifi -E.E.C
os.system("sudo systemctl stop dnsmasq")
os.system("sudo systemctl stop hostapd")
print("parando servicios")
time.sleep(0.5)

try:
    #para red
    os.system("sudo python3 /home/pi/ControllerConfig/configv.py")
    os.system("sudo cp /home/pi/ControllerConfig/redf.mc /etc/wpa_supplicant/wpa_supplicant.conf")
    os.system("sudo cp /home/pi/ControllerConfig/ConWifi.conf /etc/dhcpcd.conf")
    
    #/etc/hostapd/hostapd.con
    #para hotspot
    #os.system("sudo cp /etc/wpa_supplicant/wpa_supplicantMarcos.conf /etc/wpa_supplicant/wpa_supplicant.conf")
    #os.system("sudo cp hotspot.conf /etc/hdcpcd.conf")
    #os.system("sudo cp hotspot.conf /etc/hostapd/hostapd.conf")
    #os.system("sudo cp SinWifi.conf /etc/dhcpcd.conf")
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
#time.sleep(0.5)
#cmd='sudo ifconfig wlan0 up'
#os.system(cmd)
#print('wifi encendido')
#os.system("sudo reboot")
    
print("End of Script")
