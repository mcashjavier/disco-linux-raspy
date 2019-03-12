import os
import sys
import time
import subprocess

#---Script Creado para cambiar y dar de alta wifi -E.E.C
os.system("sudo systemctl stop dnsmasq")
os.system("sudo systemctl stop hostapd")
print("parando servicios")
time.sleep(1.0)

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
	print('error al borrar archivo')
time.sleep(0.5)
try:
        file= open("/home/pi/hotspot.mc","w")
        file.write("interface=wlan0\n")
        file.write("driver=nl80211\n")
        file.write("ssid=Controller " + str(MiMac) +"\n")
        file.write("hw_mode=g\n")
        file.write("channel=7\n")
        file.write("wmm_enabled=0\n")
        file.write("macaddr_acl=0\n")
        file.write("auth_algs=1\n")
        file.write("ignore_broadcast_ssid=0\n")
        file.write("wpa=2\n")
        file.write("wpa_passphrase=Controller12345678\n")
        file.write("wpa_key_mgmt=WPA-PSK\n")
        file.write("wpa_pairwise=TKIP\n")
        file.write("rsn_pairwise=CCMP\n")
        file.close()
        print('archivo de config creado ok')
except:
        print('no pudo crear el archivo')
#time.sleep(15.0)

while os.path.getsize("/home/pi/hotspot.mc")==0:
        time.sleep(0.5)
        print('esperando q se guarde')
print('guardado ok')
#print(str(os.path.getsize("hotspot.mc")))
try:
    #para red
    #os.system("sudo cp red.conf /etc/wpa_supplicant/wpa_supplicant.conf")
    #os.system("sudo cp ConWifi.conf /etc/dhcpcd.conf")
    
    #/etc/hostapd/hostapd.con
    #para hotspot
    os.system("sudo cp /etc/wpa_supplicant/wpa_supplicantMarcos.conf /etc/wpa_supplicant/wpa_supplicant.conf")
    os.system("sudo cp /home/pi/hotspot.mc /etc/hdcpcd.conf")
    os.system("sudo cp /home/pi/hotspot.mc /etc/hostapd/hostapd.conf")
    os.system("sudo cp /home/pi/ControllerConfig/SinWifi.conf /etc/dhcpcd.conf")
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

    
print("End of Script")
time.sleep(3.0)
os.system("sudo reboot")
#exit
