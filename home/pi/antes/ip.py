import os
import socket
global MiIp
global MiMac

import subprocess

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
    return MiIp#str[0:17]+ ','+ ipaddr + ','+ NumMaq
  except:
    str = "00:00:00:00:00:00"
    return 'NO'#str[0:17]+ ','+ ipaddr + ','+ NumMaq

msj=subprocess.check_output("hostname -I",shell=True).decode('utf8')
print(msj.strip())
msj=subprocess.check_output("cat /sys/class/net/wlan0/address",shell=True).decode('utf8')
MiMac=msj.strip()
print(MiMac)
MiMac=MiMac[0:2]+MiMac[3:5]+MiMac[6:8]+MiMac[9:11]+MiMac[12:14]+MiMac[14:16]
print(MiMac)
