import os
import subprocess

msj=subprocess.check_output("wpa_cli",shell=True).decode('utf8')
esto=msj.strip()
print(str(esto))

msj=subprocess.check_output("wpa_cli add_network",shell=True).decode('utf8')
esto=msj.strip()
print(str(esto))

msj=subprocess.check_output('wpa_cli set_network 0 ssid "MagneticashAP"',shell=True).decode('utf8')
esto=msj.strip()
print(str(esto))

msj=subprocess.check_output('wpa_cli set_network 0 psk "5050505050"',shell=True).decode('utf8')
esto=msj.strip()
print(str(esto))

msj=subprocess.check_output("wpa_cli select_network 0",shell=True).decode('utf8')
esto=msj.strip()
print(str(esto))

