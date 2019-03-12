import os
WIFIN='CHOTO'
IPFINAL='123.123.123.123'
file=open("/home/pi/ControllerConfig/eze.conf","w")
file.write('option domain_name_servers, domain_name, domain_search, host_name'+'\n')
file.write('option classless_static_routes'+'\n')
file.write('option ntp_servers'+'\n')
file.write('require dhcp_server_identifier'+'\n')
file.write('nohook lookup-hostname'+'\n')
file.write('SSID="'+str(WIFIN)+'"'+'\n')
file.write('inform '+str(IPFINAL)+'\n')
file.write('static routers=10.0.0.1'+'\n')
file.write('static domain_name_servers=10.0.0.1'+'\n')
file.write('static domain_search=8.8.8.8')
file.flush()
file.close()
print('termino')
#os.system('sudo reboot')
