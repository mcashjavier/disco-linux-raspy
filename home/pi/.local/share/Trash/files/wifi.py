import os
import time
cmd='sudo ifconfig wlan0 down'
os.system(cmd)
print('1')
time.sleep(3.0)
cmd='sudo ifconfig wlan0 up'
os.system(cmd)
print('2')
