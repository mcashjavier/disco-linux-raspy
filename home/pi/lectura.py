import os

MiMac='aa:aa:aa:aa:aa:aa'
MiIp='10.0.0.120'
traje='aa:aa:aa:aa:aa:aa,10.0.0.120,3000020;12345678901234'
print(traje[17+len(MiIp)+2:17+len(MiIp)+2+3])

print(traje.find(',300'))
tarjeta=traje[traje.find(',300')+8:traje.find(',300')+8+14]+'?'
print(tarjeta)
