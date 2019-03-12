#
import sys
import os
import json
a='{ "1001":"Insufficient Balance",  "1002":"Please Wait..",  "1003":"Buscando Tarjeta",  "1004":"Network Error",  "1005":"Repeat Number",  "1006":"Invalid Transaction",  "1007":"Paying",  "1008":"Slide Your Card",  "1009":"To Check your Balance",  "1010":"Inexistent Card",  "1011":"Check TCPServer",  "1012":"Can not Connect to Master PC",  "1013":"For Replace Send:",  "1014":"Login Machine",  "1015":"Attention",  "1016":"You Win !!!" }CA'
a=a.strip()
b=a[:len(a)-2]
data_string =b
data = json.loads(data_string)
print(data["1001"])
