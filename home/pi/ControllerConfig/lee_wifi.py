import os

file=open('red.mc','r')
lista=[]
for line in file:
    lista.append(line)
file.close()

print(str(lista[5][10:-2]))

a='b8:27:eb:ac:df:50,1,25,11A00080.50   0.45   4000000000000000000000000XX'
aa=a.find(',')+1
b=a.find(',',aa)
c=a.find(',',b+1)
print(b)
print(a[18:b])
print(a[b+1:c])

a='35,40,01000890.50   0.45   400000000000000000000000015'

b=a.find(',',1)
c=a.find(',',b+1)
print(b)
print(a[0:b])
print(a[b+1:c])
