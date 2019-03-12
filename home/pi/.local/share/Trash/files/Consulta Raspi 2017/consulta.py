from tkinter import *
from tkinter.font import Font
import socket
import sys
import json
import serial 

serie = serial.Serial('/dev/serial0', 9600, timeout=0.2, writeTimeout=0)

def leotarjeta():
    dato = serie.readline(15)
    if len(dato.decode('utf-8'))>14:
        #print (dato)
        #tarjeta.config(text = dato)
        tarjeta.insert(0,dato)
        print(tarjeta.get())
        MiSaldo()
        tarjeta.delete(0,END)
        #MiSaldo()
    tarjeta.after(1, leotarjeta)



def jdefault(o):
    if isinstance(o, set):
        return list(o)
    return o.__dict__



def get(event):
    #Saldo.config(text = "tengo el enter")
    que=tarjeta.get()
    if que == "exit":
        Salir()
    MiSaldo()
    tarjeta.delete(0,END)

def resize_image(event):
    new_width = event.width
    new_height = event.height
    image = copy_of_image.resize((new_width, new_height))
    photo = ImageTk.PhotoImage(image)
    label.config(image = photo)
    label.image = photo #avoid garbage collection

def Salir():
    ventana.destroy()

def MiSaldo():
    if tarjeta.get():

        try:
               # Create a TCP/IP socket
                sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

                # Connect the socket to the port where the server is listening
                server_address = ('192.168.1.207', 123)#'181.164.238.87'
                print('connecting to %s port %s' % server_address)
                sock.settimeout(5.0)
                sock.connect(server_address)
                

                
            
                    # Send data
                message = 'C' + tarjeta.get() #'C;1921000005618'
                print('sending "%s"' % message)
                sock.send(message.encode())

                # Look for the response
                amount_received = 0
                amount_expected = 1#len(message)
            
                while amount_received < amount_expected:
                    data = sock.recv(200)
                    print(data)
                    amount_received += len(data)
                    #encoded
                    #data_string = json.dumps(data.decode())

                    #Decoded
                    #decoded = json.loads("{'credito': '75.1'}")
                    data1=data.decode()
                    
                    #print(data1.lstrip("{"))

                    if data1=='no':
                        Saldo.config(text ="New card")
                        bonus.config(text ="")
                        tk.config(text ="")
                    else:
                        
                        json_data =data1#'[{"name": "Brian", "city": "Seattle"}]'
                        python_obj = json.loads(json_data)
                        Saldo.config(text ="Credits: $ "+python_obj[4]["credito"])
                        bonus.config(text ="Bonus: $ "+python_obj[5]["bonus"])
                        tk.config(text ="Tk: " + python_obj[6]["tk"])

                        print(python_obj[4]["credito"])
                        print(python_obj[5]["bonus"])
                        print(python_obj[6]["tk"])
                    
                    
                    #pets = set(data_string)
                    #print(json.dumps(pets, default=jdefault))
                    #esto=decoded
                    #print ("Tenemos "+ decoded["credito"])
                    #esto=json.dumps(data_string, default=jdefault)#str(decoded['credito'])
                    #print('received "%s"' % esto)
                   # Saldo.text=
        except:
            print('error al conectar')

        finally:
            print('closing socket')
            sock.close()
            
    
    else:
        Saldo.config(text = 'Sin tarjeta...')
ventana = Tk()
ventana.attributes("-fullscreen", True)
photo=PhotoImage(file="metal.gif")
copy_of_image = photo.copy()
ventana.configure(background='black')



label = Label(ventana,image = photo)
#ventana.image = photo # keep a reference!
#label.grid(row=0,column=0,columnspan=10,rowspan=10)
#label.place(x =0, y = 0,width=ventana.winfo_screenwidth())

label.bind('<Configure>', resize_image)
label.pack(fill=BOTH, expand = YES)


#background_image=PhotoImage('fondo.png')
#ventana.geometry("600x300+0+0")
ventana.title("Consulta de Saldo")

tarjeta = Entry(ventana, text = "",justify='center',fg="white", font=("Helvetica", 14),background='black')
tarjeta.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = 2000,width=ventana.winfo_screenwidth())
tarjeta.focus()
tarjeta.bind('<Return>', get)

titulo = Label(ventana, text = "Check Balance Magnetic Cash 2017",fg="white", font=("white", 20),background='black')
titulo.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = 0,width=ventana.winfo_screenwidth())

Saldo = Label(ventana, text = "",fg="black", font=("black", 40),background='#B8E266')
Saldo.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = ventana.winfo_screenheight()/2-85,width=ventana.winfo_screenwidth())
bonus = Label(ventana, text = "",fg="black", font=("black", 40),background='#B8E266')
bonus.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = ventana.winfo_screenheight()/2+60-85,width=ventana.winfo_screenwidth())
tk = Label(ventana, text = "",fg="black", font=("black", 40),background='#B8E266')
tk.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = ventana.winfo_screenheight()/2+120-85,width=ventana.winfo_screenwidth())

#eze = Label(ventana, text = "Programmed by Ezequiel.Ciccotelli",fg="grey", font=("black", 10),background='black')
eze = Label(ventana, text = "Magnetic cash 2017",fg="grey", font=("black", 10),background='black')
eze.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = ventana.winfo_screenheight()-20,width=ventana.winfo_screenwidth())


#btn_estado = Button(ventana, text = "Consultar Saldo", command = MiSaldo)
#btn_estado.place(x = (ventana.winfo_screenwidth())/2-100, y = 100,width=200)

#bsalir = Button(ventana, text = "Salir", command = Salir)
#bsalir.place(x = (ventana.winfo_screenwidth())/2-100, y = 70,width=200)

tarjeta.after(1, leotarjeta)
ventana.mainloop()
