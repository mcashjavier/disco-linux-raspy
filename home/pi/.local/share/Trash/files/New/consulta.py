from tkinter import *
from tkinter.font import Font
import socket
import sys
import json



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
       # Create a TCP/IP socket
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        # Connect the socket to the port where the server is listening
        server_address = ('181.164.238.87', 123)#'181.164.238.87'
        print('connecting to %s port %s' % server_address)
        sock.connect(server_address)

        try:
    
            # Send data
            message = 'C' + tarjeta.get() #'C;1921000005618'
            print('sending "%s"' % message)
            sock.send(message.encode())

            # Look for the response
            amount_received = 0
            amount_expected = 1#len(message)
        
            while amount_received < amount_expected:
                data = sock.recv(100)
                amount_received += len(data)
                #encoded
                #data_string = json.dumps(data.decode())

                #Decoded
                #decoded = json.loads("{'credito': '75.1'}")
                data1=data.decode()
                
                #print(data1.lstrip("{"))
                
                json_data =data1#'[{"name": "Brian", "city": "Seattle"}]'
                python_obj = json.loads(json_data)
                print(python_obj[0]["credito"])
                print(python_obj[1]["bonus"])
                print(python_obj[2]["tk"])
                
                
                #pets = set(data_string)
                #print(json.dumps(pets, default=jdefault))
                #esto=decoded
                #print ("Tenemos "+ decoded["credito"])
                #esto=json.dumps(data_string, default=jdefault)#str(decoded['credito'])
                #print('received "%s"' % esto)
               # Saldo.text=
                Saldo.config(text ="Credits: $ "+python_obj[0]["credito"])
                bonus.config(text ="Bonus: $ "+python_obj[1]["bonus"])
                tk.config(text ="Tk: " + python_obj[2]["tk"])

        finally:
            print('closing socket')
            sock.close()
            
    
    else:
        Saldo.config(text = 'Escribi algo antes de mandar!!! *-*')
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
#label.pack(fill=BOTH, expand = YES)


#background_image=PhotoImage('fondo.png')
#ventana.geometry("600x300+0+0")
ventana.title("Consulta de Saldo")

tarjeta = Entry(ventana, text = "-",justify='center',fg="white", font=("Helvetica", 14),background='black')
tarjeta.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = 2000,width=ventana.winfo_screenwidth())
tarjeta.focus()
tarjeta.bind('<Return>', get)

titulo = Label(ventana, text = "Controller Magnetic Cash 2017",fg="green", font=("black", 20),background='black')
titulo.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = 0,width=ventana.winfo_screenwidth())

Saldo = Label(ventana, text = "",fg="green", font=("black", 40),background='black')
Saldo.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = ventana.winfo_screenheight()/2-60,width=ventana.winfo_screenwidth())
bonus = Label(ventana, text = "",fg="red", font=("black", 40),background='black')
bonus.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = ventana.winfo_screenheight()/2+60-60,width=ventana.winfo_screenwidth())
tk = Label(ventana, text = "",fg="yellow", font=("black", 40),background='black')
tk.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = ventana.winfo_screenheight()/2+120-60,width=ventana.winfo_screenwidth())

eze = Label(ventana, text = "Programmed by Ezequiel.Ciccotelli",fg="grey", font=("black", 10),background='black')
eze.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = ventana.winfo_screenheight()-20,width=ventana.winfo_screenwidth())


#btn_estado = Button(ventana, text = "Consultar Saldo", command = MiSaldo)
#btn_estado.place(x = (ventana.winfo_screenwidth())/2-100, y = 100,width=200)

#bsalir = Button(ventana, text = "Salir", command = Salir)
#bsalir.place(x = (ventana.winfo_screenwidth())/2-100, y = 70,width=200)

ventana.mainloop()