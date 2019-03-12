from tkinter import *
import socket
import sys

def Salir():
    ventana.destroy()

def MiSaldo():
    if tarjeta.get(): 
       # Create a TCP/IP socket
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        # Connect the socket to the port where the server is listening
        server_address = ('181.164.238.87', 123)
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
                print('received "%s"' % data.decode())
               # Saldo.text=
                Saldo.config(text = data.decode())

        finally:
            print('closing socket')
            sock.close()
    
    else:
        Saldo.config(text = 'Escribi algo antes de mandar!!! *-*')
ventana = Tk()
ventana.attributes("-fullscreen", True)
ventana.configure(background='black')
#ventana.geometry("600x300+0+0")
ventana.title("Consulta de Saldo")

tarjeta = Entry(ventana, text = "-",justify='center',fg="white", font=("Helvetica", 14),background='black')
tarjeta.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = 140,width=ventana.winfo_screenwidth())

Saldo = Label(ventana, text = "-",fg="green", font=("Helvetica", 16),background='black')
Saldo.place(x = (ventana.winfo_screenwidth())/2-(ventana.winfo_screenwidth()/2), y = 160,width=ventana.winfo_screenwidth())

btn_estado = Button(ventana, text = "Consultar Saldo", command = MiSaldo,)
btn_estado.place(x = (ventana.winfo_screenwidth())/2-100, y = 100,width=200)

bsalir = Button(ventana, text = "Salir", command = Salir)
bsalir.place(x = (ventana.winfo_screenwidth())/2-100, y = 200,width=200)

ventana.mainloop()