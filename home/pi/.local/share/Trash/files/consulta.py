from tkinter import *
import socket
import sys

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
            message = tarjeta.get() #'C;1921000005618'
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

tarjeta = Entry(ventana, text = "-")
tarjeta.place(x = (ventana.winfo_screenwidth())/2-250, y = 140,width=500)

Saldo = Label(ventana, text = "-")
Saldo.place(x = (ventana.winfo_screenwidth())/2-250, y = 160,width=500)

btn_estado = Button(ventana, text = "Consultar Saldo", command = MiSaldo)
btn_estado.place(x = (ventana.winfo_screenwidth())/2-100, y = 100,width=200)

ventana.mainloop()