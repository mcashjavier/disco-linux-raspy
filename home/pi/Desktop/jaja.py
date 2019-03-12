# Tcp Chat server
from datetime import datetime, date, time, timedelta
import random 
import socket, select
from time import sleep
global trabajando
global ocupado
global ocupadoCelu 
from datetime import datetime
global ServerOcupado
global totalpeticiones
global MiFecha
global FechaReinicio
global Reiniciar
MiFecha=''
FechaReinicio=''
b=[]
ocupado=0
ocupadoCelu=0
ServerOcupado=0
totalpeticiones=0
global ConError 
ConError="No"

def BorroInutiles():
	try:
		for o in range(len(CONNECTION_LIST)):
			esta=0
			for i in range(len(Con)):
				if Con[i][1]==str(CONNECTION_LIST[o]) and CONNECTION_LIST[o] !=server_socket:
					esta=1
					##print "esta con"
			for i in range(len(Locales)):
				if Locales[i][4]==CONNECTION_LIST[o] and CONNECTION_LIST[o] !=server_socket:
					esta=1
					##print "esta locales"
			if esta==0:
				# if CONNECTION_LIST[i]==server_socket:
				if CONNECTION_LIST[o] !=server_socket:
					##print "es igual borra"#Con
					
					BorroConexion(str(CONNECTION_LIST[o]))#03/05/18 para borrar conexiones viejas
					
					CONNECTION_LIST.remove(CONNECTION_LIST[o])


	except:
		print ("salio por error en borroinutiles")
		return

def BorrarComunicacion(id,s):
	try:
		for i in range(len(Con)):
			#for j in range(len(Con[i])-2): #Obtengo IDCONEXION
			if Con[i][0]==id:# or Con[i][2]==id: #comparo con id de conexion
					Con.remove(Con[i])
					#print str(datetime.now())
					CONNECTION_LIST.remove(s) # agregue para que borre conexion al borrar
					s.close()
					return("Delete Ok")
		return("NO")
	except:
		print ("No Pudo Borrar Conexion")
		return("NO")
		#BorrarComunicacion(id,s)

def BorrarLocal(id):
	try:
		for i in range(len(Locales)):
			for j in range(len(Locales[i])-2): #Obtengo IDCONEXION
				if Locales[i][j]==id: #comparo con id de conexion
					Locales.remove(Locales[i])
					return("Server Delete Ok")
		return("NO")
	except:
		print ("No Pudo Borrar Local")
		return("NO")
		#BorrarLocal(id)
	
def BorroConexion(elsocket):#general
	try:
		for i in range(len(Con)):
			#for j in range(len(Con[i])-1): #Obtengo IDCONEXION
			if Con[i][1]==elsocket or Con[i][2]==elsocket: #comparo con id de conexion
				Con.remove(Con[i])

		for i in range(len(Locales)):
			#for j in range(len(Locales[i])-1): #Obtengo IDCONEXION
			if Locales[i][3]==elsocket: #comparo con id de conexion
				Locales.remove(Locales[i])

		#return
	except:
		print ("No pudo borrar conexion")
		#return


def IntelCon():
    global Conexiones
    c=random.randint(1, 99999)
    return(int(c))
	            
def PedidoEmpresa(e,l,t,idc):
	global Conexiones
	global ocupadoCelu
	ccc = random.uniform(0.001, 0.025)
	sleep(ccc)
	for i in range(len(Locales)):

		if int(Locales[i][0])==int(e):#if int(Locales[i][j])==int(e):
			if int(Locales[i][1])==int(l) and str(Locales[i][2])==str(t[:1]): #Comparo Local

				eze=IntelCon()
					###print(eze)
				Con.append(["%05d" % eze,idc,Locales[i][3],t[:1]]) #Agrego Conexion con id

				return "%05d" % eze
	return("NO")
	#ocupadoCelu=0
	#return("NO")
    


def AgregarEmpresaLocal(ide,idl,t,ids,msocket):
	igual=0
	cual=0
	global ocupado
	
	#print CONNECTION_LIST
	try:
		ccc = random.uniform(0.01, 0.5)
		sleep(ccc)
		if ocupado == 1:
			ccc = random.uniform(0.01, 0.5)
			sleep(ccc)
			return("NO")
		ocupado=1	
		for i in range(len(Locales)):
			#for j in range(len(Locales[i])): #Obtengo Empresa
					#if int(Locales[i][j])==int(e):
				#print str(Locales[i][0] + ' ' + Locales[i][1] + ' '+ Locales[i][2])
			if Locales[i][0] == ide and Locales[i][1] == idl and  Locales[i][2] == t[:1]:
				
					##print Locales [i]#[j]
					##print Locales [i:j]
					##print Locales [i,j]
					#Locales [i][j]
					#print 'entro ' + str(i)
					igual=1
					cual=i
					#print str(Locales[i][3])
					
					#print "SOBREESCRIBO----------------------------------"
					#print "anterior: " + str(Locales[i][4]) + " nuevo: " + str(msocket) + " tipo " + str(Locales[i][2])
					#for p in range(len(CONNECTION_LIST)):
					#	if CONNECTION_LIST[p] == ParaBorrar:# and str(Locales[i][3])!= ids :
					ParaBorrar=Locales[i][4]
					Locales.remove(Locales[cual])
					CONNECTION_LIST.remove(ParaBorrar)
					
		#print "LOCALES------------------------------------"
		#print Locales
		#print "LISTA--------------------------------------"
					#print CONNECTION_LIST
		#Locales.append([ide,idl,t[:1],ids,msocket])
		#print "Guardo bien local"+str(ide) + " " + str(idl)
		#ocupado=0
		#return("OK")
					
					#		try:

					#CONNECTION_LIST.pop(p)#remove(Locales[i][4])
					#		except:
					#			continue
					#CONNECTION_LIST.remove(Locales[i][3])
					
					#continue

	except:
		#print("Error inesperado:", sys.exc_info()[0])
		#raise
		print ("salio x error "+str(ide) + " " + str(idl))
		a=1
		ocupado=0
		return("NO")
		#continue
	
	finally:
		Locales.append([ide,idl,t[:1],ids,msocket])
		print ("Guardo bien local "+str(ide) + " " + str(idl))
		ocupado=0
		return("OK")
	# if len(Locales)==1 and igual==1:
		# try:
			# Locales.remove(Locales[i])
		# except:
			# a=1
	# if cual>1 and igual==1:
		# try:
			# Locales.remove(Locales[i])
		# except:
			# a=1
	#finally:
		



	

    
def IdentificoConexion(recibi,so,so1):
    #d=[]
    
    b=recibi
    #print b[0][1] + " " + b[0][2] + " " + b[0][3]
    if(b[0][0])=='C':
        if len(Con) > 0:
            for o in range(len(Con)):
                if Con[o][1]==so:
                    #print "es igual borra conexion"#Con
                    Con.remove(Con[o])
        return(PedidoEmpresa(b[0][1],b[0][2],b[0][3],so))#empresa,#local,#socket
            
    if (b[0][0])=='S':
		
        return(AgregarEmpresaLocal(b[0][1],b[0][2],b[0][3],so,so1))
            
    else:

        return(b)
 
#Function to broadcast chat messages to all connected clients
def broadcast_data (sock, message,ids,cuanto):
    #Do not send the message to master socket and the client who has send us the message
	#x=0
    global trabajando
    global ocupadoCelu
    global ServerOcupado
    global ConError
    global totalpeticiones
    global FechaReinicio
    global Locales
    global Con
    #global totalpeticiones
    #global CONNECTION_LIST
    for socket in CONNECTION_LIST:

			
		if message[0][0] == "DS" and message[0][2]=="1":
			return

		b=message

		if (b[0][0])=='locales':
			if socket != server_socket and socket == sock : #socket != server_socket and
				try :

					totalpeticiones=totalpeticiones+1
					
					#bb="Cantidad de EyL =" + str(len(Locales))+  " Cant de Conexiones =" + str(len(Con))+ " Cant de Sock "+ str(len(CONNECTION_LIST)) + " Error: "+ str(ConError)
					socket.send("Locales ")
					socket.send(str(Locales))#message)

					socket.close
					#trabajando=0
					#b=[]
					return

				except :

					socket.close

					return
		if (b[0][0])=='conexiones':
			if socket != server_socket and socket == sock : #socket != server_socket and
				try :

					totalpeticiones=totalpeticiones+1

					socket.send("Conexiones")
					socket.send(str(Con))
					#sock.send(str(bb))
					socket.close
					#trabajando=0
					#b=[]
					return
					#socket.send(str(Locales))
				except :

					socket.close

					return
		if (b[0][0])=='cdco':
			if socket != server_socket and socket == sock : #socket != server_socket and
				try :

					totalpeticiones=totalpeticiones+1
					bb="Cantidad de EyL =" + str(len(Locales))+  " Cant de Conexiones =" + str(len(Con))+ " Cant de Sock "+ str(len(CONNECTION_LIST)) + " Peticiones: "+ str(totalpeticiones) + " Error: "+ str(ConError)
					socket.send(str(bb))#message)
					socket.close
					#trabajando=0
					#b=[]
					return
					#socket.send(str(Locales))
				except :

					socket.close
					#CONNECTION_LIST.remove(socket)
					return
		if (b[0][0])=='C':
			if socket != server_socket and socket == sock : #socket != server_socket and
				try :

					totalpeticiones=totalpeticiones+1
					eze=IdentificoConexion(message,ids,sock)
					borrosocket=[]
					socket.send(eze)#message)
					if eze=='NO':
						sock.close()
						CONNECTION_LIST.remove(sock)#borrosocket.append(sock)


				except :

					try:
						CONNECTION_LIST.remove(sock)
					except:
						continue
					return
		if (b[0][0])=='S':
			if socket != server_socket and socket == sock : #socket != server_socket and
				try :

					totalpeticiones=totalpeticiones+1
					socket.send(IdentificoConexion(message,ids,sock))#message)
					#trabajando=0
					#b=[]
					print ("salio bien S")
					return
					#socket.send(str(Locales))
				except :

					#b=[]
					print ("salto except de S")
					socket.close
					#CONNECTION_LIST.remove(socket)
					return
		if (b[0][0])=='crst':
			if socket != server_socket and socket == sock : #socket != server_socket and
				try :
					###print(len(ids))
					#z=IdentificoConexion(message,"1234")
					totalpeticiones=0
					Con[:] = []
					Locales[:] = []

					return
					#socket.send(str(Locales))
				except :

					#b=[]
					socket.close
					#CONNECTION_LIST.remove(socket)
					return
		else:

			if len(Con)== 0 and (b[0][0])=='DC' and socket == sock:
						socket.send('NO')
						socket.close
						#b=[]
						#trabajando=0
						#return
			if  socket != server_socket and socket != sock  :
				#print "Entro socket"
				try :

					
					for i in range(len(Con)):
						#for j in range(len(Con[i])-2): #Obtengo IDCONEXION
						if ((Con[i][0])==(b[0][1])): #comparo con id de conexion
							
							
							
							con1=str(Con[i][1])
							con2=str(Con[i][2])
							tipot=(str(Con[i][3]))

							try:
								if (b[0][0])=='DS':
									if str(con1) == str(socket):#str(socket.getpeername()):
										eze=""
										#print str(b[0][2])
										
										for i in range(len(b)):
											for j in range(len(b[i])):
												if (j>1):
													cadena=str(b[i][j])
													#esta=cadena.find("DS")
													#if esta > 0:
													#	socket.send(str(eze))
													#	eze=""
													#	print "estaaa DS " + str(b[i][j])
														#j=j+3
														#j=j+2
													eze=eze + ',' + str(b[i][j])
													#print str(b[i][j] + " valor de j " + str(j))
										if eze==",no":
											eze="no"
										if eze==",Ok":
											eze="Ok"

										print(str(b[0][1]))#(BorrarComunicacion((b[0][1])))
										socket.setblocking(0)
										socket.send(str(eze)) #Con[i][j] + ' ' + con1 + ' ' + con2)#con1 + ' 1 '+ idc
										print(BorrarComunicacion((b[0][1]),socket))

										ServerOcupado=0
										socket.close
										
										ocupadoCelu=0
										#trabajando=0
										#b=[]
										return
									#elif con2 == ids:
									#	socket.send(message) #Con[i][j] + ' ' + con1 + ' ' + con2)#con1 + ' 1 '+ idc
									#	return
									else:
										#break
										a=1
										
								if (b[0][0])=='DC':
									if str(con2) == str(socket):#str(socket.getpeername()):
										eze=str(str(b[0][1]) + str(b[0][2]))
										print eze
										##print(str(eze)+str(len(eze))+str(tipot))
										socket.setblocking(0)
										if ((tipot=='P')):# and (len(eze)==23)):
											socket.send(str(eze)) #Con[i][j] + ' ' + con1 + ' ' + con2)#con1 + ' 1 '+ idc
										elif (tipot=='M'):
											socket.send(str(eze)) #Con[i][j] + ' ' + con1 + ' ' + con2)#con1 + ' 1 '+ idc
										#trabajando=0
										#b=[]
										return

									else:
										#break
										a=1
							except:
								if (b[0][0])=='DS':
									##print("mando borrar a cliente por falta de conexion" + b[0][1])
									ConError="DS " + eze
									print("Salio x Error")
									socket.send(str(eze))#"Error al enviar desde Server")
									BorroConexion(str(socket))

									ocupadoCelu=0
									ServerOcupado=0
									return
									#socket.close
									#sock.close
								else:

									ConError="DC " + eze
									socket.send(str(eze))#socket.send("Error al enviar desde Cliente")
									BorroConexion(str(socket))

									return
		#print "llego abajo de broadcast"
		#return

				except :
					print "entro except broadcast"
					BorroConexion(str(socket))
					BorroInutiles()

					continue

if __name__ == "__main__":
     
    # List to keep track of socket descriptors
    CONNECTION_LIST = []
    RECV_BUFFER = 1024 # Advisable to keep it as an exponent of 2
    PORT = 4556
    Locales = []#[[1,1,23450],[1,2,23451],[2,1,43452],[2,2,53453]]
    Con=[]
    Conexiones=0
    b=[]
    #ocupado=0
    #trabajando=0
    
	
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    # this has no effect, why ?
    server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_socket.setblocking(0)
    #server_socket.settimeout(5)
    server_socket.bind(("", PORT))
    server_socket.listen(500)
    
    # Add server socket to the list of readable connections
    CONNECTION_LIST.append(server_socket)
 
    print "Magnetic Cash Server started on port " + str(PORT) + " By E.E.C"
	
    eze=1
    if FechaReinicio=='':
			FechaReinicio=datetime.today()+timedelta(hours=1)#(hours=24)
			print('Reinicia' + str(FechaReinicio))

    while 1:
		

        # MiFecha=datetime.today()
        # print(str(MiFecha))
        
        # if FechaReinicio < MiFecha and len(Con)==0:#reinicio variables cada 24 hs
			# FechaReinicio=datetime.today()+timedelta(hours=1)

					
			# print("reinicio")
			# try:
				
				# for sock1 in CONNECTION_LIST:
					# print('entro for')
					# if server_socket!=sock1:
						# sock1.close()
						
				# print(str(CONNECTION_LIST))
				# TODOS=CONNECTION_LIST
				# CONNECTION_LIST[:]=[]
				# CONNECTION_LIST.append(server_socket)
				# print('paso')
				# #sleep(1.5)
				
				# #read_sockets,write_sockets,error_sockets = select.select(TODOS,[],[])
				# #for sock1 in read_sockets:
				# #	print('entro for')
				# #	if server_socket!=sock1:
						# #CONNECTION_LIST.remove(CONNECTION_LIST[sock])
				# #		print('cerro')
				# #		#sleep(1.5)
				# #		sock1.close()

			# except:
				# print('fallo')
				# #time.sleep()
				
	
        # Get the list sockets which are ready to be read through select
        if len(CONNECTION_LIST)==1:
			sleep(0.050)#solo hace sleep cuando no hay mas conexiones
			print "paso sleep"
        read_sockets,write_sockets,error_sockets = select.select(CONNECTION_LIST,[],[])

        for sock in read_sockets:
            #New connection
            #print "dentro del for " + str(sock)
            if sock == server_socket:
                # Handle the case in which there is a new connection recieved through server_socket
				if len(CONNECTION_LIST)<501:
					sockfd, addr = server_socket.accept()
					
					CONNECTION_LIST.append(sockfd)
					##print "Client (%s, %s) connected" % addr
					##print str(sockfd)
					Conexiones=+1
				else:

					BorroInutiles()
					break
					#server_socket.listen(500)

             
            #Some incoming message from a client
            else:
                # Data recieved from client, process it
                try:

                    dire=str(sock)#str(sock.getpeername())
                    #sock.settimeout(3)
                    ok="1"
                    #sock.send(str(ok))
                    #sock.setblocking(0)
                    data = sock.recv(RECV_BUFFER)
                    #print "leyo " + str(data)
                    if data:

						c=[]
						#c=data#.split(',')
						c.append(data.split(','))
						#if c[0][0] == "DS" and c[0][2]=="1":
						#	c=[]
						if c[0][0] == "DS" and c[0][2]!="1":
							ccc = random.uniform(0.010, 0.050)
							sleep(ccc)
						if c[0][0] == "DC":
							ccc = random.uniform(0.010, 0.050)
							sleep(ccc)
						if c[0][0] == "DS" and c[0][2]=="1":
							
						#while ServerOcupado == 1:
							a=1

						else:
							broadcast_data(sock,c,str(sock),len(c))#str(sock.getpeername()))
							data=""
							try:
								#sleep(1)
								estac=0
								for i in range(len(Con)):
									if Con[i][1]==str(sock) and sock !=server_socket:
										estac=1
										#print "esta conexiones"
								for i in range(len(Locales)):
									if Locales[i][4]==sock and sock !=server_socket:
										estac=1
										#print "esta locales"
								if estac==0:
									#print('borro socket')
									CONNECTION_LIST.remove(sock)
									try:
										sock.close()
									except:
										print('no pudo cerrar socket')

							except :
								#trabajando=0
								#b=[]
								continue

                    else:
						print("mando borrar a cliente por falta de conexion")# + b[0][1])
						try:
							if len(dire) > 0:
								BorroConexion(str(dire))
							CONNECTION_LIST.remove(sock)
						except:
							a=1
							continue

                except:

					#BorroInutiles()
					continue

     
    server_socket.close()
