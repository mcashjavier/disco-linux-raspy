import socket
import sys

# Create a TCP/IP socket
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

# Connect the socket to the port where the server is listening
server_address = ('181.164.238.87', 123)
print('connecting to %s port %s' % server_address)
sock.connect(server_address)

try:
    
    # Send data
    message = 'C;1921000005618'
    print('sending "%s"' % message)
    sock.send(message.encode())

    # Look for the response
    amount_received = 0
    amount_expected = 1#len(message)
    
    while amount_received < amount_expected:
        data = sock.recv(100)
        amount_received += len(data)
        print('received "%s"' % data.decode())

finally:
    print('closing socket')
    sock.close()
