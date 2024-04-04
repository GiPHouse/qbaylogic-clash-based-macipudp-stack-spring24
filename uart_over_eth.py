import struct
import socket
import fcntl
import util
import env

eth_type = b'\xff\xff'
payload = "Hello world! :D (please work)".encode('utf-8')
padding_needed = 46 - len(payload)
if padding_needed > 0:
    payload += b'\x00' * padding_needed

assert len(payload) <= 1500

with util.open_socket() as s, util.open_serial() as sr:
    sr.flushInput()
    mac = util.get_mac_addr(s, env.IFNAME)
    # Send frame to self
    print("Transmitting frame:")
    frame = mac + mac + eth_type + payload
    s.send(frame)

    frame = b'UUUUUUU\xd5\x00\xe0Ok\xfb\xa3\x00\xe0Ok\xfb\xa3\xff\xffHello world! :D (please work)\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00nj\xd5'
    print(len(frame))

    #frame = b'\x55\x55\x55\x55\x55\x55\x55\xD5' + frame + 4 * b'\xAA'
    print(frame)
    sr.write(util.make_uart_packet(frame))
    print("Received from uart:")
    print(sr.read(len(frame)))

    reponse = s.recv(1500)
    print("Received response:")
    print(reponse)
