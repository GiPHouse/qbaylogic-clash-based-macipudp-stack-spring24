import serial
from struct import pack, unpack
from time import sleep
import sys

READ = 0b0
WRITE = 0b1

# Format: (OP, PHY, REG, D16)
w_req = []
for i in range(5**2):
     w_req.append(
          (READ, i, 0b1, 0xf)
     )
r_req = []
for i in range(5**2):
     r_req.append(
          (READ, i, 0b1, 0xf)
     )

# Check format is correct

def print_instruction(instruction):
    (instr, addr1, addr2, data) = instruction
    print(pack('>?', instr))
    print(pack('>B', addr1))
    print(pack('>B', addr2))
    print(pack('>H', data))
    print("_____________________________")

try:
    with serial.Serial('/dev/ttyUSB0', 9600, timeout=1) as ser:
        for (instr, addr1, addr2, data) in w_req:
            ser.write(pack(">B", instr))
            ser.write(pack(">B", addr1))
            ser.write(pack(">B", addr2))
            ser.write(pack(">H", data))

        for (instr, addr1, addr2, data) in r_req:
            ser.write(pack(">B", instr))
            ser.write(pack(">B", addr1))
            ser.write(pack(">B", addr2))
            ser.write(pack(">H", data))
            (result, ) = unpack(">B", ser.read(1))
            (result2, ) = unpack(">B", ser.read(1))
            print("first byte: {}, second byte: {}".format(bin(result), bin(result2)))
            print("result: {}".format((result << 8) ^ result2))

except Exception as e:
    print(e)
    print("Could not open device at port (check that its plugged in)")
