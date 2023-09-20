# Switch_maze rescue functions
import serial
import time
import pigpio
import os
import pandas as pd
import statistics as stats
import datetime
import numpy as np
import yagmail
"""
Functions and parameters for switch maze mouse rescue.
"""
print('********** getting user input **********')
animals=int(input("How many animals need to be extracted from maze?"))
print('********************** input complete, starting rescue **********************')

# hard-coded recording parameters
chuck_lines = 2  # chuck first weight reads for stability

# init pigpio
pi = pigpio.pi()

# initialize serial port for usb OpenScale
ser = serial.Serial()
ser.port = "/dev/ttyUSB0" # user may need to change this
ser.baudrate = 9600
ser.timeout = 100
# specify timeout when using readline()
ser.open()
ser.flush()
for x in range(10):
    line = ser.readline()
    print(line)
if ser.is_open == True:
    print("\nScale ok. Configuration:\n")
    print(ser, "\n")  # print serial parameters

# set pin inputs from arduino
ard_pi_1 = 13  # reports BB1high
ard_pi_2 = 19  # reports BB2low
ard_pi_5 = 21  # reports BB5low

# set pins to and from FED3
Food_retrieval = 15  # BNC output from FED3
pi.set_mode(Food_retrieval, pigpio.INPUT)
give_pellet = 11  # trigger FED3 command from Pi
pi.set_mode(give_pellet, pigpio.OUTPUT)

# set pin outputs to arduino
pi_ard_1 = 22  # open door1
pi_ard_2 = 27  # open door2
pi_ard_3 = 23  # open door3
pi_ard_4ow = 3  # open wheel
PiArd_reset = 24  # reset arduino
pi_ard_calibrate_lick = 8  # recalibrate capacitive lick sensor
pi.set_mode(pi_ard_1, pigpio.OUTPUT)
pi.set_mode(pi_ard_2, pigpio.OUTPUT)
pi.set_mode(pi_ard_3, pigpio.OUTPUT)
pi.set_mode(pi_ard_4ow, pigpio.OUTPUT)
pi.set_mode(PiArd_reset, pigpio.OUTPUT)
pi.set_mode(pi_ard_calibrate_lick, pigpio.OUTPUT)
pi.write(pi_ard_1, 0)
pi.write(pi_ard_2, 0)
pi.write(pi_ard_3, 0)
pi.write(pi_ard_4ow, 1)  # open running wheel
pi.write(pi_ard_calibrate_lick, 0)

def read_scale():
    """
    This function takes a quick read to sense the scale.
    """
    global chuck_lines
    m = []  # store weights here
    ser.close()
    ser.open()
    ser.flush()
    for x in range(chuck_lines):  # chuck lines
        line = ser.readline()
    for x in range(2):  # 2 lines*120ms per line=0.24s of data
        line = ser.readline()
        line_as_list = line.split(b",")
        relProb = line_as_list[0]
        relProb_as_list = relProb.split(b"\n")
        n = float(relProb_as_list[0])
        n = n * 1000
        m.append(n)
    w = stats.mean(m)
    return w

#final init calls
pi.set_mode(ard_pi_1, pigpio.INPUT)
pi.set_mode(ard_pi_2, pigpio.INPUT)
pi.set_mode(ard_pi_5, pigpio.INPUT)

# initialize state
pi.write(give_pellet, 0) # stop giving pellets
pi.write(pi_ard_1, 0) # close door1
pi.write(pi_ard_2, 1) # open door2
return_flag=0
MODE = 1