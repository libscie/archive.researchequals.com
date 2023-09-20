# Switch_maze SEM
import serial
import time
import pigpio
import os
import pandas as pd
import statistics as stats
import datetime
import numpy as np
"""
Functions and parameters for switch maze standard operation.
Change e.g., cohort tags and water amount here.
"""
#initialize ard during user input
pi = pigpio.pi() # init pigpio
pi_ard_door1 = 15
pi_ard_door2 = 18
pi.set_mode(pi_ard_door1, pigpio.OUTPUT)
pi.set_mode(pi_ard_door2, pigpio.OUTPUT)
ard_pi_BB1 = 23# reports BB1low
ard_pi_BB2 = 24# reports BB2low
pi.set_mode(ard_pi_BB1, pigpio.INPUT)
pi.set_mode(ard_pi_BB2, pigpio.INPUT)
pi.write(pi_ard_door1, 0)  # close door1
pi.write(pi_ard_door2, 0)  # close door2
PiArd_reset = 14
pi.set_mode(PiArd_reset, pigpio.OUTPUT)
pi.write(PiArd_reset, 0) # ard resets when reset pin is LOW
time.sleep(0.1)
pi.write(PiArd_reset, 1) # back HIGH so it stops resetting

# query parameter input from user
print('********** getting user input **********')
test_answer=int(input("test (0) or exp (1) ?"))
if test_answer==0:
    test_tags = ["202100030","137575399426", "2006010085"]#insert your test tags here manually
    animal_list = test_tags
    print('TEST session with your usual test tags')
elif test_answer==1:
    animal_list = [
    "34443624695",
    "34443624808",
    "137575399507",
    "34443624982",
    ]  # insert your mouse tags here manually
    print('LIVE session with subjects:')
    print(animal_list)
print('********************** input complete, starting session **********************')

# hard-coded recording parameters
minimum_entry_time = 6 # seconds to wait until starting to detect exit
nest_timeout = 10  # timeout in nest after exiting maze in seconds
heavy = 35 #g limit too heavy = more than one animal on scale 
light = 10 #g limit too light = animal incompletely on scale
chuck_lines = 5  # chuck first weight reads for stability

# document data folder
os.chdir("/home/pi/Documents/Data/")

# initialize serial port for usb RFID
serRFID = serial.Serial()
serRFID.port = "/dev/ttyUSB1" # user may need to change this
serRFID.baudrate = 9600
serRFID.timeout = 100  # timeout in seconds when using readline()
serRFID.open()
if serRFID.is_open == True:
    print("\nRFID antenna ok. Configuration:\n")
    print(serRFID, "\n")  # print serial parameters
serRFID.close()

# initialize serial port for usb OpenScale
ser = serial.Serial()
ser.port = "/dev/ttyUSB0" # user may need to change this
ser.baudrate = 9600 # can be changed for speed but must match arduino
ser.timeout = 100
# specify timeout when using readline()
ser.close()
ser.open()
ser.flush()
for x in range(10):
    line = ser.readline()
    print(line)
if ser.is_open == True:
    print("\nScale ok. Configuration:\n")
    print(ser, "\n")  # print serial parameters

# animal timers
animal_timer = animal_list.copy()
for x in range(np.size(animal_list)):
    animal_timer[x] = int(round(time.time())) - nest_timeout


# initialize state variables
MODE = 1

# functions
def RFID_readtag(RFIDnum):
    """
    This function reads the RFID tag, removes junk and returns the
    converted ID from hexadecimal to decimal.
    """
    RFIDtimer = int(round(time.time()))
    if RFIDnum == "RFID1":
        try:
            serRFID.close()
            serRFID.open()
            serRFID.flush()
            junk = serRFID.read(1)
            tag = serRFID.read(10)
            checksum = serRFID.read(2)
            junk2 = serRFID.read(3)
            animaltag = str(int(tag, 16))  # transform from hexadecimal to a number
            print(animaltag)
            serRFID.close()
            return animaltag
        except:
            serRFID.close()
            print("RFID read failed")
            animaltag = False
            return animaltag

def acquire_weight(animaltag):
    global chuck_lines
    """
    This function acquires 50 datapoints of the animal's weight and returns 
    mean, median, mode, max_mode(the latter does not
    work in python 3.7).
    """
    print("Acquiring weight")
    flag_heavy = False
    ys = []  # store weights here
    ser.close()
    ser.open()
    ser.flush()
    for x in range(chuck_lines):  # chuck lines
        line = ser.readline()
    for x in range(50):  # 50 lines*120ms per line=6s of data
        line = ser.readline()
        if x % 1 == 0:
            print(line)
        line_as_list = line.split(b",")
        relProb = line_as_list[0]
        relProb_as_list = relProb.split(b"\n")
        relProb_float = float(relProb_as_list[0])
        relProb_float = relProb_float * 1000
        # More than one animal:
        if relProb_float > heavy:
            print("MULTIPLE ANIMALS ON SCALE")
            flag_heavy = True
            return flag_heavy
        else:
            ys.append(relProb_float)

    if not flag_heavy:
        for i in range(len(ys)):
            ys[i] = round(ys[i], 3)
        weight_data_mean = stats.mean(ys)
        weight_data_median = stats.median(ys)
        # mode
        try:
            weight_data_mode = stats.mode(ys)
        except:
            weight_data_mode = "NO MODE"
        # mode max
        try:
            weight_data_max_mode = stats.multimode(ys)
            weight_data_max_mode = weight_data_max_mode[-1]  # largest of modes
        except:
            weight_data_max_mode = "NO MAX_MODE"
        # appending data to database
        save = SaveData()
        save.append_weight(
            weight_data_mean,
            weight_data_median,
            weight_data_mode,
            weight_data_max_mode,
            animaltag,
        )
        return flag_heavy

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

class SaveData:
    def append_weight(
        self,
        weight_data_mean,
        weight_data_median,
        weight_data_mode,
        weight_data_max_mode,
        animaltag,):
        """
        Function used to save weight data to a .csv file
        """
        weight_list = {
            "Weight_Mean": [],
            "Weight_Median": [],
            "Weight_Mode": [],
            "Weight_Max_Mode": [],
            "Date_Time": [],
        }
        weight_list.update({"Weight_Mean": [weight_data_mean]})
        weight_list.update({"Weight_Median": [weight_data_median]})
        weight_list.update({"Weight_Mode": [weight_data_mode]})
        weight_list.update({"Weight_Max_Mode": [weight_data_max_mode]})
        weight_list.update({"Date_Time": [datetime.datetime.now()]})
        df_w = pd.DataFrame(weight_list)
        print(df_w)
        if not os.path.isfile(animaltag + "_weight.csv"):
            df_w.to_csv(animaltag + "_weight.csv", encoding="utf-8-sig", index=False)
            print("weight file created")
        else:
            df_w.to_csv(
                animaltag + "_weight.csv",
                mode="a+",
                header=False,
                encoding="utf-8-sig",
                index=False,
            )
            print("weight file appended")

    def append_event(self, rotation, food_time, event_type, animaltag, hardware_time): 
        """
        Function used to save event data to a .csv file
        """
        global event_list
        event_list = {
            "Date_Time": [],
            "Rotation": [],
            "Pellet_Retrieval": [],
            "Type": [],
            "hardware_time": [],
        }
        event_list.update({"Rotation": [rotation]})
        event_list.update({"Pellet_Retrieval": [food_time]})
        event_list.update({"Type": [event_type]})
        event_list.update({"Date_Time": [datetime.datetime.now()]})
        event_list.update({"hardware_time": [hardware_time]})
        df_e = pd.DataFrame(event_list)
        if not os.path.isfile(animaltag + "_events.csv"):
            df_e.to_csv(animaltag + "_events.csv", encoding="utf-8-sig", index=False)
        else:
            df_e.to_csv(
                animaltag + "_events.csv",
                mode="a+",
                header=False,
                encoding="utf-8-sig",
                index=False,
            )

#final init calls
save = SaveData()

# save session parameters in a begin session event line
for x in range(np.size(animal_list)):
    animaltag = animal_list[x]
    tick = pi.get_current_tick()
    save.append_event(
        minimum_entry_time, nest_timeout, "begin session", animaltag, tick
    )