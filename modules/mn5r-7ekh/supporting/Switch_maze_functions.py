# Switch_maze
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
# query parameter input from user
print('********** getting user input **********')
test_answer=int(input("test (0) or exp (1) ?"))
if test_answer==0:
    test_tags = ["202100030","137575399426", "2006010085"]#insert your test tags here manually
    animal_list = test_tags
    start_time = datetime.datetime.now()
    print(start_time)
    stop_time = datetime.datetime.now()+datetime.timedelta(days=1)
    print(stop_time)
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
    start_answer = int(input("How many minutes to wait until start?"))
    start_time = datetime.datetime.now() + datetime.timedelta(minutes=start_answer)
    print(start_time)
    stop_answer = float(input("Stop at how many hours +/- tomorrow 9:30am?"))
    stop_time = datetime.datetime.now()+datetime.timedelta(days=1)
    stop_time=stop_time.replace(hour=9,minute=30,second=0,microsecond=0)+datetime.timedelta(hours=stop_answer)
    print(stop_time)

# which side food and drink spout
def food_side(FED):
    if FED == "Left":
        water = "Right"
    elif FED == "Right":
        water = "Left"
    return [water, FED]
FED_answer = input("Which pod is the FED3 in: l or r? (left or right)").capitalize()
assert FED_answer == "R" or FED_answer == "L", "INVALID FED POSITION! Please answer either l or r."
if FED_answer == "L":
    water_position, FED_position = food_side("Left") 
    ard_pi_3 = 16  # arduino pins BB3/4
    ard_pi_4 = 26
    print('food on LEFT')
elif FED_answer == "R":
    water_position, FED_position = food_side("Right") 
    ard_pi_3 = 26  # arduino pins BB3/4
    ard_pi_4 = 16
    print('food on Right')
print('********************** input complete, starting session **********************')
    
# change these recording parameters as needed
water_time = 0.1  # seconds water dispensed when animal licks spout 0.1=20ul standard
run_time = 120  # running wheel availability in seconds 120s standard
chuck_lines = 2  # chuck first weight reads for stability
nest_timeout = 100  # timeout in nest after exiting maze in seconds 100s standard
heavy = 35  # heavy (more than one mouse) limit in g, 40g standard
exit_wait = 1  # safety timer in seconds so outgoing is not trapped on exit, decrease when they learn to use

# data folder
os.chdir("/home/pi/Documents/Data/")
# init pigpio
pi = pigpio.pi()
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
ard_pi_lick = 12  # reports Capacitive lick sensor

# set pins to and from FED3
Food_retrieval = 15  # BNC output from FED3
pi.set_mode(Food_retrieval, pigpio.INPUT)
give_pellet = 11  # trigger FED3 command from Pi
pi.set_mode(give_pellet, pigpio.OUTPUT)

# pin to water valve
give_water = 5  # to valve
pi.set_mode(give_water, pigpio.OUTPUT)

# parameters for running wheel rotary encoder
wheel_in_port = 18
pi.set_mode(wheel_in_port, pigpio.INPUT)
clkLastState = pi.read(wheel_in_port)
cycle = 90  # cycle on running wheel gives this many counts. 90 copal
run_clk_start = 0
wheel_duration = run_time * 1000

# set pin input room door sensor
room_door = 25  # IR proximity detector on room door
pi.set_mode(room_door, pigpio.INPUT)

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
pi.write(PiArd_reset, 0)  # ard resets when reset pin is LOW
time.sleep(0.1)
pi.write(PiArd_reset, 1)  # back HIGH so it stops resetting

# initialize state variables
MODE = 1
choice_flag = False
counter = 0
cycle = 90  # cycle on running wheel gives this many counts 90 for copal; 1200 for 600B
run_flag = False
food_flag = False
water_flag = True
flag_heavy = False
flag_animals_left = False
task_complete_flag = False
entry_flag = False
another_entered = False
sub_flag = False
substition = 0
food_clk_end = 0
food_clk_start = 0
run_clk_start = 0
# animal timers
animal_timer = animal_list.copy()
for x in range(np.size(animal_list)):
    animal_timer[x] = int(round(time.time())) - nest_timeout

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
        animaltag,
    ):
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

    def append_event(
        self, rotation, food_time, event_type, animaltag, FED_position, hardware_time
    ): 
        """
        Function used to save event data to a .csv file
        """
        global event_list
        event_list = {
            "Date_Time": [],
            "Rotation": [],
            "Pellet_Retrieval": [],
            "Type": [],
            "FED_Position": [],
            "hardware_time": [],
        }
        event_list.update({"Rotation": [rotation]})
        event_list.update({"Pellet_Retrieval": [food_time]})
        event_list.update({"Type": [event_type]})
        event_list.update({"Date_Time": [datetime.datetime.now()]})
        event_list.update({"FED_Position": [FED_position]})
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

def door_callback(gpio, level, tick):
    print(gpio, level, tick)
    tick = pi.get_current_tick()
    if not level:
        print("door closed")
        for x in range(np.size(animal_list)):
            fn = animal_list[x]
            save.append_event(
                run_time, water_time, "room door closed", fn, FED_position, tick
            )
    else:
        print("door OPEN")
        for x in range(np.size(animal_list)):
            fn = animal_list[x]
            save.append_event(
                run_time, water_time, "room door opened", fn, FED_position, tick
            )

#final init calls
cb1 = pi.callback(room_door, pigpio.EITHER_EDGE, door_callback)
save = SaveData()
pi.set_mode(ard_pi_1, pigpio.INPUT)
pi.set_mode(ard_pi_2, pigpio.INPUT)
pi.set_mode(ard_pi_3, pigpio.INPUT)
pi.set_mode(ard_pi_4, pigpio.INPUT)
pi.set_mode(ard_pi_5, pigpio.INPUT)
pi.set_mode(ard_pi_lick, pigpio.INPUT)
pi.set_mode(ard_pi_lick, pigpio.PUD_DOWN)

# save session parameters in a begin session event line
for x in range(np.size(animal_list)):
    animaltag = animal_list[x]
    tick = pi.get_current_tick()
    save.append_event(
        run_time, water_time, "begin session", animaltag, FED_position, tick
    )