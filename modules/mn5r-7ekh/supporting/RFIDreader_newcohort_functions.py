# ReadRFID tags to start a new cohort on Switch_maze

import serial
import time
import RPi.GPIO as GPIO
import statistics as stats
from datetime import datetime
import io
import sys
import yagmail
# query parameter input from user
print('********** getting user input **********')
cohort_size=int(input("How many animals in this cohort?"))
print('********************** input complete, starting identification **********************')
    
# recording parameters
animal_list = (
    []
)  # empty list of tags so code can find out who they are
chuck_lines = 2  # chuck first weight reads for stability
heavy = 40  # heavy (more than one mouse) limit in g, 40g standard
exit_wait = 5  # safety timer in seconds so outgoing is not trapped on exit, decrease when they learn to use

# set GPIO numbering mode
GPIO.setmode(GPIO.BOARD)
GPIO.setwarnings(False)  # Ignore warning for now

# initialize serial port for usb RFID
serRFID = serial.Serial()
serRFID.port = "/dev/ttyUSB1"
serRFID.baudrate = 9600
serRFID.timeout = 100  # specify timeout when using readline()
serRFID.open()
if serRFID.is_open == True:
    print("\nRFID antenna ok. Configuration:\n")
    print(serRFID, "\n")  # print serial parameters
serRFID.close()

# initialize serial port for usb OpenScale
ser = serial.Serial()
ser.port = "/dev/ttyUSB0"
ser.baudrate = 9600
ser.timeout = 100000
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
Food_retrieval = 11  # BNC output FED3
GPIO.setup(Food_retrieval, GPIO.IN)
give_pellet = 10  # trigger FED3
GPIO.setup(give_pellet, GPIO.OUT)

# pin to water valve
give_water = 29  # to valve
GPIO.setup(give_water, GPIO.OUT)

# set pin inputs from running wheel rotary encoder
clk = 12
GPIO.setup(clk, GPIO.IN)
clkLastState = GPIO.input(clk)

# set pin input room door sensor
room_door = 22  # IR proximity detector on room door
GPIO.setup(room_door, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)

# set pin outputs to arduino
pi_ard_1 = 15  # open door1
pi_ard_2 = 13  # open door2
pi_ard_3 = 16  # open door3
pi_ard_4ow = 5  # open wheel

GPIO.setup(pi_ard_1, GPIO.OUT)
GPIO.setup(pi_ard_2, GPIO.OUT)
GPIO.setup(pi_ard_3, GPIO.OUT)
GPIO.setup(pi_ard_4ow, GPIO.OUT)
GPIO.output(pi_ard_1, False)
GPIO.output(pi_ard_2, False)
GPIO.output(pi_ard_3, False)
GPIO.output(pi_ard_4ow, False)

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

def RFID_readtag(RFIDnum):
    """
    This function reads the RFID tag, removes the junk incoming and returns the
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
            print("Something went wrong")
            animaltag = False
            return animaltag

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
        Function used to save weight parameters to a .csv file
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
    ):  # add hardware time call outs to below!

        """
        Function used to save event parameters to a .csv file
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

def acquire_weight(animaltag):
    global chuck_lines
    """
    This function is used to acquire 50 datapoints of the animals weight and returns
    a few different parameters - mean, median, mode, max_mode(the latter does not
    work in python 3.7). Mode tends to be the most accurate metric.
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
    for x in range(3):  # 3 lines*120ms per line=0.4s of data
        line = ser.readline()
        if x % 1 == 0:
            print(line)
        line_as_list = line.split(b",")
        relProb = line_as_list[0]
        relProb_as_list = relProb.split(b"\n")
        n = float(relProb_as_list[0])
        n = n * 1000
        m.append(n)
    w = stats.mean(m)
    return w

GPIO.setup(ard_pi_1, GPIO.IN)
GPIO.setup(ard_pi_2, GPIO.IN)
GPIO.setup(ard_pi_5, GPIO.IN)