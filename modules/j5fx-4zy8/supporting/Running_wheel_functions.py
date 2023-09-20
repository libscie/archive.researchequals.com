# Switch_maze running wheel functions
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
Change e.g., running time and off time here.
"""
# hard-coded recording parameters
off_time = 6 # seconds run wheel clamped after time is up
run_time = 5000 # milliseconds of run wheel access after start detected
animaltag='001'
cycle=120
# document data folder
os.chdir("/home/pi/Documents/Data/")
# init pigpio
pi = pigpio.pi()
wheel_in_port = 23  # running wheel encoder input to pi
wheel_in_port2 = 24 
pi_ard_ow = 15 # signal to arduino to open/close wheel with servo
pi.set_mode(wheel_in_port, pigpio.INPUT)
pi.set_mode(pi_ard_ow, pigpio.OUTPUT)

# initialize state variables
MODE = 1
clkLastState=0

class SaveData:
    def append_event(
        self, rotation, food_time, event_type, animaltag, hardware_time
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
tick = pi.get_current_tick()
save.append_event(run_time, off_time, "begin_session", animaltag, tick)