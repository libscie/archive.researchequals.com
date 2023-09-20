# Switch_maze lick sensor functions
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
Change e.g., water amount here.
"""
# hard-coded recording parameters
water_time = 0.1  # seconds water dispensed when animal licks spout 0.1=20ul standard
timeout = 1000 # milliseconds to wait between dispensed drops
animaltag='001'
# document data folder
os.chdir("/home/pi/Documents/Data/")
# init pigpio
pi = pigpio.pi()
ard_pi_lick = 11  # reports Capacitive lick sensor

# pin to water valve
give_water = 7  # to relay switch controlling valve
pi.set_mode(give_water, pigpio.OUTPUT)

# set pin outputs to arduino
PiArd_reset = 2  # reset arduino
pi_ard_calibrate_lick = 3  # recalibrate capacitive lick sensor
pi.set_mode(PiArd_reset, pigpio.OUTPUT)
pi.set_mode(pi_ard_calibrate_lick, pigpio.OUTPUT)
pi.write(pi_ard_calibrate_lick, 0)
pi.write(PiArd_reset, 0)  # ard resets when reset pin is LOW
time.sleep(0.01)
pi.write(PiArd_reset, 1)  # back HIGH so it stops resetting

# initialize state variables
MODE = 1

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
pi.set_mode(ard_pi_lick, pigpio.INPUT)
pi.set_mode(ard_pi_lick, pigpio.PUD_DOWN)

# save session parameters in a begin session event line
tick = pi.get_current_tick()
save.append_event('', water_time, "begin session", animaltag, tick)