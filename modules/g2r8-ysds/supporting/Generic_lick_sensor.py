# Lick activated water dispenser module from Switch_maze
from Lick_sensor_functions import *
"""
Execution loop for lick logging and water dispensing.
Delivers a drop of water upon licking.
Next drop of water will be available after 1s.
Changes typically not recommended here.
This is an example.
"""
while True:
    # reset
    if MODE == 1:
        print("water available")
        tick = pi.get_current_tick()
        save.append_event("*", "", "drop_available", animaltag, tick)
        lick_timer = int(round(time.time() * 1000))
        licks = 0
        drink_delay = 0
        time_to_next = 0
        lick_flag = True
        MODE=2
    # record licking
    if MODE == 2:
        if pi.read(ard_pi_lick):
            licks = licks + 1
            if lick_flag:
                drink_time = int(round(time.time() * 1000))
                drink_delay = drink_time - lick_timer
                tick = pi.get_current_tick()
                save.append_event("", drink_delay, "drink", animaltag, tick)
                pi.write(give_water, 1)  # give a water drop
                time.sleep(water_time)
                pi.write(give_water, 0)
                lick_flag = False
                print("DRINK")
    if MODE == 2 and not lick_flag and int(round(time.time() * 1000)) - drink_time>timeout:
        tick = pi.get_current_tick()
        save.append_event("*", licks, "lick_tally", animaltag, tick)
        MODE=1