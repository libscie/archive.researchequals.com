# Timed running wheel module from Switch_maze
from Running_wheel_functions import *
"""
Execution loop for run logging and locking wheel.
Wheel is on for 5000ms after run is initiated and then off for 6s (change from functions).
Changes typically not recommended here.
This is an example.
"""
while True:

    # reset
    if MODE == 1:
        pi.write(pi_ard_ow, 1)  # open running wheel
        print("\nwheel open\n")
        tick = pi.get_current_tick()
        save.append_event("*", "", "run_available", animaltag, tick)
        start_timer=int(round(time.time() * 1000))
        start_flag=True
        limit=cycle
        latency_to_run=[]  
        counter=0
        MODE=2

    # wait for wheel use and record it      
    if MODE == 2:
        clkState=pi.read(wheel_in_port)
        # record rotary encoder
        if clkState != clkLastState:
            counter += 1  
            clkLastState = clkState
        # record latency to run start at first quarter revolution
        if start_flag and counter>limit/4: 
            tick = pi.get_current_tick()
            save.append_event("", "", "run_start", animaltag,tick) 
            run_clk_start = int(round(time.time() * 1000))
            latency_to_run = run_clk_start-start_timer
            start_flag=False
        # print recorded encoder ticks after each revolution
        if counter >= limit:
            print(counter)
            limit=counter+cycle
        # time is up
        if not start_flag and int(round(time.time() * 1000)) - run_clk_start > run_time:
            pi.write(pi_ard_ow, 0)  # close running wheel
            cycles_str = round(counter / cycle, 4)
            tick = pi.get_current_tick()
            save.append_event(cycles_str, "", "Run_log", animaltag, tick)
            print("\nwheel closed\n")
            time.sleep(off_time)
            MODE=1
