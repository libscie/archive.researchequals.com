# Switch_maze
from switch_maze_functions import *
"""
Execution loop for switch maze standard operation.
Changes typically not recommended here.
This is the temporal layout of the maze.
"""
while True:
    # start
    if MODE == 1:
        print("\n MODE 1 open start \n")
        pi.write(pi_ard_1, 1)  # open door1
        MODE = 3
        print("\nMODE 3\n")
        print("Scanning RFID tag in scale")
        print(datetime.datetime.now())
    if MODE == 2:
        # stop mode
        w = read_scale()
        if w < 10 and pi.read(ard_pi_1):
            pi.write(pi_ard_1, 0)  # close door1
            time.sleep(60)
            print("stopped")
        elif w < 10:
            time.sleep(600)  # scan every 10min for safety
        elif w > 10:
            pi.write(pi_ard_1, 1)  # open door1
            time.sleep(10)
    # animal on scale
    if MODE == 3:
        while True:
            animaltag = RFID_readtag("RFID1")
            if datetime.datetime.now() > stop_time:
                MODE = 2
                print(datetime.datetime.now())
                print("STOPPING MAZE because stop_time was reached")
                break
            if animaltag:
                w = read_scale()
                if not animaltag in animal_list:
                    MODE = 3
                    print("animal not in list")
                    print(animaltag)
                    break
                elif (
                    w > 10
                    and w < heavy
                    and pi.read(ard_pi_1)
                    and int(round(time.time())) - animal_timer[animal_list.index(animaltag)]
                    > nest_timeout
                ):
                    pi.write(pi_ard_1, 0)  # close door1
                    MODE = 4
                    choice_flag = False
                    entry_flag = False
                    water_flag = True
                    break
            else:
                MODE = 3
                print("*", end=",")
                break

    # correct animal on scale
    if MODE == 4:
        print("\nMODE 4\n")
        # Weighing for entry
        flag_heavy = acquire_weight(animaltag)
        if flag_heavy:
            # Append data
            tick = pi.get_current_tick()
            save.append_event(
                "+", "", "ENTRY DENIED MULTIPLE ANIMALS", animaltag, FED_position, tick
            )
            MODE = 1
        if not flag_heavy:
            pi.write(pi_ard_2, 1)  # open door 2
            pi.write(pi_ard_3, 1)  # open door 3
            MODE = 5
            print("\nMODE 5\n")
            print("\nblock start\n")
            print(animaltag)
            print(datetime.datetime.now())
            mode5timer = int(round(time.time()))
            # Append data
            tick = pi.get_current_tick()
            save.append_event("+", "", "START", animaltag, FED_position, tick)
    # time out mode5 if animal did not enter maze
    if (
        MODE == 5
        and pi.read(ard_pi_2)
        and not entry_flag
        and int(round(time.time())) > mode5timer + 300
    ):
        pi.write(pi_ard_2, 0)  # close door 2
        MODE = 1
    # animal at decision point
    if MODE == 5 and pi.read(ard_pi_5) and not choice_flag:
        # BB5 triggered, write data
        print("\ntrial start\n")
        pi.write(pi_ard_3, 1)  # open door 3
        #  send pulse to FED
        pi.write(give_pellet, 1)  # prepare a pellet
        if water_flag:
            pi.write(give_water, 1)  # prepare a water drop
            time.sleep(0.001)
            pi.write(give_water, 0)
            water_flag = False
        # append BB5 for the first time the animal enters the maze
        if event_list["Type"] == ["START"]:
            tick = pi.get_current_tick()
            save.append_event("*", "", "BB5", animaltag, FED_position, tick)
        # append food data
        if food_flag:
            #             print("appending food pod data")
            cycles_str = round(counter / cycle, 4)
            tick = pi.get_current_tick()
            save.append_event(
                cycles_str, "", "Food_log_BB5", animaltag, FED_position, tick
            )
        # append run data
        if run_flag:
            #             print("appending running wheel data, licks:")
            print(licks)
            cycles_str = round(counter / cycle, 4)
            tick = pi.get_current_tick()
            save.append_event(
                cycles_str, "", "Run_log_BB5", animaltag, FED_position, tick
            )
            tick = pi.get_current_tick()
            save.append_event(
                licks, drink_delay, "exit_drink", animaltag, FED_position, tick
            )
        # reset flags
        choice_flag = True
        entry_flag = True
        run_flag = False
        food_flag = False
        lick_flag = True
        licks = 0
        counter = 0
        food_clk_end = 0
        pellet_complete_flag = True

    # animal going back home
    if MODE == 5 and pi.read(ard_pi_2) and choice_flag:
        print("\nblock end\n")
        print(datetime.datetime.now())
        pi.write(pi_ard_2, 0)  # close door 2
        pi.write(pi_ard_1, 1)  # open door1
        tick = pi.get_current_tick()
        save.append_event("-", "", "END", animaltag, FED_position, tick)
        pi.write(give_pellet, 0)
        run_flag = False
        food_flag = False
        another_entered = False
        choice_flag = False
        entry_flag = False
        animal_timer[animal_list.index(animaltag)] = int(round(time.time()))
        time.sleep(exit_wait)  # safety timer so outgoing is not trapped on exit
        pi.write(pi_ard_calibrate_lick, 1)
        time.sleep(0.5)
        pi.write(pi_ard_calibrate_lick, 0)
        time.sleep(0.1)
        MODE = 1
    # enter food pod
    if MODE == 5 and pi.read(ard_pi_4) and choice_flag:
        print("\nenter food\n")
        pi.write(give_pellet, 0)  # signal back low so you can trigger next one
        pi.write(pi_ard_3, 0)  # close door 3
        tick = pi.get_current_tick()
        save.append_event("*", "", "BB4", animaltag, FED_position, tick)
        food_clk_start = time.process_time()
        choice_flag = False
        food_flag = True
        limit = cycle
    # enter drink/running wheel pod
    if MODE == 5 and pi.read(ard_pi_3) and choice_flag:
        print("\nenter water\n")
        pi.write(pi_ard_4ow, 1)  # open running wheel
        pi.write(pi_ard_3, 0)  # close door 3
        tick = pi.get_current_tick()
        save.append_event("*", "", "BB3", animaltag, FED_position, tick)
        run_clk_start = time.process_time()
        choice_flag = False
        run_flag = True
        limit = cycle
        lick_timer = int(round(time.time() * 1000))
        licks = 0
        drink_delay = 0
    # record delay to food retrieval
    if food_flag:
        if pi.read(Food_retrieval) and pellet_complete_flag:
            food_clk_end = round(time.process_time() - food_clk_start, 4)
            pellet_complete_flag = False
            print("EAT")
            tick = pi.get_current_tick()
            save.append_event(
                "", food_clk_end, "Food_retrieval", animaltag, FED_position, tick
            )
    # record running wheel and licking
    if run_flag:
        clkState = pi.read(wheel_in_port)
        if clkState != clkLastState:
            counter += 1
            clkLastState = clkState
        if counter >= limit:
            print(counter)
            limit = counter + cycle
        if time.process_time() - run_clk_start > run_time:
            pi.write(pi_ard_4ow, 0)  # close running wheel
        if pi.read(ard_pi_lick):
            licks = licks + 1
            water_flag = True
            if lick_flag:
                drink_delay = int(round(time.time() * 1000)) - lick_timer
                tick = pi.get_current_tick()
                save.append_event(
                    "", drink_delay, "drink", animaltag, FED_position, tick
                )
                pi.write(give_water, 1)  # give a water drop
                time.sleep(water_time)
                pi.write(give_water, 0)
                lick_flag = False
                print("DRINK")