# Single entry module from Switch_maze
from SEM_functions import *
"""
Execution loop for single entry module demo.
Opens, detects one animal in module and reads its RFID, if animal hasn't exited in nest_timeout, closes safely, 
weighs animal, passes to other side, waits minimum_entry_time, then allows return.
Changes typically not recommended here.
This is an example.
"""
while True:

    # reset
    if MODE == 1:
        pi.write(pi_ard_door1, 1)  # open SEM
        print("\nSEM open\n")
        for x in range(np.size(animal_list)):
            animaltag = animal_list[x]
            tick = pi.get_current_tick()
            save.append_event("*", "", "entry_available", animaltag, tick)
        MODE=2

    # wait for entry    
    if MODE == 2:
        print("Scanning RFID tag in scale")
        print(datetime.datetime.now())

        while True:
            animaltag = RFID_readtag("RFID1")
            if animaltag:
                w = read_scale()
                if w<light:
                    print("too light ")
                    print(w)
                if w>heavy:
                    print("too heavy ")
                    print(w)
                if not int(round(time.time()))- animal_timer[animal_list.index(animaltag)]> nest_timeout:
                    print("too soon after last visit, time to next ")
                    print(int(round(time.time()))- animal_timer[animal_list.index(animaltag)]-nest_timeout)
                if not pi.read(ard_pi_BB1):
                    print("BB1 detected something - not safe to close door 1")
                if (
                    w > light
                    and w < heavy
                    and pi.read(ard_pi_BB1)
                    and int(round(time.time()))
                    - animal_timer[animal_list.index(animaltag)]
                    > nest_timeout
                ):
                    pi.write(pi_ard_door1, 0)  # close door1
                    MODE = 3
                    choice_flag = False
                    entry_flag = False
                    water_flag = True
                    break
            else:
                MODE = 2
                print("*", end=",")
                break

    # correct animal on scale
    if MODE == 3:
        print("\nweighing\n")
        # Weighing for entry
        flag_heavy = acquire_weight(animaltag)
        if flag_heavy:
            # Append data
            tick = pi.get_current_tick()
            save.append_event(
                "+", "", "ENTRY DENIED MULTIPLE ANIMALS", animaltag, tick
            )
            print("ENTRY DENIED MULTIPLE ANIMALS")
            MODE = 1
        if not flag_heavy:
            pi.write(pi_ard_door2, 1)  # open door 2
            tick = pi.get_current_tick()
            save.append_event("", "", "entry", animaltag, tick)
            print("\nPASS\n")
            print(animaltag)
            print(datetime.datetime.now())
            time.sleep(minimum_entry_time)
            print("waiting for exit")
            print(datetime.datetime.now())
            MODE = 4

    # wait for exit    
    if MODE == 4:
        if pi.read(ard_pi_BB2):
            pi.write(pi_ard_door2, 0)  # close door 2
            animal_timer[animal_list.index(animaltag)] = int(round(time.time()))
            MODE = 1