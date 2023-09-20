from RFIDreader_newcohort_functions import *

identified=0
while True:
    # start
    if MODE == 1:
        print("\n MODE 1 open start \n")
        GPIO.output(pi_ard_1, True)  # open door1
        MODE = 2
    # animal on scale
    if MODE == 2 and GPIO.input(ard_pi_2):  # BB2 broken
        print("\nMODE 2\n")
        w = read_scale()
        if w > 10 and w < heavy and GPIO.input(ard_pi_1):
            GPIO.output(pi_ard_1, False)  # close door1
            print("\nMODE 2 confirming sem occupancy\n")
            time.sleep(1)
            w = read_scale()
            if w > 10 and w < heavy:  # one animal
                print(datetime.now())
                MODE = 3
            else:
                MODE = 1
    # animal on scale
    if MODE == 3:
        print("\nMODE 3\n")
        secs = int(round(time.time()))
        # check RFID
        print("Reading animal tag")
        while True:
            animaltag = RFID_readtag("RFID1")
            if not animaltag in animal_list:
                MODE = 5
                print("new animal")
                print(animaltag)
                identified=identified+1
                animal_list.append(animaltag)
                break
            else:
                MODE = 5
                print("already identified")
                print(animaltag)
                break
    # open
    if MODE == 5 and GPIO.input(ard_pi_2):
        print("\nidentified\n")
        print(datetime.now())
        GPIO.output(pi_ard_2, False)  # close door 2
        GPIO.output(pi_ard_1, True)  # open door1
        time.sleep(exit_wait)  # safety timer so outgoing is not trapped on exit
        MODE = 1
    # all identified -- send an email to user   
    if identified==cohort_size:
        print("identified animals (copy-paste into your switch_maze_functions.py):")
        print(animal_list)
        identified=identified+1
        text_trap = io.StringIO()
        sys.stdout = text_trap # remember to restore stdout function with sys.stdout=sys.__stdout__