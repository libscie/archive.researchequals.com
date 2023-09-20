# rescue main -- run this when you have mice in the maze that need to be extracted
from rescue_functions import *

while True:
    if MODE == 1 and pi.read(ard_pi_2) and return_flag<animals:
        pi.write(pi_ard_1, 1)  # open door1
        pi.write(pi_ard_2, 0)  # close door2
        return_flag=return_flag+1
        print("returned")
        print(return_flag)
        MODE=2
    
    if MODE == 2:
        # wait for sem to empty
        w = read_scale()
        if w < 10 and pi.read(ard_pi_1):
            time.sleep(2)
            print("sem likely empty")
            MODE=3
        elif w > 10:
            pi.write(pi_ard_1, 1)  # open door1
            time.sleep(1)

    if MODE == 3:
        # check sem is empty
        w = read_scale()
        if w < 5:
            pi.write(pi_ard_1, 0)  # close door1
            pi.write(pi_ard_2, 1)  # open door2
            print("sem confirmed empty")            
            MODE=1
            if return_flag==animals:
                print("all animals returned")
                try:
                    # initializing the server connection
                    yag = yagmail.SMTP(
                        user="switchmazebot2000@gmail.com", oauth2_file="~/oauth2_creds.json"
                    )
                    # sending the email
                    yag.send(
                        to="m.m.karnani@vu.nl",
                        subject="Switch maze rescue complete",
                        contents="""
                        Switch maze rescue is complete.
                        """,
                    )
                    print("Email sent successfully")
                except:
                    print("Error, email was not sent")
        elif w > 5:
            print("sem not empty on double check")
            pi.write(pi_ard_1, 1)  # open door1
            time.sleep(1)
            MODE=2