# end of session: remove animals, write a note and run this script
import RPi.GPIO as GPIO
import os
import pandas as pd
import datetime
import numpy as np
from github import Github
from github import InputGitTreeElement

animal_list = [
    "34443624695",
    "34443624808",
    "137575399507",
    "34443624982",
    ]  # insert your mouse tags here manually
earmarks = ["R", "L", "RL", "nm"]
database_list = [
    "04435/1o1-42717",
    "04435/1o1-42718",
    "04435/1o1-42719",
    "04435/1o1-42720",
    ] # this is helpful for logging data remotely

print('********** getting user input **********')
print("animal list in this script is:")
print(animal_list)
print("database list in this script is:")
print(database_list)
test_answer2=int(input("continue with these data (1) or stop (0) and input diff data in the script?"))
if test_answer2==0:
    quit()
note = [
    "test lights 2230/1030, lick thresh 100, 120s wheel access 0.1s water is 10ul 1 pellet"
]
print("your standard note is")
print(note)
test_answer=int(input("log your standard note (0) or something else (1) ?"))
if test_answer==1:
    note=input("type your note")
injection_time=input("type injection time")
issue=input("type any issues during the run")
print('********************** input complete, logging end **********************')

fd = "/home/pi/Documents/Data/"
os.chdir(fd)

# mark end of session in event list
class SaveData:
    def append_event(
        self, rotation, food_time, event_type, animaltag, FED_position, hardware_time
    ):  # add hardware time call outs to below!

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

save = SaveData()
for x in range(np.size(animal_list)):
    animaltag = animal_list[x]
    save.append_event(issue, note, "end session", animaltag, injection_time, "")
print("end logged")


# # deposit welfare data
# g = Github("") #put your token here
# repo = g.get_user().get_repo("Switch_maze")  # repo name
# file_list = list()
# file_names = list()
# for i, entry in enumerate(animal_list):
#     file_list.append(fd + animal_list[i] + "_weight.csv")
#     file_names.append("wdata/" + database_list[i] + "_weight.csv")
# commit_message = "weights from 5s automatic measurement during entry"
# master_ref = repo.get_git_ref("heads/main")
# master_sha = master_ref.object.sha
# base_tree = repo.get_git_tree(master_sha)
# element_list = list()
# for i, entry in enumerate(file_list):
#     with open(entry) as input_file:
#         data = input_file.read()
#     element = InputGitTreeElement(file_names[i], "100644", "blob", data)
#     element_list.append(element)
# tree = repo.create_git_tree(element_list, base_tree)
# parent = repo.get_git_commit(master_sha)
# commit = repo.create_git_commit(commit_message, tree, [parent])
# master_ref.edit(commit.sha)
# print("welfare database updated")