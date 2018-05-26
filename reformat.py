# Just a quick script to reformat the space delimited data file to csv
import re

lines = open ("GMM_dataset.txt")
clean = open("GMM_dataset_clean.txt", "w")
for line in lines:
    line = re.sub("^ +", "", line)
    line = re.sub(" +", ",", line)
    arr = line.split(",")
    clean.write(str(float(arr[0]))+","+str(float(arr[1].strip()))+"\n")
