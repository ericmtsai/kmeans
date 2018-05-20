import numpy as np
import random

X = np.random.randint(10,45,(25,2))
Y = np.random.randint(55,70,(25,2))
Z = np.vstack((X,Y))
 
# convert to np.float32
Z = np.float32(Z)
f = open("data.txt", "w")
for z in Z:
    fst = z[0]+random.random()
    snd = z[1]+random.random()
    f.write("["+str(fst)+","+str(snd)+"]"+"\n")
f.close()
