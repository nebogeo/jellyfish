import osc
import time

def sync(bpm):
    osc.Message("/eval",["(nz-sync z "+str(bpm)+")"]).sendlocal(8000)

bpm = 353.23
#bpm = 240.0
count = 0

while 1:
    if (count%4==0): 
        sync(bpm)
        print(60/bpm)
    time.sleep(60/bpm)
    count+=1
