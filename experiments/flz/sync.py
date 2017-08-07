import osc
import time

def sync(bpb,bpm):
    osc.Message("/sync",[bpb,bpm]).sendlocal(8000)

#bpm = 753.23
bpm = 40.0
count = 0
bpb = 8

while 1:
    if (count%bpb==0): 
        print("sync")
        sync(bpb,bpm)
    t=60/bpm
    print(t)
    time.sleep(t-0.0005)
    count+=1
