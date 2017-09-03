import osc
import time

def sync(beat,bpm):
    osc.Message("/sync",[beat,bpm]).sendlocal(8000)
    osc.Message("/sync",[beat,bpm]).sendlocal(4000)
    osc.Message("/sync",[beat,bpm]).sendto("192.168.0.21",8000)

#bpm = 753.23
bpm = 255.3
count = 0
beat = 0

while 1:
    sync(beat,bpm)
    t=60/bpm
    print(t)
    time.sleep(t-0.0005)
    count+=1
    beat+=1
