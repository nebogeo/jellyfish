import svgwrite
import ctypes

start_time = -1

real_count = 0
future_count = 0
setting_count = 0
tick_count = 0

def draw_tick(dwg, time, text):
    global real_count
    global setting_count
    global tick_count
    global future_count
    time -= start_time
    time *=100
    #print(time)
    #svgwrite.rgb(10, 10, 16, '%')
    col = 'black'
    if text=="tick-real": col='red'
    if text=="tick-time": col='yellow'
    if text=="tick-future": col='green'
    if text=="sync": col='blue'
    if text=="setting": col='orange'
    
    start = 0
    end = 100
    if text=="tick-real": 
        print time
        start=real_count
        end=real_count+10
        real_count=(real_count+12)%100
    if text=="tick-future": 
        print time
        start=future_count
        end=future_count+10
        future_count=(future_count+12)%100
    if text=="setting": 
        start=setting_count
        end=setting_count+20
        setting_count=(setting_count+5)%100
    if text=="tick-time": 
        start=tick_count
        tick_count=(tick_count+10)%100
    if text=="sync": 
        start=40 
        end=60

    if text!="tick-real":
        if text=="tick-time":
            width=5
            dwg.add(dwg.circle(center=(time, start), r=6, fill=col, opacity='0.8'))
        else:
            dwg.add(dwg.line((time, start), (time, end), stroke=col))
    
        dwg.add(dwg.text(text, insert=(time, start), 
                         fill='black', font_size=6))
    
dwg = svgwrite.Drawing('debug.svg', profile='tiny')

with open('debug.txt', 'r') as lines:
    for line in lines:
        line=line.split()
        if start_time==-1: start_time=int(line[0])
        converted = ctypes.c_long(int(line[1])).value

        print("---",converted)

        draw_tick(dwg,int(line[0])+(converted/float(0xffffffff)),line[2])
    
dwg.save()

