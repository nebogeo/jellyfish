import svgwrite

start_time = -1


def draw_tick(dwg, time, text):
    time -= start_time
    time *=100
    #print(time)
    #svgwrite.rgb(10, 10, 16, '%')
    col = 'black'
    if text=="tick-real": col='red'
    if text=="tick-time": col='yellow'
    if text=="sync": col='blue'
    if text=="setting": col='orange'

    start = 0
    end = 100
    if text=="tick-real": end=10
    if text=="setting": end=50
    if text=="tick-time": start=50
    if text=="sync": 
        start=40 
        end=60

    dwg.add(dwg.line((time, start), (time, end), stroke=col))
    #dwg.add(dwg.text(text, insert=(time, 50), fill='black'))
    
dwg = svgwrite.Drawing('debug.svg', profile='tiny')

with open('debug.txt', 'r') as lines:
    for line in lines:
        line=line.split()
        if start_time==-1: start_time=int(line[0])
        draw_tick(dwg,int(line[0])+(int(line[1])/float(4294967295)),line[2])
                                                      
dwg.save()

