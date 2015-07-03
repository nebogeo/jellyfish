import driver
import time
import osc

driver.init()

print(driver.read_all())

def lookup(code):
    if code==0: return " "
    if code==1: return "a"
    if code==2: return "b"
    if code==3: return "c"
    if code==4: return "d"
    if code==5: return "+"
    if code==6: return "-"
    if code==7: return ">"
    if code==8: return "<"
    if code==9: return "A"
    if code==10: return "B"
    if code==11: return "C"
    if code==12: return "D"
    if code==13: return "["
    if code==14: return "]"
    if code==15: return "."


def send_lz(blocks,last):
    conv = ""
    for block in blocks:
        conv += lookup(block)

    if last!=conv:
        last=conv
        print conv
        osc.Message("/eval",["(lz-prog l 0 \""+conv+"\")"]).sendlocal(8000)
    return last

last = ""

while 1:
    last=send_lz(driver.read_all(),last)
    time.sleep(0.5)
