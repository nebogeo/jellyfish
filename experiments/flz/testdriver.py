import osc

def convert_symbols(s):
    return {convert_4bit(v):k for k, v in s.items()}

symbols = {" ":[1,1,1,1],".":[0,0,0,0],
           "[":[1,0,1,0],"]":[0,1,0,1],
           "A":[1,1,0,0],"B":[0,1,1,0],"C":[0,0,1,1],"D":[1,0,0,1],
           "a":[1,0,0,0],"b":[0,1,0,0],"c":[0,0,1,0],"d":[0,0,0,1],
           "<":[0,1,1,1],">":[1,0,1,1],"+":[1,1,0,1],"-":[1,1,1,0]}




def send_pattern(pat):
    osc.Message("/eval",["(lz-prog l 0 \""+pat[0]+"\")\n "+\
                         "(lz-prog l 1 \""+pat[1]+"\")\n "+\
                         "(lz-prog l 2 \""+pat[2]+"\")\n "+\
                         "(lz-prog l 3 \""+pat[3]+"\")\n "]).sendlocal(8000)


def send_grp(grp):
    osc.Message("/eval",["(set-nz-vx! z 0)(set-nz-grp! z "+str(grp)+")"]).sendlocal(8000)

send_grp(4)

osc.Message("/eval",["(set-nz-vx! z 0)"]).sendlocal(8000)
    
#osc.Message("/eval",["(set-scale major)"]).sendlocal(8000)
osc.Message("/eval",["(set-scale pentatonic-minor)"]).sendlocal(8000)
#osc.Message("/eval",["(set-scale hawaiian)"]).sendlocal(8000)
#osc.Message("/eval",["(set-scale '(2 2 2 2 2 2))"]).sendlocal(8000)

#osc.Message("/eval",["(synth-record \"dz\")"]).sendlocal(8000)
osc.Message("/eval",["(set-nz-bar-reset! z #f"]).sendlocal(8000)

send_pattern(["B-B-B",
              "--C--",
              "acDbd",
              ">-A++"])




