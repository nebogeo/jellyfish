import osc

def convert_4bit(arr):
    return arr[3]|arr[2]<<1|arr[1]<<2|arr[0]<<3;

def convert_pattern(pat):
    return map(convert_4bit,pat)

def convert_patterns(pats):
    return {k: convert_pattern(v) for k, v in pats.items()}


patterns = {"circle":    [[0,0,0,0],[1,1,1,1]],
            "rectangle": [[0,1,0,1],[1,0,1,0]],
            "triangle":  [[1,1,0,0],[1,0,0,1],[0,0,1,1],[0,1,1,0]],
            "square":    [[0,0,0,1],[0,0,1,0],[0,1,0,0],[1,0,0,0],
                          [1,1,1,0],[1,1,0,1],[1,0,1,1],[0,1,1,1]]}

token_change_time = 0.5
token_orient_time = 0.1

# symbols = 

print(convert_patterns(patterns))



def send_pattern(pat):
    osc.Message("/eval",["(lz-prog l 0 \""+pat[0]+"\")\n "+\
                         "(lz-prog l 0 \""+pat[1]+"\")\n "+\
                         "(lz-prog l 0 \""+pat[2]+"\")\n "+\
                         "(lz-prog l 0 \""+pat[3]+"\")\n "]).sendlocal(8000)

send_pattern(["ad-B+",
              "ad+C-",
              "ab+D-",
              "++A--"])

