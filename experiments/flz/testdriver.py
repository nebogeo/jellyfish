import osc

def convert_4bit(arr):
    return arr[3]|arr[2]<<1|arr[1]<<2|arr[0]<<3;

## this classifier is simply a filter for biasing against readings
## for different tokens in favour of orientations of existing ones
## noise is considered to be caused primarily by 'in between' sensor
## readings of the tangible tokens

class tangible_classifier:
    def __init__(self,tokens):
        self.token_current = "?"
        self.token_theory = "?"
        self.value_current = 0
        self.value_theory = 0
        self.confidence_time = 0
        self.token_change_time = 0.2
        self.token_orient_time = 0.1
        # convert to integer representation
        self.tokens = self.convert_tokens(tokens)

    def convert_token(self,patterns):
        return map(convert_4bit,patterns)

    def convert_tokens(self,tokens):
        return {k: self.convert_token(v) for k, v in tokens.items()}

    def value_to_token(self,val):
        for k,v in self.tokens.items():
            if val in v: return k
        return "?"

    def observation(self,dt,value):
        # have we witnessed a change?
        if value is not self.value_theory:
            self.value_theory = value
            self.token_theory = self.value_to_token(value)
            if self.token_current in self.tokens and \
               value in self.tokens[self.token_current]:
                # permutation of the current token is more likely
                self.confidence_time += self.token_orient_time
            else:
                # than a new token (could be rotation noise etc)
                self.confidence_time += self.token_change_time
            
        if self.confidence_time==0:
            self.token_current=self.token_theory
            self.value_current=self.value_theory
            self.confidence_time=0
        else:
            self.confidence_time=max(0,self.confidence_time-dt)
                    
        print(value,self.token_current,self.value_current)

tokens = {"circle":    [[0,0,0,0],[1,1,1,1]],
          "rectangle": [[0,1,0,1],[1,0,1,0]],
          "triangle":  [[1,1,0,0],[1,0,0,1],[0,0,1,1],[0,1,1,0]],
          "square":    [[0,0,0,1],[0,0,1,0],[0,1,0,0],[1,0,0,0],
                        [1,1,1,0],[1,1,0,1],[1,0,1,1],[0,1,1,1]]}

def test_classifier():
    c = tangible_classifier(tokens)
    c.observation(0.1,convert_4bit([1,0,0,0]))
    c.observation(0.1,convert_4bit([1,0,0,0]))
    c.observation(0.1,convert_4bit([1,0,0,0]))
    c.observation(0.1,convert_4bit([1,1,0,0]))
    c.observation(0.1,convert_4bit([1,1,0,0]))
    c.observation(0.1,convert_4bit([0,1,0,0]))
    c.observation(0.1,convert_4bit([0,1,0,0]))
    c.observation(0.1,convert_4bit([0,1,0,0]))
    c.observation(0.1,convert_4bit([0,1,0,0]))

#test_classifier()

####################################

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
    osc.Message("/eval",["(set-nz-grp! z "+str(grp)+")"]).sendlocal(8000)

send_grp(3)
    
osc.Message("/eval",["(set-scale '(2 4 2 2 2))"]).sendlocal(8000)
#osc.Message("/eval",["(set-scale '(2 2 2 2 2 2))"]).sendlocal(8000)

#osc.Message("/eval",["(synth-record \"dz\")"]).sendlocal(8000)

#send_pattern(["B+B-B",
#              "abcdc",
#              "acaaA",
#              "--A++"])

send_pattern(["........",
              "aaaaa",
              "-aAa+",
              "-aAa+"])



