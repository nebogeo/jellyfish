import osc,time
from random import choice

code = [
"""(play-now (mul (sine 440) (adsr 0 0.1 0 0)) 0)""",
"""(play-now (mul (sine (add 400 (mul 1000 (sine 220)))) (adsr 0 0.1 0 0)) 0)""",
"""(play-now (mul (sine (add 400 (mul 1000 (saw 20)))) (adsr 0 0.1 0 0)) 0)""",
]


while 1:

    print("sending")
    #osc.Message("/eval",[choice(code)]).sendto("192.168.1.192",8000)
    osc.Message("/eval",[choice(code)]).sendlocal(8000)
    time.sleep(0.3)
