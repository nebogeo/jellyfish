from numpy import *
import wave 
import audioop

wav_in=wave.open('phb.wav','r')
contents=wav_in.readframes(wav_in.getnframes())
# contents now holds all the samples in a long text string.
samples_list=[]; ctr=0

while 1:
    try:
        samples_list.append(audioop.getsample(contents,2,ctr))
        # "2" refers to no. of bytes/sample, ctr is sample no. to take.
        ctr+=1
    except:
        # We jump to here when we run out of samples
        break

out_array=array(samples_list)
out_fft=fft.fft(out_array)

for i in out_fft:
    print i

