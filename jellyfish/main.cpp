#include <stdio.h>
#include "jellyfish.h"
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    vec3 *heap=new vec3[256];
    jellyfish jf(heap,256);

    u32 a=0;
    jf.poke(a++,vec3(LDA,13,0));
    jf.poke(a++,vec3(LDL,20,0));
    jf.poke(a++,vec3(ADD,0,0));
    jf.poke(a++,vec3(DUP,0,0));
    jf.poke(a++,vec3(STA,13,0));
    jf.poke(a++,vec3(SIN,0,0));
    jf.poke(a++,vec3(STA,14,0));

    jf.poke(a++,vec3(LDA,14,0));
    jf.poke(a++,vec3(LDA,15,0));
    jf.poke(a++,vec3(CRS,0,0));
    jf.poke(a++,vec3(STA,16,0));
    
    jf.poke(a++,vec3(JMP,0,0));

    jf.poke(15,vec3(0,1,0));

    srand(2334);

//    jf.trash();
/*    
*/

    while(1)
    {
        printf("--\n");
        jf.run();
        jf.pretty_dump();
        sleep(1);
    }

    return 0;
}
