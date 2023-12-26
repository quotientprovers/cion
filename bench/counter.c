#include <stdlib.h>
#include <stdio.h>

int _beginARW_, _endARWsucc_,_endARWfail_;char *_NOTE_;
int counter = 0;

// decr just returns if counter==0. States are:
// Q1(counter==0)
// Q2(counter>0)

void incr() { /* while(1) { */
   int v = counter;
   if(_beginARW_ | counter == v){
      counter = v+1;
      _endARWsucc_=1;
      return;
   } else {
      _endARWfail_=1;
   }
/* end while */
}
void decr() {  /* while(1) { */
   int v = counter;
   if (v == 0) return;
   if(_beginARW_ | counter == v){
      counter = v-1;
      _endARWsucc_=1;
      return;
   } else {
      _endARWfail_=1;
   }
/* end while */
}