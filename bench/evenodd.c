#include <stdlib.h>
#include <stdio.h>

int _beginARW_, _endARWsucc_,_endARWfail_;char *_NOTE_;
int counter = 0;

// Similar to counter.c, except that decr is always defined and the state changes. 
// In each state, the method brings you to the other state.
// The methods cannot bring you back to the current state.
// Q0: (counter%2 == 0)
// Q1: (counter%2 == 1)

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
   if(_beginARW_ | counter == v){
      counter = v-1;
      _endARWsucc_=1;
      return;
   } else {
      _endARWfail_=1;
   }
/* end while */
}