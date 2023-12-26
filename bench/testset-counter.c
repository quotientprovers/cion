#include <stdlib.h>

int _beginARW_, _endARWsucc_,_endARWfail_;
int counter = 0;
int lock = 0;

void incr() { /* while(1) { */

    int l = lock;
    
    if(l == 0) {
        if (_beginARW_ | lock == 0){
            lock = 1;
            _endARWsucc_=1;
            // critical section
            counter = counter + 1;
            lock = 0;
            return;
        } else {
            _endARWfail_=1;
        }
    }
/* end while */
}
void decr() {  /* while(1) { */

    int l = lock;
    
    if(l == 0) {
        if (counter == 0) 
            return;

        if(_beginARW_ | lock == 0){
            lock = 1;
            _endARWsucc_=1;
            counter = counter - 1;
            lock = 0;
            return;
        } else {
            _endARWfail_=1;
        }
    }
/* end while */
}