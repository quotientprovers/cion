#include <stdlib.h>
#include <stdio.h>

#define SIZE 20

int _beginARW_, _endARWsucc_,_endARWfail_; 
int back = 0;

int items[SIZE];

void init (){
    //items = malloc(SIZE * sizeof(int));
    int* item = (void *)0;
    for (int i = 0; i < SIZE; i++)
        items[i] = *item;
}

void enq (int x) {
    int i = back ++;
    // items[i] = x;

    int* item = items[i];
    if(_beginARW_ | items[i] == item){
        items[i] = x;
        _endARWsucc_ = 1
    }
    else{
        _endARWfail_ = 1
    }
}

int deq () {
    int range;
    int* x;

    // while (1) {
    //     range = back - 1;
    //     for (int i = 0; i <= range; i ++) {
    //         x = swap (&items[i], (void *)0);
    //         if(x != (void *)0)
    //             return *x;
    //     }
    // }

    /* while (1) { */
        range = back - 1;
        int i = 0;
        if (i <= range){
            int item = &items[i];
            if (_beginARW_ | &items[i] = item){
                x = items[i];
                items[i] = (void *)0;
                if(x != (void *)0){
                    _endARWsucc_ = 1;
                    return *x;
                }
                else {
                    i = i + 1;
                }
            }
            else {
                _endARWfail_ = 1;
            }
        }
    /* } end while */
}
