#include <stdlib.h>
#include <stdio.h>

int _beginARW_, _endARWsucc_,_endARWfail_;


int range = 0;
int *items;

void init(int n){
    items = (int*)malloc(n * sizeof(int));
}

void push(int v){
    items[range] = v;
    range = range + 1;
}

int pop(){
    // int r = range;
    // while(r >= 0){
    //     int v = items[r];
    //     if(v != (void*)0){
    //         items[r] = (void*)0;
    //         return v;
    //     }
    //     r = r - 1;
    // }
    // return -1; // stack is empty

    int r = range;
    if (r >= 0){
        int v = items[r];
        if (_beginARW_ | v != (void*)0){
            items[r] = (void*)0;
            _endARWsucc_=1;
            return v;
        }
    }
    else{
        _endARWfail_=1;
    }
}