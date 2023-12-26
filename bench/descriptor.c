#include <stdlib.h>
#include <stdio.h>

int _beginARW_, _endARWsucc_,_endARWfail_;char *_NOTE_;

struct node_t { int tid; };
struct node_t* descriptor;
int lock;

// Mutual exclusion. Critical section appends to a list.
// Q0: (lock == 0   && descriptor=NULL)
// Q1: (lock == tid && descriptor=NULL)
// Q2: (lock == 0   && descriptor!=NULL)
// Q3: (lock == tid && descriptor!=NULL)

void claim() {
// while(1) {
  int myTid;
  // Below n should be malloc'd but CIL does a strange translation otherwise
  // = malloc(sizeof(struct node_t));
  struct node_t n;
  assume(myTid > 0);
  n.tid = myTid;
  if (descriptor == 0) {
    int l = lock;
    if (l == 0) {
      if(_beginARW_ | l == lock) {
        lock = myTid;
        _endARWsucc_ = 1;
        descriptor = &n; 
        lock = 0;
        return;
      } else {
          _endARWfail_ = 1;
      }
    }
  }
// end while
}

void disclaim() {
// while(1) {
  int myTid;
  assume(myTid > 0);
  if (descriptor != 0) {
    int l = lock;
    if (l == 0) {
        if(_beginARW_ | l == lock) {
            lock = myTid;
            _endARWsucc_ = 1;
            descriptor = 0;
            lock = 0;
            return;
        } else {
            _endARWfail_ = 1;
        }
    }
  }
}