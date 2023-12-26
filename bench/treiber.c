// Adapted from: https://en.wikipedia.org/wiki/Treiber_stack

#include <stdlib.h>

int _beginARW_, _endARWsucc_,_endARWfail_;
struct node_t {
   unsigned long val;
   struct node_t* next;
};

struct node_t *top;

//extern void __VERIFIER_assume(int);
//extern void __VERIFIER_error();
//extern int __VERIFIER_nondet_int();
//#define assert(b) { if (!(b)) { __VERIFIER_error(); } }
//#define assume(b) __VERIFIER_assume(b);

void __states() {
  int _ap1;
  _ap1 = top == (void *)0;
}

void push() {  /* while(1) { */
  int item;
  struct node_t n;
  n.val = item;
  struct node_t* oldTop;
  oldTop = top;
  n.next = oldTop;
  if(_beginARW_ | top == oldTop) {
      top = &n;
      assume(top != (void *)0);
      _endARWsucc_ = 1;
      return;
  } else { _endARWfail_=1; }
//end while
}

int pop() { /* while(1) { */
  int result;
  struct node_t* newTop;
  struct node_t* oldTop;
  oldTop = top;
  if(oldTop == (void*)0) { return 0; }
  newTop = oldTop->next;
  if(_beginARW_ | top == oldTop) {
      top = newTop;
      _endARWsucc_ = 1;
      return oldTop->val;
  } else { _endARWfail_=1; }
//end while
}
