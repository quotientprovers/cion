#include <stdlib.h>

int _beginARW_, _endARWsucc_,_endARWfail_;
struct node_t {
   int key;
   int del; 
   struct node_t* next;
};

struct node_t *root;

extern void __VERIFIER_assume(int);
extern void assume(int);
extern void __VERIFIER_error();
extern int __VERIFIER_nondet_int();

#define assert(b) { if (!(b)) { __VERIFIER_error(); } }
//#define assume(b) __VERIFIER_assume(b);

void __states() {
  int _ap1;
  _ap1 = root == (void *)0;
}

// locate(k):
// // return the node with the key
// // … or else x points to the last node whose key is below k
// 1. x, y = root
// 2. while ( y != null & y.key < k ) {
// 3.      x = y;
// 4.      y = x.next  }
// 5. return (x,y);

int contains() {  /* while(1) { */
   int k;
   struct node_t* x; struct node_t* y; 
   k = __VERIFIER_nondet_int();
   // abstract over locate
   if(__VERIFIER_nondet_int()) {
       assume(x->key == k);           // x points to the key
       assume(x->next == y);
   } else if(__VERIFIER_nondet_int()) {
       assume(x->key < k);            // x last node before key
       assume(x->next == y);
       assume(y != (void *)0);        // y is non-null but above key
       assume(y->key > k);
   } else if(__VERIFIER_nondet_int()) {
       assume(x->key < k);            // x last node before key
       assume(x->next == y);
       assume(y == (void *)0);        // y is null
   } else {
       assume(x == (void *)0);             // empty list
       assume(y == (void *)0);
   }

   // contains implementation
   if ( y == (void *)0)
       return 0;
   else if ( y->key != k )
       return 0;
   else if ( y->del == 0)
       return 1;
   else return 0;
// end while
}



int insert() {  /* while(1) { */
   int k;
   struct node_t z;
   struct node_t* x; struct node_t* y; 
   k = __VERIFIER_nondet_int();
   // abstract over locate
   if(__VERIFIER_nondet_int()) {
       assume(x->key == k);           // x points to the key
       assume(x->next == y);
   } else if(__VERIFIER_nondet_int()) {
       assume(x->key < k);            // x last node before key
       assume(x->next == y);
       assume(y != (void *)0);        // y is non-null but above key
       assume(y->key > k);
   } else if(__VERIFIER_nondet_int()) {
       assume(x->key < k);            // x last node before key
       assume(x->next == y);
       assume(y == (void *)0);        // y is null
   } else {
       assume(x == (void *)0);             // empty list
       assume(y == (void *)0);
   }

   if(_beginARW_ | (x->next == y && x->del == 0)) {

      if (y->key != k) {
          z.next = y;
          x->next = &z;
          _endARWsucc_=1;
          return 1;
      } else {
          _endARWsucc_=1;
          return 0;
      }
   } else { _endARWfail_=1; } 
// end while
}


int delete() {  /* while(1) { */
   int k;
   struct node_t* x; struct node_t* y; 
   k = __VERIFIER_nondet_int();
   // abstract over locate
   if(__VERIFIER_nondet_int()) {
       assume(x->key == k);           // x points to the key
       assume(x->next == y);
   } else if(__VERIFIER_nondet_int()) {
       assume(x->key < k);            // x last node before key
       assume(x->next == y);
       assume(y != (void *)0);        // y is non-null but above key
       assume(y->key > k);
   } else if(__VERIFIER_nondet_int()) {
       assume(x->key < k);            // x last node before key
       assume(x->next == y);
       assume(y == (void *)0);        // y is null
   } else {
       assume(x == (void *)0);             // empty list
       assume(y == (void *)0);
   }

   if(_beginARW_ | (x->next == y && x->del == 0)) {
      if (y->key == k) {
          y->del = 1;
          x->next = y->next;
          _endARWsucc_=1;
          return 1;
      } else {
          _endARWsucc_=1;
          return 0;
      }
   } else { _endARWfail_=1; } 
// end while
}