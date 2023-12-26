#include <stdlib.h>

int _beginARW_, _endARWsucc_,_endARWfail_;char *_NOTE_;
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

int gk;
int reader_k, k;
struct node_t *reader_x;
struct node_t *reader_y;
struct node_t *x;
struct node_t *y;

void __fullstates() {
    LAND3(x->next == y, ! x->del, k == y->key);
    LAND7(x->next == y, ! x->del, x->key < k, k < y->key, 
          reader_x == x, reader_x->key < reader_k, reader_k < reader_y->key);
    LAND6(x->next == y, ! x->del, x->key < k, k < y->key, reader_x == x, reader_k == reader_y->key);
    LAND4(x->next == y, ! x->del, x->key < k, k < y->key);
    LAND6(x->next == y, ! x->del, k == y->key, reader_x == x, 
          reader_x->key < reader_k, reader_k < reader_y->key);
    LAND5(x->next == y, ! x->del, k == y->key, reader_x == x, reader_k == reader_y->key);

    LAND3(x->next == y, x->del, k == y->key);
    LAND7(x->next == y, x->del, x->key < k, k < y->key, 
          reader_x == x, reader_x->key < reader_k, reader_k < reader_y->key);
    LAND6(x->next == y, x->del, x->key < k, k < y->key, reader_x == x, reader_k == reader_y->key);
    LAND4(x->next == y, x->del, x->key < k, k < y->key);
    LAND6(x->next == y, x->del, k == y->key, reader_x == x, 
          reader_x->key < reader_k, reader_k < reader_y->key);
    LAND5(x->next == y, x->del, k == y->key, reader_x == x, reader_k == reader_y->key);

    //LAND6(x->next == y, ! x->del, k == y->key, reader_x == y, reader_x->key < reader_k, reader_k < reader_y->key);
    //LAND5(x->next == y, ! x->del, k == y->key, reader_x == y, reader_k == reader_y->key);
}


// locate(k):
// // return the node with the key
// // … or else x points to the last node whose key is below k
// 1. x, y = root
// 2. while ( y != null & y.key < k ) {
// 3.      x = y;
// 4.      y = x.next  }
// 5. return (x,y);

// int contains() {  /* while(1) { */
//    int k;
//    struct node_t* x; struct node_t* y; 

//    // contains implementation
//    if ( y == (void *)0)
//        return 0;
//    else if ( y->key != k )
//        return 0;
//    else if ( y->del == 0)
//        return 1;
//    else return 0;
// // end while
// }



int insert() {  /* while(1) { */
    struct node_t z;
    //int k; struct node_t* x; struct node_t* y; 

   // __VERIFIER_assume(x->next == y);
   y = x->next; // emulate the interference-aware traversal.

   if(_beginARW_ | (x->next == y)) {
       if (x->del == 0) {
          if (y->key != k) {
            z.next = y;
            x->next = &z;
            _endARWsucc_=1;
            return 1;
          } else {_endARWfail_=1;return 0; }
      } else { _endARWfail_=1; }
   } else { _endARWfail_=1; } 
// end while
}


int delete() {  /* while(1) { */
   // int k; struct node_t* x; struct node_t* y; 

   // __VERIFIER_assume(x->next == y);
   y = x->next;

   if(_beginARW_ | (x->next == y)) {
       if (x->del == 0) {
            if (y->key == k) {
                y->del = 1;
                x->next = y->next;
                _endARWsucc_=1;
                return 1;
            } else {
                _endARWfail_=1;
                return 0;
            }
       } else { _endARWfail_=1; } 
   } else { _endARWfail_=1; } 
// end while
}