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
void __choice() {
    // int _st1, _st2, _st3, _st4;
    // all combinations:
    //    gx->del 
    //    gx->next == gy
    //    reader_k == gy->key
    //    k == gy->key
    //    gx->key < reader_k 
    //    gx->key < k 
    //    gy->key > reader_k 
    //    gy->key > k

    // all combinations:
    //    gx->del 
    //    gx->next == gy
    // gx->key < reader_k < gy->key || reader_k == gy->key
    // gx->key < k < gy->key || k == gy->key
    // gy->next == gz
    //  reader_x == y || (reader_x == x && reader_y = y)
        CHOICE(x->del, !x->del);
        CHOICE(reader_x->del, !reader_x->del);
        CHOICE(x->next == y, !x->next == y);
        CHOICE(reader_x->next == reader_y, ! reader_x->next == reader_y);
        CHOICE(reader_k < reader_y->key, reader_k == reader_y->key);
        CHOICE(k < y->key, k == y->key);
        CHOICE(reader_x == x, reader_x == y); // && reader_y == y


        // if(CHOICE) { assume(x->del); }
        // else { assume(! x->del); }
        // if(CHOICE) { assume(reader_x->del); }
        // else { assume(! reader_x->del); }
        // if(CHOICE) { assume(x->next == y); }
        // else { assume(! x->next == y);}
        // if(CHOICE) { assume(reader_x->next == reader_y); }
        // else { assume(! reader_x->next == reader_y); }
        // if(CHOICE) { assume(reader_x->key < reader_k < reader_y->key ); } 
        // else { assume(reader_k == reader_y->key); }
        // if(CHOICE) { assume(x->key < k < y->key); }
        // else { assume(k == y->key); }  
        // if(CHOICE) { assume(reader_x == x); assume(reader_y == y); }
        // else { assume(reader_x == y); }



    // _st1 = LAND4(reader_k == k, gy->key == gk, !gx->del, gx->next == gy);  // x points to the key
    // _st2 = LAND3(gy->key < gk, !gx->del, gx->next == gy);  // x last node before key. y is non-null but above key
    // _st3 = LAND3(gy->key == gk, gx->del, gx->next == gy);  // x points to the key
    // _st4 = LAND3(gy->key < gk, gx->del, gx->next == gy);  // x last node before key. y is non-null but above key
    //_st3 = LAND3(gx->key < gk, gx->next == gy, gy == (void *)0); // x last node before key; y null
    //_st3 = LAND2(gx == (void *)0, gy == (void *)0); // empty list
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
    // capture the x,y pair
    // int _t = __VERIFIER_nondet_int();
    // if(_t) { k = gk; x = gx; y = gy; }
    // else { k = gk; x = gy; }

   // capture the x,y pair
   x = gx; y = gy;

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
    // capture the x,y pair
    // int _t = __VERIFIER_nondet_int();
    // if(_t) { k = gk; x = gx; y = gy; }
    // else { k = gk; x = gy; }

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
    // capture the x,y pair
    // int _t = __VERIFIER_nondet_int();
    // if(_t) { k = gk; x = gx; y = gy; }
    // else { k = gk; x = gy; }

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