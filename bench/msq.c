#include <stdlib.h>

int _beginARW_, _endARWsucc_,_endARWfail_; char *_NOTE_;
struct node_t {
   unsigned long val;
   struct node_t* next;
};

struct queue { 
  struct node_t *head;
  struct node_t *tail;
};
struct queue Q;


void __states() {
  int _ap1; int _ap2; //int _s3; int _s4;
  _ap1 = Q.tail == Q.head;
  _ap2 = Q.tail->next == (void *)0;
  // _s1 = (Q.tail == Q.head && Q.tail->next != (void *)0);
  // _s2 = (Q.tail == Q.head && Q.tail->next == (void *)0);
  // _s3 = (Q.tail != Q.head && Q.tail->next != (void *)0);
  // _s4 = (Q.tail != Q.head && Q.tail->next == (void *)0);
}

int enq() {  /* while(1) { */
  struct node_t* next;
  struct node_t* node;
  struct node_t* tail;
  struct node_t* mytl;
  int v;

  //node = malloc(sizeof(struct node_t));
  struct node_t node; 
  node.val = v;
  tail = Q.tail; 
  next = tail->next;
  assume(&node != Q.tail); assume(&node != tail->next);  // freshness of malloc

  if (Q.tail == tail) {  // if (Q.tail != tail) {continue;}
    if (next == (void *)0) {
      if (_beginARW_ | tail->next == next) { /* CAS SUCCEED. Breakout. */ //if (CAS(&tail->tl,next,node)) break;
        tail->next = &node;
        _endARWsucc_=1;
        return 0;
      }
      else { 
      _endARWfail_=1;
      }
    }
  }
/* end while */ }


int deq() {  /* while(1) { */
  struct node_t* head;
  struct node_t* tail;
  struct node_t* next;
  struct node_t* myhd;

  int pval;

  head = Q.head;
  tail = Q.tail;
  next = head->next;

  if (Q.head == head) { // if (Q.head != head) {continue;}
    if (head == tail) {
      if (next == (void *)0){
        return 0;
      }
    }
    else {
      pval = next->val;
      if (_beginARW_ | Q.head == head) { /* CAS SUCCEED. Breakout. */ //if (CAS(&Q->head,head,next)) break;
        Q.head = next;
        _endARWsucc_=1;
        return pval;
      }
      else { 
      _endARWfail_=1;
      }
    }
  }
/* end while */ }


int adv_tail(){ /* while(1) { */
  struct node_t* tail;
  struct node_t* next;
  tail = Q.tail;
  next = tail->next;
  if (next != (void *) 0){
      if (_beginARW_ | Q.tail == tail) { /* CAS SUCCEED. Breakout. */ //if (CAS(&Q->tail,tail,next)) break;
        Q.tail = next;
        _endARWsucc_=1;
        return 0;
      }
      else { _endARWfail_=1; }
  }
/* end while */}
