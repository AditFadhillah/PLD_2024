#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#define __STDC_FORMAT_MACROS
#include <inttypes.h>

uint64_t heap[200]; /* deliberately small heap */
uint64_t *heapStart = heap;
uint64_t *heapEnd = heap+199;

uint64_t *freelist = heap;

void initialize_freelist() {
  *freelist = 0xfff09c169414613;
  *(freelist+1) = 200;
  freelist +=2;
}

int isHeapPointer(uint64_t *value) {
  if (value < heapStart) return 0;
  if (value > heapEnd) return 0;
  if (*(value-2) != 0xfff09c169414613) return 0;
  return 1;
}

uint64_t firstGlobal[100] = {0}; /* simulated global variables */
uint64_t *lastGlobal = firstGlobal+99;

uint64_t stackBottom[100] = {0}; /* simulated stack variables */
uint64_t *stackTop = stackBottom+99;

int stack_size = 1000; /* work stack for GC to use */

void mark() {
  uint64_t *stack[stack_size];
  uint64_t stackPointer = 0;
  uint64_t *tmp;
  uint64_t size;

  /* add root set to work stack */
  for (uint64_t *i = firstGlobal; i <= lastGlobal; i++)
    if (isHeapPointer((uint64_t*) *i))
      stack[stackPointer++] = (uint64_t*) *i;
  for (uint64_t *i = stackBottom; i <= stackTop; i++)
    if (isHeapPointer((uint64_t*) *i))
      stack[stackPointer++] = (uint64_t*) *i;

  printf("root set pushed, stackPointer == %"PRIx64"\n", stackPointer);

  while (stackPointer > 0) {
    tmp = stack[--stackPointer];
    size = *(tmp-1);
    if ((size & 0x8000000000000000) == 0) {
      /* if mark bit is clear */
      /* add pointer fields to work stack */
      for (uint64_t *i = tmp; i < tmp + size - 2; i++)
        if (isHeapPointer((uint64_t*) *i))
	  stack[stackPointer++] = (uint64_t*) *i;
      *(tmp-1) |= 0x8000000000000000; /* and set mark bit */
    }
  }
}


void sweep() {
  uint64_t *current = heapStart+2;
  uint64_t size, size2;
  freelist = NULL;
  while (current <= heapEnd) {
    size = *(current-1);
    if (size & 0x8000000000000000) { /* mark is set */
      size &= 0x7fffffffffffffff;
      *(current-1) = size; /* clear mark */
      printf("marked, size == %"PRIx64", content == %"PRIx64"\n", size, *current);
      current += size;
    } else { /* mark is clear */
      printf("unmarked\n");
      while (current+size < heapEnd &&
             ((size2 = *(current+size-1))
	      & 0x8000000000000000) == 0) {
        *(current+size-2) = 0; /* clear magic word */
        size += size2; /* join blocks */
	printf("size ==  %"PRIx64"\n", size);
      }
      /* add block to freelist */
      *(current-1) = size;
      *current = (uint64_t) freelist;
      freelist = current;
      current += size;
    }
  }
}

void gc() {
  mark();
  printf("mark done\n");
  sweep();
  printf("sweep done: freelist == %"PRIx64"\n",freelist-heapStart);
}

uint64_t *allocate(uint64_t size) {

  size += 2; /* make room for header words */
  for (int i = 0; i<2; i++) {
    uint64_t *previous = NULL;
    uint64_t *current = freelist;
    uint64_t currentSize = 0;
    printf("alloc: %d, size = %ld\n",i, size);
    
    while (current != NULL) {
      currentSize = *(current-1);
      if (currentSize > size+2) { /* split object */
	*(current+currentSize-size-2) = 0xfff09c169414613;
	*(current+currentSize-size-1) = size;
	*(current-1) -= size;
	return current+(currentSize-size);
      } else if (currentSize >= size) { /* return object */
	if (previous == NULL) freelist = (uint64_t*) *current;
	else *previous = *current;
	return current;
      } else { /* find next object in freelist */
	previous = current;
	current = (uint64_t*) *current;
      }
    }
    if (i > 0) return 0; /* already tried calling gc */
    gc(); /* call garbage collector */
  }
}

int main() {
  uint64_t *ll = NULL;
  uint64_t *cc = NULL;

  initialize_freelist();

  /* allocate pseudo-randomly until allocation fails */
  for (int i = 0; i<60; i++) {
    if ((cc = allocate((3*i)%11+2)) != 0) {
      printf("allocation successful: %ld\n", cc - heapStart);
      cc[0] = 0x1001*i;
      if (i%13 == 0) cc[1] =  firstGlobal[i%11]; else cc[1] = 0;
      ll = cc;
      firstGlobal[i%17] = (uint64_t) ll;
    } else {
      printf("Allocation failed at i == %d, size = %d\n", i, (3*i)%11+2);
      exit(0);
    }
  }
}
