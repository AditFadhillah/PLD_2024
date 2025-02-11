#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>
#include <inttypes.h>

#define __STDC_FORMAT_MACROS
#include <inttypes.h>

uint64_t heap[200]; /* deliberately small heap, don't change size it's hardcoded*/
uint64_t *heapStart = heap;
uint64_t *heapEnd = heap+199;

uint64_t *freelist = heap;


void initialize_freelist() {
  *freelist = 0xfff09c169414613;
  *(freelist+1) = 200;
  freelist +=2;
}

// uint64_t *isHeapPointer(uint64_t *value) {
//     // pointer not in heap
//     if (value < heapStart || value >= heapEnd) return 0;

//     for (uint64_t *current = heapStart; current < heapEnd; current++) {
//         // check for the magic word to find the start of an object
//         if (*current == 0xfff09c169414613) {
//             uint64_t size = *current;
//             // Check if the value falls within the bounds of this object
//             if (value >= current && value < current + size) {
//                 return current; // Return a pointer to the first field of the object
//             }
//             current += size; // Move to the next object
//         }
//     }
//     return 0; // not a valid object pointer
// }

uint64_t* isHeapPointer(uint64_t* value) {
    if (value < heapStart) {
        return 0;
    }
    if (value > heapEnd) {
        return 0;
    }
    uint64_t* cursor = value;

    while (cursor >= heapStart && *cursor != 0xfff09c169414613) {
        cursor--;
    }
    if (value < heapStart) {
        return 0;
    }
    if (*cursor == 0xfff09c169414613) {
        return cursor + 2;
    } else {
        return 0;
    }
}



uint64_t *isHeapPointer(uint64_t *value) {
    if (value < heapStart || value >= heapEnd) return 0;

    uint64_t *current = heapStart;

    while (current < heapStart && *current != 0xfff09c169414613) {
      uint64_t size = *current;
      if (value >= current && value < current + size) {
        return current;
      }
      current += size;
    }

    return 0;
}

uint64_t firstGlobal[100] = {0}; /* simulated global variables */
uint64_t *lastGlobal = firstGlobal+99;

uint64_t stackBottom[100] = {0}; /* simulated stack variables */
uint64_t *stackTop = stackBottom+99;

int stack_size = 1000; /* work stack for GC to use */

void mark() {
    uint64_t* stack[stack_size];
    uint64_t stackPointer = 0;
    uint64_t* tmp;
    uint64_t size;

    // temporary work pointer
    uint64_t* pointer;

    /* add root set to work stack */
    for (uint64_t* i = firstGlobal; i <= lastGlobal; i++) {
        if ((pointer = isHeapPointer((uint64_t*)*i))) {
            stack[stackPointer++] = pointer;
        }
    }
    for (uint64_t* i = stackBottom; i <= stackTop; i++) {
        if ((pointer = isHeapPointer((uint64_t*)*i))) {
            stack[stackPointer++] = pointer;
        }
    }

    printf("root set pushed, stackPointer == %" PRIx64 "\n", stackPointer);

    while (stackPointer > 0) {
        tmp = stack[--stackPointer];
        size = *(tmp - 1);
        if ((size & 0x8000000000000000) == 0) {
            /* if mark bit is clear */
            /* add pointer fields to work stack */
            for (uint64_t* i = tmp; i < tmp + size - 2; i++) {
                if ((pointer = isHeapPointer((uint64_t*)*i))) {
                    stack[stackPointer++] = pointer;
                }
            }
            *(tmp - 1) |= 0x8000000000000000; /* and set mark bit */
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

  for (int i = 0; i<60; i++) {
    if ((cc = allocate((3*i)%11+3)) != 0) {
      printf("allocation successful: %ld\n", cc - heapStart);
      cc[0] = 0x1001*i;
      if (i%13 == 0) cc[1] =  firstGlobal[i%17]; else cc[1] = 0;
      ll = cc;
      firstGlobal[i%17] = (uint64_t) (ll+i%3);
    } else {
      printf("Allocation failed at i == %d, size = %d\n", i, (3*i)%11+2);
      exit(0);
    }
  }
}

