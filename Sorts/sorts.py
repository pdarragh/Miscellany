#!/usr/bin/env python
import random

def is_sorted(values):
    for index in xrange(1, len(values)):
        if values[index] < values[index - 1]:
            return False
    return True

def random_sort(values, debug=False):
    A = list(values) # make a copy of the list first
    comparisons = 0
    while not is_sorted(A):
        comparisons += 1
        _compute_random_sort(A)
        if comparisons % 1000 == 0:
            print("comparisons: {}".format(comparisons))
    if debug:
        return (A, comparisons, None)
    else:
        return A

def _compute_random_sort(values):
    a_index = random.randint(0, len(values) - 2)
    b_index = random.randint(a_index + 1, len(values) - 1)
    if values[a_index] > values[b_index]:
        temp = values[b_index]
        values[b_index] = values[a_index]
        values[a_index] = temp

def progressive_random_sort(values, debug=False):
    A = list(values) # make a copy of the list first
    comparisons = 0
    for index in xrange(len(A) - 1):
        comparisons += 1
        rand_index = random.randint(index + 1, len(A) - 1)
        if A[index] > A[rand_index]:
            temp = A[index]
            A[index] = A[rand_index]
            A[rand_index] = temp
    return (A, comparisons)

def insertion_sort(values, debug=False):
    A = list(values) # make a copy of the list first
    comparisons = 0
    for i in xrange(1, len(A)):
        a = A[i]
        j = i - 1
        while j >= 0 and A[j] > a:
            comparisons += 1
            A[j+1] = A[j]
            j = j - 1
        A[j+1] = a
    if debug:
        return (A, comparisons, None)
    else:
        return A
