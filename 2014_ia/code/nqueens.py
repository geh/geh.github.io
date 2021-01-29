#!/usr/bin/env python

import datetime
from pylab import *

def first_board(size):
    """a board is a list of lists of integers
       0 represents an empty cell
       1 represents a queen
    """
    first_line = [[1] * size]
    rest = [ [0] * size for _ in range(size-1) ]
    return first_line + rest

def print_board(board):
    for line in board:
        print line

def next_board(board,column=0):
    """move one step downwards the queen located on given column.
       if queen is on last line, move it to first line and move
       next column's queen.
       if queen was also on the last column, reset the whole board.
    """
    pass

def is_solution(board):
    # test 1: horizontal
    # test 2: vertical
    # test 3: diagonal north-west-to-south-east
    # test 4: diagonal north-east-to-south-west
    return True

def ask_size_and_run():
    size = input("Enter board size.\n")
    b = first_board(size)
    while (not is_solution(b)):
        next_board(b)
        if b == first_board(size):
            print "No solution found."
            break
    else:
        # the `else` clause is only executed when the `while`
        # condition becomes false
        print "We found a solution:"
        print_board(b)

def benchmark(sizes):
    """given a list of board sizes, measure the time required
       to find the first solution (or to try all possible board)
    """
    pass
