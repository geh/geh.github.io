#!/usr/bin/env python

"""
Implementation of the n-puzzle problem.

A state is a tuple (board,[action]), whose second
element is the list of actions to reach it from the
initial state.
"""

from copy import deepcopy

def print_board(board):
    for line in board: print line

def is_solution(board_and_path):
    return board_and_path[0] == [[1,2,3],[4,5,6],[7,8,0]]

def move_down(board):
    new_board = deepcopy(board)
    size = len(board)
    for l in range(size):
        for c in range(size):
            if new_board[l][c] == 0:
                new_board[l][c] = new_board[l-1][c]
                new_board[l-1][c] = 0
                return new_board

def move_up(board):
    new_board = deepcopy(board)
    size = len(board)
    for l in range(size):
        for c in range(size):
            if new_board[l][c] == 0:
                new_board[l][c] = new_board[l+1][c]
                new_board[l+1][c] = 0
                return new_board

def move_left(board):
    new_board = deepcopy(board)
    size = len(board)
    for l in range(size):
        for c in range(size):
            if new_board[l][c] == 0:
                new_board[l][c] = new_board[l][c+1]
                new_board[l][c+1] = 0
                return new_board

def move_right(board):
    new_board = deepcopy(board)
    size = len(board)
    for l in range(size):
        for c in range(size):
            if new_board[l][c] == 0:
                new_board[l][c] = new_board[l][c-1]
                new_board[l][c-1] = 0
                return new_board

def possible_actions(board):
    size = len(board)
    actions = []
    for l in range(size):
        for c in range(size):
            if board[l][c] == 0: # hole
                if l>0:        actions.append((move_down,"down"))
                if l<(size-1): actions.append((move_up,"up"))
                if c>0:        actions.append((move_right,"right"))
                if c<(size-1): actions.append((move_left,"left"))
    return actions

def successors(board_and_path):
    board = board_and_path[0]
    path  = board_and_path[1]
    return [ (action(board), path + [name]) for (action, name) in possible_actions(board) ]

def remove_repeats(to_nub, forbidden1, forbidden2):
    return [ item for item in to_nub if item not in (forbidden1 + forbidden2) ] 

def search(board):
    current = (board,[])    # first state
    open_states = [current] # states to be explored
    closed_states = []      # states already explored
    counter = 0
    while True:
        # show progress
        counter = counter + 1
        if counter % 100 == 0: print "explored states: " , counter
        # expansion
        neighbor_boards = successors(current)
        unseen_boards = remove_repeats(neighbor_boards, closed_states, open_states)
        open_states = open_states + unseen_boards
        # selection
        assert(False) ## TODO choose next element 'current' to explore and remove it from open_states
        closed_states.append(current)
        # stop condition: solution found or state space exhausted
        if (is_solution(current)) or (len(open_states) == 0): break
    if is_solution(current):
        print "solution:", current[1]
    else:
        print "haven't found any solution!"

if __name__ == "__main__":
    b = input("Enter board (or 0 for easy, 1 for hard).\n")
    if b==0:
        b = [[2,3,0],[1,5,6],[4,7,8]]
    elif b==1:
        b = [[1,2,3],[4,5,6],[8,7,0]]
    print "Starting with:"
    print print_board(b)
    search(b)
