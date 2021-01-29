% Data Structures Exam
% 2017-07-20


# 1. Structures and Pointers

 1. Consider the declarations:

    ~~~C
    typedef struct {
      int blah;
    } Box, *BoxPtr;

    Box b;
    BoxPtr bp;
    ~~~

    Which ones of the following expressions are well-formed?

    a. `b.int`
    b. `b.blah`
    c. `bp -> int`
    d. `bp -> blah`

 2. Using the same declarations,
    which one of the following statements is correct?

    a. `bp = malloc(sizeof(BoxPtr));`
    b. `b = malloc(sizeOf(Box));`
    c. `bp = malloc(sizeof(Box));`
    d. `b = malloc(sizeOf(BoxPtr));`

 3. Using the same declarations,
    which one of the following statements is correct?

    a. `free(bp);`
    b. `bp = free(sizeof(BoxPtr));`
    c. `b = free(bp);`

# 2. Linked Lists

 4. Consider the declaration of the Node type of a linked list:

    ~~~C
    typedef struct node {
      int num;
      struct node *next;
    } Node, *NodePtr;
    ~~~

    How is the end of a Linked List represented in a Node?

    a. The `num` fields is set to 0.
    b. The `next` field is set to NULL.
    c. It cannot represent this information.

 5. How does a node store the information that it is the head
    of a Linked List?

    a. Inside of its `num` field.
    b. Inside of its `next` field.
    c. It cannot store this information.

 6. Which one of these two data structures takes more space in memory
    to represent a list of 1000 elements?

    a. An array.
    b. A linked list. 

# 3. Stacks and Queue

 7. Which one of the following can be implemented using an array:

    a. A stack but not a queue.
    b. A queue but not a stack.
    c. Both.
    d. None.

 8. Which one of the following can be implemented using a linked list:

    a. A stack but not a queue.
    b. A queue but not a stack.
    c. Both.
    d. None.

 9. Which function is not used to interact with a stack?

    a. empty
    b. length
    c. pop
    d. push

10. When two elements are stored in a stack or a queue:

    a. The order in which they have been added matters.
    b. It does not matter.

# 3. Binary Trees

11. The struct that defines a node in a binary tree must have at least:

    a. One pointer field and two data fields.
    b. One data field and two pointer fields.
    c. Two data fields and two pointer fields.

12. A node in a binary tree must always have two subtrees:

    a. Yes
    b. No

Consider the following tree:

![](bintree.bmp)

13. The output of its pre-order traversal is:

    a. 9 5 1 7 2 12 8 4 3 11
    b. 8 5 9 7 1 12 2 4 11 3 
    c. 9 1 2 12 7 5 3 11 4 8 


13. The output of its in-order traversal is:

    a. 9 5 1 7 2 12 8 4 3 11
    b. 8 5 9 7 1 12 2 4 11 3 
    c. 9 5 1 7 2 12 8 4 11 3


14. The output of its post-order traversal is:

    a. 8 5 9 7 1 12 2 4 11 3 
    b. 9 5 1 2 12 7 3 11 4 8 
    c. 9 1 2 12 7 5 3 11 4 8 

15. In a Binary Search Tree, data is sorted taking into account:

    a. The nodes being leaves or not.
    b. The nodes and their left and right subtrees.
    c. The depth of nodes from the root.

# 4. Graphs

17. Which ones of the following statements are true?

    a. A graph can have more vertices than edges.
    b. A graph can have more edges than vertices.
    c. A graph cannot be a tree.

18. Dijkstra's algorithm involves:

    a. A stack
    b. A priority queue 
    c. Weighted edges

19. In Bellman-Ford's algorithm, the way in which we scan edges is:

    a. From the source node to the most remote ones.
    b. We loop through all edges, no matter their location.

20. A (connected) undirected graph may have more than one spanning tree:

    a. True
    b. False

21. A (connected) undirected graph may have more than one minimum-cost spanning tree:

    a. True
    b. False

22. In Kruskai's algorithm, the way in which we scan edges is:

    a. From some source node to the most remote ones.
    b. We loop through all of them, from lowest weight to highest.
    c. We loop through all of them, from highest weight to lowest.

# 5. Hashing

23. A hashing function is a function:

    a. From an infinite set of values to a finite one.
    b. From a finite set of values to an infinite one.

24. Which properties a hashing functions to be used in a data
    structure should have?

    a. It should be fast to compute.
    b. It should be slow to compute.
    c. It should never have collisions.

25. A hash table is essentially:

    a. A linked list.
    b. A binary tree.
    c. An array. 

26. What may cause a hash table to behave slower?

    a. That it is too empty.
    b. That it has too many collisions.
    c. That it contains too many elements.
