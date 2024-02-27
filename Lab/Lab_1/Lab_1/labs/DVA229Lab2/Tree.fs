namespace Lab2

[<AutoOpen>]
module Tree =



// Declare a type for binary trees of integers.
//
// The tree can be one of three things:
//   * An empty tree.
//   * A leaf containing an integer.
//   * A root node containing an integer and two children.

// type Tree....

    type 'a Tree = 
        | Empty
        | Leaf of 'a
        | Node of 'a Tree * 'a *  'a Tree


    // val empty : Tree
    // A value defined to be an empty tree.


    let empty  =  Empty // failwith "Error @ left: Not implemented" //huh????


    // val isEmpty : Tree -> bool
    // Given a tree t, it returns true if t is empty and false if t is not empty.


    // let isEmpty...
    let isEmpty tree = 
        match tree with
        | Empty -> true
        | _ -> false

    // val leaf : int -> Tree
    // Given an integer, it returns a leaf containing this integer.



    // let leaf...
    let leaf value = 
        Leaf(value)
   

    // val isLeaf : Tree -> bool
    // Given a tree t, it returns true if t is a leaf and false it is not.



    // let isLeaf...
    let isLeaf tree =
        match tree with
        | Leaf(_) -> true
        | _ -> false


    // val root : Tree -> int -> Tree -> Tree
    // The function takes as input a tree t1, an integer x and and a tree t2.
    // The function returns a root node with t1 as the left child, x as the
    // element and t2 as the right child.



    // let root...
    let root left value right = 
        Node(left, value, right)


    // val head : Tree -> int
    // Given a tree t, it returns the integer stored in t if t is a root node or
    // a leaf. However, if t is empty, the function throws an exception.



    // let head...
    let head input = 
        match input with 
        | Node(left, value, right) -> value
        | Leaf(value) -> value
        | Empty ->  failwith "Empty"


    // val left : Tree -> Tree
    // Given a tree t, it returns the left child of t if t is a root node.
    // However, if t is a leaf or a an empty tree, the function throws
    // an exception.



    // let left...
    let left tree = 
        match tree with
        | Node(left, value, right) -> left
        | _ -> failwith "Not a root"


    // val right : Tree -> Tree
    // Given a tree t, it returns the right child of t if t is a root node.
    // However, if t is a leaf or a an empty tree, the function throws
    // an exception.



    // let right...
    let right tree =
        match tree with
        | Node(left, value, right) -> right
        | _ -> failwith "Not a root"
