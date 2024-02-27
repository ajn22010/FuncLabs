[<AutoOpen>]
module Lab1



// val add10 : int -> int
// Given an integer m, it returns m + 10.



// let add10...



// val add20 : int -> int
// Given an integer m, it returns m + 20.
//
// Note:
//   * Define add20 using add10.
//   * You cannot use arithmetic operators.



// let add20...



// val add_mn : int -> int -> int
// Given one integer m and one non-negative integer n, it returns m + 10 * n.
//
// Note:
//   * Define add_mn using add10.
//   * You cannot use arithmetic operators to directly compute the return
//     value.


//
//Part 1. 
//Different operator for appending? And had to place [] around it, I guess appending a list to another list, or rather combining two lists, while :: inserts a new element in the list.
//

let list1 = 3 :: [4;5;2;7]
let list2Length = List.length [4;5;2;7]
let list3 = [4; 5; 2; 7] @ [3]
//
//Part 2.
// a b c = 5 15 20. "in" is a local scope. So declares a new scope, declares it to 10 and adds 5 to it.
//
let x = 42
let myName = "Kalle"
let age = 25
let y = 4 + 2

//
// Part 3
//
let add10 x = 
    x + 10

let add20 x =
     add10 (add10 x)

let rec add_mn m n =
    if n < 1 then m
    elif n = 1 then (add10(m))
    else add10((add_mn m (n-1)))

//
// Part 4
//

let vAdd (x1, y1) (x2, y2) : float * float = (x1 + x2, y1 + y2)

let rec vSum list =
    match list with
    | [] ->  failwith "Error @ vSum: Not implemented!"
    | head :: [] -> head
    | head::tail -> vAdd(head) (vSum tail) 