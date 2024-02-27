namespace Lab2



module List =



// When implemented correctly, all functions in this module will be
// polymorphic. For example, mem will be given the following type:
//
//   val mem : 'a -> 'a list -> bool (where 'a has equality).
//
// The type annotations and descriptions that accompany each function are
// there to guide you in your work - you are not meant to constrain the
// functions to be defined for integers only.
//
// Do not worry! You will not have to do any extra work to make the functions
// polymorphic. F# uses a Hindley-Milner type system, where polymorphism
// is the default.


// val mem : int -> int list -> bool
// Given an integer x and a list of integers xs, it returns true if
// xs contains x and false if it does not.
//
// Note:
//   * You cannot use builtin list functions.



//let rec mem x xs = failwith "Error @ mem: Not implemented!"
//     let rec mem x xs = 

    let rec mem x xs = 
        match xs with
        | [] -> false
        | head :: tail -> if (head = x) then true else mem x tail 




    // val union : int list -> int list -> int list
    // Given two lists, it returns their union.
    // The output list cannot contain duplicates.
    //
    // Note:
    //   * You cannot use builtin list functions.



    // let rec union xs ys = //failwith "Error @ union: Not implemented!"
    let rec union xs ys = 
        match (xs @ ys) with 
        | [] -> [] 
        | head :: tail -> if (mem head tail) = true then (union tail []) else head :: (union tail []) 




    // val foo1 : int -> int list
    // The function takes as input an integer x and an integer list xs.
    // The function returns an integer list containing n repetitions of the
    // value x, where n is the number of times that x occurs in xs.
    //
    // Examples:
    //  foo1 1 [2;1;2;1;4;1] => [1;1;1]
    //  foo1 4 [2;1;2;1;4;1] => [4]
    //
    // Note:
    //   * You cannot use builtin list functions.



    //  let rec foo1 x xs = failwith "Error @ foo1: Not implemented!"

    let rec foo1 x xs = 
        match xs with
        | [] -> []
        | head :: tail -> if head=x then head :: (foo1 x tail) else  (foo1 x tail)


    // val foo2 : (int -> int -> bool) -> int -> int list
    // The function takes as input a predicate function f, an integer x1
    // and an integer list xs. The function returns an integer list ys
    // containing all elements x2 in xs for which f x1 x2 is true. The elements
    // in ys are ordered according to their order of appearance in xs.
    //
    // Examples:
    //  let eq x y = x = y
    //  let lt x y = x < y
    //  let gt x y = x > y
    //
    //  foo2 eq 2 [1;2;4;2;5] => [2;2]
    //  foo2 lt 2 [1;2;4;2;5] => [4;5]
    //  foo2 gt 2 [1;2;4;2;5] => [1]
    //
    // Note:
    //   * You cannot use builtin list functions.



    // let rec foo2 f x xs = failwith "Error @ foo2: Not implemented!"

    let eq a b = (a = b)
    let lt a b = (a < b)
    let gt a b = (a > b)
    let rec foo2 arg stupid derp  = 
        match derp with
        | [] -> []
        | head :: tail -> if (arg stupid head) then head :: (foo2 arg stupid tail ) else  (foo2 arg stupid tail )

    // Question:
    //   Look at these function calls:
    //     foo2 eq 2 [1;2;4;2;5]
    //     foo2 lt 2 [1;2;4;2;5]
    //     foo2 gt 2 [1;2;4;2;5]
    //   Is there a way to avoid defining the functions eq, lt, and gt, 
    //   and instead use the operators =, <, and > directly in the calls to foo2? 
    //   (Hint: F# has a way to turn infix operators into functions.)
