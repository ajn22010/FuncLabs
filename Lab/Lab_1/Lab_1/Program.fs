// For more information see https://aka.ms/fsharp-console-apps
module Lab1

let list1 = 3 :: [4; 5; 6; 7]
let listLength = List.length list1
let list3 = [4; 5; 2; 7] @ 3 :: [1]

let rec lengthFunc list = 
    match list with
    | [] -> 0
    | head :: [] -> 1
    | head :: tail -> lengthFunc(tail) + 1

let add10 n = n+10

let add20 n = add10 (add10 n)

let add19 n = add10(add10 n-1)                   //add20 n-1 here n-1 evaluates. add10(add10 n-1) also evaluates... So why do we need parentheses in the recursive version?

let rec add_mn m n= 
    match n with
    | 0 -> m
    | _ ->  add10(add_mn m (n-1) )  //Idk, evaluation is confusing? add10( add_mn m n-1 ) causes segfault in the recursion


let vAdd (x1,y1) (x2,y2) : float * float = (x1+x2,y1+y2)

let rec vSum list =
    match list with
    | [] -> failwith "bruh"
    | head :: [] -> head
    | head :: tail -> vAdd head (vSum tail)
