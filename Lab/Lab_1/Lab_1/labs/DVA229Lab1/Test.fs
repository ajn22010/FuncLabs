[<RequireQualifiedAccess>]
module Test



open FsCheck



let equalBy f x y = f x = f y



// Why do we need this function?
// Right now, I do not have an answer.
// Feel free to investigate this mystery!
let equalWhenStrings x y = equalBy (sprintf "%A") x y



// backticks allow for spaces in names
type ``Lab1 functions`` =

  static member Add_mn m (NonNegativeInt n) =
    try
      if n < 0
        then m          = Lab1.add_mn m n
        else m + 10 * n = Lab1.add_mn m n

    with
    | _ -> false



  static member VSum (xs : (float * float) list) =
    let verify result =
      // Floating point addition is not associative in the real world!
      // That's why we need to compare result to both reduce and reduceBack.
      equalWhenStrings (List.reduce     Lab1.vAdd xs) result ||
      equalWhenStrings (List.reduceBack Lab1.vAdd xs) result

    try
      let result = Lab1.vSum xs

      match xs with
      | [] -> false
      | _  -> verify result

    with
    | _ -> List.isEmpty xs



let add_mn () = Check.Quick ``Lab1 functions``.Add_mn



let vSum () = Check.Quick ``Lab1 functions``.VSum



let all () = Check.QuickAll<``Lab1 functions``>()
