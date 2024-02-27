namespace Lab2



[<RequireQualifiedAccess>]
module Test =



  open FsCheck



  let catchWithFalse (f : Lazy<bool>) =
    try f.Force () with | _ -> false
  
  let trueIfException f = 
    try
      f () |> ignore
      false
    with | _ -> true
  
  let allAreTrue conds =
    let _and a b = a && b
    List.reduce _and conds


  // backticks allow for spaces in names
  type ``Lab2 list functions`` =
    static member Mem (x : int) (xs : int list) =
      lazy (List.contains x xs = Lab2.List.mem x xs)
      |> catchWithFalse



    static member Union (xs : int list) (ys : int list) =
      try
        let result =
          Lab2.List.union xs ys
          |> List.sort

        Set.union (Set.ofList xs) (Set.ofList ys)
        |> Set.toList
        |> List.sort
        |> (=) result

      with
      | _ -> false



    static member Foo1 (x : int) (xs : int list) =
      try
        let n = List.sumBy (fun y -> if x = y then 1 else 0) xs

        Seq.replicate n x
        |> Seq.toList
        |> (=) (Lab2.List.foo1 x xs)

      with
      | _ -> false



    static member Foo2 (x : int) (xs : int list) =
      try
        let eq  a b = a = b
        let lt  a b = a < b
        let gt  a b = a > b
        let neq a b = a = b

        Lab2.List.foo2 eq  x xs = List.filter (eq  x) xs &&
        Lab2.List.foo2 lt  x xs = List.filter (lt  x) xs &&
        Lab2.List.foo2 gt  x xs = List.filter (gt  x) xs &&
        Lab2.List.foo2 neq x xs = List.filter (neq x) xs

      with
      | _ -> false



  module List =
    let mem () = Check.Quick ``Lab2 list functions``.Mem



    let union () = Check.Quick ``Lab2 list functions``.Union



    let foo1 () = Check.Quick ``Lab2 list functions``.Foo1



    let foo2 () = Check.Quick ``Lab2 list functions``.Foo2



    let all () = Check.QuickAll<``Lab2 list functions``>()
  
  type ``Lab2 empty tree functions`` =
    static member ``Given an empty tree, then tree is not a leaf `` () =
        match (isEmpty (Tree.empty), isLeaf (Tree.empty)) with
        | (true, false) -> true
        | _ -> false

    static member ``Given an empty tree, when head is called, then error shall be raised `` () = trueIfException (fun () -> Tree.head (Tree.empty))

    static member ``Given an empty tree, when left is called, then error shall be raised `` () = trueIfException (fun () -> Tree.left (Tree.empty))

    static member ``Given an empty tree, when right is called, then error shall be raised `` () = trueIfException (fun () -> Tree.right (Tree.empty))

  type ``Lab2 leaf error handling`` = 
    static member ``Given a leaf, when left is called, then error should be raised `` (e : int) =
      trueIfException (fun () -> Tree.left (Tree.leaf e))

    static member ``Given a leaf, when right is called, then error should be raised `` (e : int) =
      trueIfException (fun () -> Tree.right (Tree.leaf e))

    

  type ``Lab2 Tree functional tests`` =
    (*  The following properties check that the different types are disjoint, 
        i.e., that a leaf is not empty and a root is neither empty nor a leaf *)
    

    static member ``Given an leaf, then tree is not empty `` (e : int) =
        let l = leaf e
        match (isEmpty l, isLeaf l) with
        | (false, true) -> true
        | _ -> false

    static member ``Given a root, then tree is neither empty nor a leaf `` (e1 : int) (e2 : int) (e3 : int) =
      let emptyRoot = Tree.root Tree.empty e1 Tree.empty
      let testRoot = Tree.root (Tree.leaf e1) e2 (Tree.leaf e3)
      [
          testRoot |> Tree.isEmpty = false ;
          testRoot |> Tree.isLeaf = false ;
          emptyRoot |> Tree.isEmpty = false ;
          emptyRoot |> Tree.isLeaf = false ;
        ] |> allAreTrue

    static member ``Given a non-empty tree when head is called, the value is retrieved `` (e : int) =
      let testLeaf = Tree.leaf e
      [
        testLeaf 
          |> Tree.head = e ;
        (Tree.root (Tree.leaf (e / 2)) e (Tree.leaf (e / 4))) 
          |> Tree.head = e ;
      ] |> allAreTrue

    static member ``Given a root when left is called, the left subtree is retrieved `` (e : int) =
      let expected = Tree.leaf e
      let testRoot = Tree.root (Tree.leaf e) e (Tree.empty)
      testRoot 
      |> Tree.left = expected

    static member ``Given a root when right is called, the right subtree is retrieved `` (e : int) =
      let expected = Tree.leaf e
      let testRoot = Tree.root (Tree.empty) e (Tree.leaf e)
      testRoot 
      |> Tree.right = expected

  module Tree =    
    let all () = Check.QuickAll<``Lab2 Tree functional tests``>()
  
  
  type ``Lab2 BST functional tests`` =
    static member ``Given a sorted tree t with head n, when value e is inserted and e < n, then e is found in the left subtree of t `` (b: int) (a : NonEmptyArray<int>) =
      let l = a.Get |> List.ofArray
      (b < List.reduce min l) ==> 
      ((BST.ofList l) |> (BST.insert b) |> Tree.left |> BST.toList |> (List.contains b))

    static member ``Given a sorted tree t with head n, when value e is inserted and e >= n, then e is found in the right subtree of t `` (b: int) (a : NonEmptyArray<int>) =
      let l = a.Get |> List.ofArray
      (b >= List.reduce max l) ==> 
      ((BST.ofList l) |> (BST.insert b) |> Tree.right |> BST.toList |> (List.contains b))


    static member SortList (xs : int list) =
      lazy (BST.sortList xs = List.sort xs)
      |> catchWithFalse
    



  module BST =
    let sortList () = Check.Quick ``Lab2 BST functional tests``.SortList



    let all () = Check.QuickAll<``Lab2 BST functional tests``>()



  let all () =
    Check.QuickAll<``Lab2 list functions``>()
    let singularTestConfig = {Config.Quick with MaxTest = 1}
    Check.All<``Lab2 empty tree functions``> (singularTestConfig)
    Check.QuickAll<``Lab2 leaf error handling``>()
    Check.QuickAll<``Lab2 Tree functional tests``>()
    Check.QuickAll<``Lab2 BST functional tests``>()