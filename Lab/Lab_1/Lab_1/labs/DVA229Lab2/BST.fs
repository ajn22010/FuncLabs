namespace Lab2


[<AutoOpen>]
module BST =
 open Tree


  // You should not declare a new type for BSTs!
  // Instead, make use of the interface functions you defined in Tree.fs.



  // val insert : int -> Tree -> Tree
  // Given an integer x and a BST t1, it returns a BST t2 containing all
  // elements in t1 as well as x. x is inserted, even if x is already in t1.
  // let rec insert...  
    let rec insert value tree = //failwith "Error @ insert: Not implemented"
        match tree with
        | Empty -> Leaf(value)
        | Leaf(leafVal) -> 
            if value>=leafVal then Node(Empty, leafVal, Leaf(value))
            else Node(Leaf(value), leafVal, Empty)
        | Node(left, nodeVal, right) -> 
            if value>=nodeVal then  Node(left, nodeVal, (insert value right))
            else Node((insert value left), nodeVal, right)    

  // val insertList : int list -> Tree -> Tree
  // Given an integer list xs and a BST t1, it returns a BST t2 containing
  // all elements in t1 as well as all x in xs. All x in xs are inserted,
  // even if they are already in t1.
  //
  // Note:
  //   * Define insertList using insert.



  // let rec insertList...
  let rec insertList list tree = //failwith "Error @ insertList: Not implemented"
        match list with
        | head::tail -> (insert head (insertList tail tree))
        | _ -> tree


  // val ofList : int list -> Tree
  // Given an integer list xs, it returns a BST containing all x in xs.
  //
  // Note:
  //   * Define ofList using insertList.
  


  // let ofList...
  let ofList list = insertList list Empty //failwith "Error @ ofList: Not implemented"


  // val toList : Tree -> int list
  // Given a tree t, it returns a list of integers xs.
  // xs contains all elements in t, sorted in ascending order.
  //
  // Hint:
  //   * Define toList as an inorder traversal of t.
  //
  // Note:
  //   * You cannot use any builtin sorting functions.



  // let rec toList...
    let rec toList tree = //failwith "Error @ toList: Not implemented"
        match tree with
        | Leaf(value) -> value :: []
        | Node(left, value, right) ->  (toList left) @ (value :: (toList right))
        | Empty -> []



  // val sortList : int list -> int list
  // Given an integer list xs, it returns an integer list ys.
  // ys contains all elements in xs, sorted in ascending order.
  //
  // Note:
  //   * You cannot use any builtin sorting functions.


   let sortList xs = toList(ofList xs) //failwith "Error @ sortList: Not implemented!"

       