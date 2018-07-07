module TreePath exposing
  ( TreePath
  --, data
  )

{-|

@docs TreePath
-}

import Array exposing (Array)


type Node b l
  = Branch b (Array (Node b l))
  | Leaf l


{-|
-}
type TreePath b l =
  TreePath
    { tree : Node b l
    , path : List Int
    }


{-|
-}
mkTree : b -> TreePath b l
mkTree data =
  TreePath
    { tree = Branch data Array.empty
    , path = []
    }


type Data b l
  = BranchData b
  | LeafData l


data : TreePath b l -> Data b l
data treePath =
  getFocusedNode treePath
    |> \node ->
        case node of
          Branch data _ ->
            BranchData data

          Leaf data ->
            LeafData data


goToRoot : TreePath b l -> TreePath b l
goToRoot (TreePath { tree }) =
  TreePath
    { tree = tree
    , path = []
    }


-- PRIVATE HELPERS

{-| This is pretty horrid...
-}
getFocusedNode : TreePath b l -> Node b l
getFocusedNode (TreePath { tree, path }) =
  let
    traverseTree : Int -> Node b l -> Node b l
    traverseTree pathIndex tree =
      case tree of
        Branch _ children ->
          case Array.get pathIndex children of
            Just node ->
              node

            Nothing ->
              Debug.crash "The path somehow has an invalid index in it..."

        Leaf _ ->
          Debug.crash "The path is somehow longer than the depth of the tree..."
  in
  path
    |> List.foldl traverseTree tree
