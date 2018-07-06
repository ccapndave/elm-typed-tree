module TreePath exposing
  ( TreePath
  --, data
  )

{-|

@docs TreePath
-}

import Array exposing (Array)


type alias Tree b l
  = Node b l


type Node b l
  = Branch b (Array Node)
  | Leaf l


{-|
-}
type TreePath b l =
  TreePath
    { tree : Tree b l
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


branchData : TreePath b l -> Maybe b -- this is going to be annoying
branchData treePath =
  getFocusedNode treePath
    |> \node ->
        case node of
          Branch data _ ->
            Just data

          Leaf data ->
            Nothing


leafData : TreePath b l -> Maybe l -- this is going to be annoying
leafData treePath =
  getFocusedNode treePath
    |> \node ->
        case node of
          Branch _ _ ->
            Nothing

          Leaf data ->
            Just data


--- is this better?
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
--- --- --- ---


-- PRIVATE HELPERS

{-|
-}
getFocusedNode : TreePath b l -> Node b l
getFocusedNode (TreePath { tree, path }) =
  let
    traverseTree : Int -> Tree b l -> Tree b l
    traverseTree pathIndex tree =
      case tree of
        Branch _ children ->
          Array.get tree pathIndex -- if this returns Nothing then the path is invalid, too

        Leaf _ ->
          Debug.crash "The path is somehow invalid..."
  in
  path
    |> List.foldl traverseTree tree
