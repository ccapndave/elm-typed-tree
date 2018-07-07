module TreePath exposing
  ( TreePath
  , Data(..)
  , decoder
  , treeDecoder
  , data
  , top
  , up
  , right
  , down
  )

{-|

@docs TreePath
@docs Data
@docs decoder
@docs treeDecoder
@docs data
@docs top
@docs up
@docs right
@docs down
-}

import Json.Decode as JD exposing (Decoder, field)
import Json.Encode as JE exposing (Value)
import Array exposing (Array)


{-|
-}
type Data b l
  = BranchData b
  | LeafData l


type Node b l
  = Branch b (Array (Node b l))
  | Leaf l


{-|
-}
type TreePath b l =
  TreePath
    { tree : Node b l
    , path : Array Int
    }


{-|
-}
mkTree : b -> TreePath b l
mkTree data =
  TreePath
    { tree = Branch data Array.empty
    , path = Array.empty
    }


{-|
-}
decoder : Decoder b -> Decoder l -> Decoder (TreePath b l)
decoder branchDataDecoder leafDataDecoder =
  JD.map2 (\tree path -> TreePath { tree = tree, path = path })
    (field "tree" <| nodeDecoder branchDataDecoder leafDataDecoder)
    (field "path" <| JD.array JD.int)


{-|
-}
treeDecoder : Decoder b -> Decoder l -> Decoder (TreePath b l)
treeDecoder branchDataDecoder leafDataDecoder =
  JD.map (\tree -> TreePath { tree = tree, path = Array.empty })
    (nodeDecoder branchDataDecoder leafDataDecoder)


nodeDecoder : Decoder b -> Decoder l -> Decoder (Node b l)
nodeDecoder branchDataDecoder leafDataDecoder =
  JD.maybe (JD.field "children" (JD.array <| JD.lazy (\_ -> nodeDecoder branchDataDecoder leafDataDecoder)))
    |> JD.andThen (\maybeChildren ->
      case maybeChildren of
        Just children ->
          branchDataDecoder
            |> JD.map (\data -> Branch data children)

        Nothing ->
          leafDataDecoder
            |> JD.map (\data -> Leaf data)
    )


{-|
-}
encode : (b -> Value) -> (l -> Value) -> TreePath b l -> Value
encode branchEncode leafEncode treePath =
  JE.null


{-|
-}
data : TreePath b l -> Data b l
data treePath =
  getFocusedNode treePath
    |> \node ->
        case node of
          Branch data _ ->
            BranchData data

          Leaf data ->
            LeafData data


{-|
-}
top : TreePath b l -> TreePath b l
top (TreePath { tree, path } as treePath) =
  TreePath
    { tree = tree
    , path = Array.empty
    }


{-|
-}
up : TreePath b l -> Maybe (TreePath b l)
up (TreePath { tree, path } as treePath) =
  if Array.isEmpty path then
    Nothing
  else
    Just <|
      TreePath
        { tree = tree
        , path = Array.slice 0 -1 path
        }


{-|
-}
right : TreePath b l -> Maybe (TreePath b l)
right (TreePath { tree, path } as treePath) =
  case Array.get (Array.length path - 1) path |> Debug.log "currentIdx" of
    Just currentIdx ->
      treePath
        |> up
        |> Maybe.andThen (down (currentIdx + 1))

    Nothing ->
      Nothing


{-|
-}
down : Int -> TreePath b l -> Maybe (TreePath b l)
down idx (TreePath { tree, path } as treePath) =
  getFocusedNode treePath
    |> \node ->
      case node of
        Branch _ children ->
          Array.get idx children
            |> Maybe.map (\_ ->
              TreePath
                { tree = tree
                , path = Array.push idx path
                }
            )

        Leaf _ ->
          Nothing


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
    |> Array.foldl traverseTree tree
