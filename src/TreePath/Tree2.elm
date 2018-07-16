module TreePath.Tree2
    exposing
        ( decoder
        , toRootPath
        , Tree
        , TreePath2
        , data2
        , top2
        , up2
        , offset2
        , down2
        , downs2
        , TreePath1
        , data1
        , top1
        , up1
        , offset1
        , down1
        , downs1
        )

import TreePath.Data as Data exposing (Data)
import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)


type alias Tree a leaf =
    Tree2 a leaf


type Tree1 leaf
    = Tree1
        { data : leaf
        }


type Tree2 a leaf
    = Tree2
        { data : Data a leaf
        , children : Array (Tree1 leaf)
        }


type TreePath1 a leaf
    = TreePath1
        { tree : Tree2 a leaf
        , path : Array Int
        }


type TreePath2 a leaf
    = TreePath2
        { tree : Tree2 a leaf
        , path : Array Int
        }


type alias DecoderConfig a leaf =
    { level2Decoder : Decoder a
    , level2ChildrenField : String
    , leafDecoder : Decoder leaf
    }


decoder : DecoderConfig a leaf -> Decoder (Tree2 a leaf)
decoder config =
    decoder2
        ( config.level2Decoder, config.level2ChildrenField )
        config.leafDecoder


toRootPath : Tree4 a b c leaf -> TreePath4 a b c leaf
toRootPath tree =
    TreePath4
        { tree = tree
        , path = Array.empty
        }


decoder1 : Decoder leaf -> Decoder (Tree1 leaf)
decoder1 leafDecoder =
    leafDecoder
        |> JD.map (\data -> Tree1 { data = data })


getFocusedTree1 : TreePath1 a leaf -> Tree1 leaf
getFocusedTree1 (TreePath1 { tree, path }) =
    getFocusedTree2 (TreePath2 { tree = tree, path = path })
        |> treeChildren2
        |> Array.get (Array.get 0 path |> unsafe "getFocusedTree1")
        |> unsafe "getFocusedTree1"


treeData1 : Tree1 leaf -> leaf
treeData1 (Tree1 { data }) =
    data


data1 : TreePath1 a leaf -> leaf
data1 =
    getFocusedTree1 >> treeData1


top1 : TreePath1 a leaf -> TreePath2 a leaf
top1 (TreePath1 { tree, path }) =
    TreePath2 { tree = tree, path = Array.empty }


offset1 : Int -> TreePath1 a leaf -> Maybe (TreePath1 a leaf)
offset1 dx ((TreePath1 { tree, path }) as treePath) =
    treePath
        |> up1
        |> Maybe.andThen (down2 <| (Array.get 0 path |> unsafe "offset1") + dx)


down1 : Int -> TreePath1 a leaf -> Maybe Never
down1 idx ((TreePath1 { tree, path }) as treePath) =
    Nothing


downs1 : TreePath1 a leaf -> List Never
downs1 ((TreePath1 { tree, path }) as treePath) =
    []


up1 : TreePath1 a leaf -> Maybe (TreePath2 a leaf)
up1 (TreePath1 { tree, path }) =
    Just <| TreePath2 { tree = tree, path = Array.slice 0 -1 path }


decoder2 : ( Decoder a, String ) -> Decoder leaf -> Decoder (Tree2 a leaf)
decoder2 ( aDecoder, aChildrenField ) leafDecoder =
    JD.oneOf
        [ JD.map2 (\data children -> Tree2 { data = data, children = children })
            (aDecoder |> JD.map Data.BranchData)
            (JD.field aChildrenField (JD.array <| decoder1 leafDecoder))
        , JD.map (\data -> Tree2 { data = data, children = Array.empty })
            (leafDecoder |> JD.map Data.LeafData)
        ]


getFocusedTree2 : TreePath2 a leaf -> Tree2 a leaf
getFocusedTree2 (TreePath2 { tree, path }) =
    tree


treeChildren2 : Tree2 a leaf -> Array (Tree1 leaf)
treeChildren2 (Tree2 { children }) =
    children


treeData2 : Tree2 a leaf -> Data a leaf
treeData2 (Tree2 { data }) =
    data


data2 : TreePath2 a leaf -> Data a leaf
data2 =
    getFocusedTree2 >> treeData2


top2 : TreePath2 a leaf -> TreePath2 a leaf
top2 (TreePath2 { tree, path }) =
    TreePath2 { tree = tree, path = Array.empty }


offset2 : Int -> TreePath2 a leaf -> Maybe (TreePath2 a leaf)
offset2 dx ((TreePath2 { tree, path }) as treePath) =
    Nothing


down2 : Int -> TreePath2 a leaf -> Maybe (TreePath1 a leaf)
down2 idx ((TreePath2 { tree, path }) as treePath) =
    getFocusedTree2 treePath
        |> treeChildren2
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath1 { tree = tree, path = Array.push idx path })


downs2 : TreePath2 a leaf -> List (TreePath1 a leaf)
downs2 ((TreePath2 { tree, path }) as treePath) =
    getFocusedTree2 treePath
        |> treeChildren2
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath1 { tree = tree, path = Array.push idx path })


up2 : TreePath2 a leaf -> Maybe Never
up2 (TreePath2 { tree, path }) =
    Nothing


unsafe : String -> Maybe a -> a
unsafe msg maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.crash msg
