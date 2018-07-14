module TreePath.Tree3
    exposing
        ( decoder
        , toRootPath
        , Tree
        , TreePath3
        , data3
        , top3
        , up3
        , offset3
        , down3
        , TreePath2
        , data2
        , top2
        , up2
        , offset2
        , down2
        , TreePath1
        , data1
        , top1
        , up1
        , offset1
        , down1
        )

import TreePath.Data as Data exposing (Data)
import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)


type alias Tree a b leaf =
    Tree3 a b leaf


type Tree1 leaf
    = Tree1
        { data : leaf
        }


type Tree2 a leaf
    = Tree2
        { data : Data a leaf
        , children : Array (Tree1 leaf)
        }


type Tree3 a b leaf
    = Tree3
        { data : Data a leaf
        , children : Array (Tree2 b leaf)
        }


type TreePath1 a b leaf
    = TreePath1
        { tree : Tree3 a b leaf
        , path : Array Int
        }


type TreePath2 a b leaf
    = TreePath2
        { tree : Tree3 a b leaf
        , path : Array Int
        }


type TreePath3 a b leaf
    = TreePath3
        { tree : Tree3 a b leaf
        , path : Array Int
        }


type alias DecoderConfig a b leaf =
    { level3Decoder : Decoder a
    , level3ChildrenField : String
    , level2Decoder : Decoder b
    , level2ChildrenField : String
    , leafDecoder : Decoder leaf
    }


decoder : DecoderConfig a b leaf -> Decoder (Tree3 a b leaf)
decoder config =
    decoder3
        ( config.level3Decoder, config.level3ChildrenField )
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


getFocusedTree1 : TreePath1 a b leaf -> Tree1 leaf
getFocusedTree1 (TreePath1 { tree, path }) =
    getFocusedTree2 (TreePath2 { tree = tree, path = path })
        |> treeChildren2
        |> Array.get (Array.get 1 path |> unsafe "getFocusedTree1")
        |> unsafe "getFocusedTree1"


treeData1 : Tree1 leaf -> leaf
treeData1 (Tree1 { data }) =
    data


data1 : TreePath1 a b leaf -> leaf
data1 =
    getFocusedTree1 >> treeData1


top1 : TreePath1 a b leaf -> TreePath3 a b leaf
top1 (TreePath1 { tree, path }) =
    TreePath3 { tree = tree, path = Array.empty }


offset1 : Int -> TreePath1 a b leaf -> Maybe (TreePath1 a b leaf)
offset1 dx ((TreePath1 { tree, path }) as treePath) =
    treePath
        |> up1
        |> Maybe.andThen (down2 <| (Array.get 1 path |> unsafe "offset1") + dx)


down1 : Int -> TreePath1 a b leaf -> Maybe Never
down1 idx ((TreePath1 { tree, path }) as treePath) =
    Nothing


up1 : TreePath1 a b leaf -> Maybe (TreePath2 a b leaf)
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


getFocusedTree2 : TreePath2 a b leaf -> Tree2 b leaf
getFocusedTree2 (TreePath2 { tree, path }) =
    getFocusedTree3 (TreePath3 { tree = tree, path = path })
        |> treeChildren3
        |> Array.get (Array.get 0 path |> unsafe "getFocusedTree2")
        |> unsafe "getFocusedTree2"


treeChildren2 : Tree2 a leaf -> Array (Tree1 leaf)
treeChildren2 (Tree2 { children }) =
    children


treeData2 : Tree2 a leaf -> Data a leaf
treeData2 (Tree2 { data }) =
    data


data2 : TreePath2 a b leaf -> Data b leaf
data2 =
    getFocusedTree2 >> treeData2


top2 : TreePath2 a b leaf -> TreePath3 a b leaf
top2 (TreePath2 { tree, path }) =
    TreePath3 { tree = tree, path = Array.empty }


offset2 : Int -> TreePath2 a b leaf -> Maybe (TreePath2 a b leaf)
offset2 dx ((TreePath2 { tree, path }) as treePath) =
    treePath
        |> up2
        |> Maybe.andThen (down3 <| (Array.get 0 path |> unsafe "offset2") + dx)


down2 : Int -> TreePath2 a b leaf -> Maybe (TreePath1 a b leaf)
down2 idx ((TreePath2 { tree, path }) as treePath) =
    getFocusedTree2 treePath
        |> treeChildren2
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath1 { tree = tree, path = Array.push idx path })


up2 : TreePath2 a b leaf -> Maybe (TreePath3 a b leaf)
up2 (TreePath2 { tree, path }) =
    Just <| TreePath3 { tree = tree, path = Array.slice 0 -1 path }


decoder3 : ( Decoder a, String ) -> ( Decoder b, String ) -> Decoder leaf -> Decoder (Tree3 a b leaf)
decoder3 ( aDecoder, aChildrenField ) ( bDecoder, bChildrenField ) leafDecoder =
    JD.oneOf
        [ JD.map2 (\data children -> Tree3 { data = data, children = children })
            (aDecoder |> JD.map Data.BranchData)
            (JD.field aChildrenField (JD.array <| decoder2 ( bDecoder, bChildrenField ) leafDecoder))
        , JD.map (\data -> Tree3 { data = data, children = Array.empty })
            (leafDecoder |> JD.map Data.LeafData)
        ]


getFocusedTree3 : TreePath3 a b leaf -> Tree3 a b leaf
getFocusedTree3 (TreePath3 { tree, path }) =
    tree


treeChildren3 : Tree3 a b leaf -> Array (Tree2 b leaf)
treeChildren3 (Tree3 { children }) =
    children


treeData3 : Tree3 a b leaf -> Data a leaf
treeData3 (Tree3 { data }) =
    data


data3 : TreePath3 a b leaf -> Data a leaf
data3 =
    getFocusedTree3 >> treeData3


top3 : TreePath3 a b leaf -> TreePath3 a b leaf
top3 (TreePath3 { tree, path }) =
    TreePath3 { tree = tree, path = Array.empty }


offset3 : Int -> TreePath3 a b leaf -> Maybe (TreePath3 a b leaf)
offset3 dx ((TreePath3 { tree, path }) as treePath) =
    Nothing


down3 : Int -> TreePath3 a b leaf -> Maybe (TreePath2 a b leaf)
down3 idx ((TreePath3 { tree, path }) as treePath) =
    getFocusedTree3 treePath
        |> treeChildren3
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath2 { tree = tree, path = Array.push idx path })


up3 : TreePath3 a b leaf -> Maybe Never
up3 (TreePath3 { tree, path }) =
    Nothing


unsafe : String -> Maybe a -> a
unsafe msg maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.crash msg
