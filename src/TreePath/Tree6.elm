module TreePath.Tree6
    exposing
        ( decoder
        , toRootPath
        , Tree
        , TreePath6
        , data6
        , top6
        , up6
        , offset6
        , down6
        , TreePath5
        , data5
        , top5
        , up5
        , offset5
        , down5
        , TreePath4
        , data4
        , top4
        , up4
        , offset4
        , down4
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


type alias Tree a b c d e leaf =
    Tree6 a b c d e leaf


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


type Tree4 a b c leaf
    = Tree4
        { data : Data a leaf
        , children : Array (Tree3 b c leaf)
        }


type Tree5 a b c d leaf
    = Tree5
        { data : Data a leaf
        , children : Array (Tree4 b c d leaf)
        }


type Tree6 a b c d e leaf
    = Tree6
        { data : Data a leaf
        , children : Array (Tree5 b c d e leaf)
        }


type TreePath1 a b c d e leaf
    = TreePath1
        { tree : Tree6 a b c d e leaf
        , path : Array Int
        }


type TreePath2 a b c d e leaf
    = TreePath2
        { tree : Tree6 a b c d e leaf
        , path : Array Int
        }


type TreePath3 a b c d e leaf
    = TreePath3
        { tree : Tree6 a b c d e leaf
        , path : Array Int
        }


type TreePath4 a b c d e leaf
    = TreePath4
        { tree : Tree6 a b c d e leaf
        , path : Array Int
        }


type TreePath5 a b c d e leaf
    = TreePath5
        { tree : Tree6 a b c d e leaf
        , path : Array Int
        }


type TreePath6 a b c d e leaf
    = TreePath6
        { tree : Tree6 a b c d e leaf
        , path : Array Int
        }


type alias DecoderConfig a b c d e leaf =
    { level6Decoder : Decoder a
    , level6ChildrenField : String
    , level5Decoder : Decoder b
    , level5ChildrenField : String
    , level4Decoder : Decoder c
    , level4ChildrenField : String
    , level3Decoder : Decoder d
    , level3ChildrenField : String
    , level2Decoder : Decoder e
    , level2ChildrenField : String
    , leafDecoder : Decoder leaf
    }


decoder : DecoderConfig a b c d e leaf -> Decoder (Tree6 a b c d e leaf)
decoder config =
    decoder6
        ( config.level6Decoder, config.level6ChildrenField )
        ( config.level5Decoder, config.level5ChildrenField )
        ( config.level4Decoder, config.level4ChildrenField )
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


getFocusedTree1 : TreePath1 a b c d e leaf -> Tree1 leaf
getFocusedTree1 (TreePath1 { tree, path }) =
    getFocusedTree2 (TreePath2 { tree = tree, path = path })
        |> treeChildren2
        |> Array.get (Array.get 4 path |> unsafe "getFocusedTree1")
        |> unsafe "getFocusedTree1"


treeData1 : Tree1 leaf -> leaf
treeData1 (Tree1 { data }) =
    data


data1 : TreePath1 a b c d e leaf -> leaf
data1 =
    getFocusedTree1 >> treeData1


top1 : TreePath1 a b c d e leaf -> TreePath6 a b c d e leaf
top1 (TreePath1 { tree, path }) =
    TreePath6 { tree = tree, path = Array.empty }


offset1 : Int -> TreePath1 a b c d e leaf -> Maybe (TreePath1 a b c d e leaf)
offset1 dx ((TreePath1 { tree, path }) as treePath) =
    treePath
        |> up1
        |> Maybe.andThen (down2 <| (Array.get 4 path |> unsafe "offset1") + dx)


down1 : Int -> TreePath1 a b c d e leaf -> Maybe Never
down1 idx ((TreePath1 { tree, path }) as treePath) =
    Nothing


up1 : TreePath1 a b c d e leaf -> Maybe (TreePath2 a b c d e leaf)
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


getFocusedTree2 : TreePath2 a b c d e leaf -> Tree2 e leaf
getFocusedTree2 (TreePath2 { tree, path }) =
    getFocusedTree3 (TreePath3 { tree = tree, path = path })
        |> treeChildren3
        |> Array.get (Array.get 3 path |> unsafe "getFocusedTree2")
        |> unsafe "getFocusedTree2"


treeChildren2 : Tree2 a leaf -> Array (Tree1 leaf)
treeChildren2 (Tree2 { children }) =
    children


treeData2 : Tree2 a leaf -> Data a leaf
treeData2 (Tree2 { data }) =
    data


data2 : TreePath2 a b c d e leaf -> Data e leaf
data2 =
    getFocusedTree2 >> treeData2


top2 : TreePath2 a b c d e leaf -> TreePath6 a b c d e leaf
top2 (TreePath2 { tree, path }) =
    TreePath6 { tree = tree, path = Array.empty }


offset2 : Int -> TreePath2 a b c d e leaf -> Maybe (TreePath2 a b c d e leaf)
offset2 dx ((TreePath2 { tree, path }) as treePath) =
    treePath
        |> up2
        |> Maybe.andThen (down3 <| (Array.get 3 path |> unsafe "offset2") + dx)


down2 : Int -> TreePath2 a b c d e leaf -> Maybe (TreePath1 a b c d e leaf)
down2 idx ((TreePath2 { tree, path }) as treePath) =
    getFocusedTree2 treePath
        |> treeChildren2
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath1 { tree = tree, path = Array.push idx path })


up2 : TreePath2 a b c d e leaf -> Maybe (TreePath3 a b c d e leaf)
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


getFocusedTree3 : TreePath3 a b c d e leaf -> Tree3 d e leaf
getFocusedTree3 (TreePath3 { tree, path }) =
    getFocusedTree4 (TreePath4 { tree = tree, path = path })
        |> treeChildren4
        |> Array.get (Array.get 2 path |> unsafe "getFocusedTree3")
        |> unsafe "getFocusedTree3"


treeChildren3 : Tree3 a b leaf -> Array (Tree2 b leaf)
treeChildren3 (Tree3 { children }) =
    children


treeData3 : Tree3 a b leaf -> Data a leaf
treeData3 (Tree3 { data }) =
    data


data3 : TreePath3 a b c d e leaf -> Data d leaf
data3 =
    getFocusedTree3 >> treeData3


top3 : TreePath3 a b c d e leaf -> TreePath6 a b c d e leaf
top3 (TreePath3 { tree, path }) =
    TreePath6 { tree = tree, path = Array.empty }


offset3 : Int -> TreePath3 a b c d e leaf -> Maybe (TreePath3 a b c d e leaf)
offset3 dx ((TreePath3 { tree, path }) as treePath) =
    treePath
        |> up3
        |> Maybe.andThen (down4 <| (Array.get 2 path |> unsafe "offset3") + dx)


down3 : Int -> TreePath3 a b c d e leaf -> Maybe (TreePath2 a b c d e leaf)
down3 idx ((TreePath3 { tree, path }) as treePath) =
    getFocusedTree3 treePath
        |> treeChildren3
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath2 { tree = tree, path = Array.push idx path })


up3 : TreePath3 a b c d e leaf -> Maybe (TreePath4 a b c d e leaf)
up3 (TreePath3 { tree, path }) =
    Just <| TreePath4 { tree = tree, path = Array.slice 0 -1 path }


decoder4 : ( Decoder a, String ) -> ( Decoder b, String ) -> ( Decoder c, String ) -> Decoder leaf -> Decoder (Tree4 a b c leaf)
decoder4 ( aDecoder, aChildrenField ) ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) leafDecoder =
    JD.oneOf
        [ JD.map2 (\data children -> Tree4 { data = data, children = children })
            (aDecoder |> JD.map Data.BranchData)
            (JD.field aChildrenField (JD.array <| decoder3 ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) leafDecoder))
        , JD.map (\data -> Tree4 { data = data, children = Array.empty })
            (leafDecoder |> JD.map Data.LeafData)
        ]


getFocusedTree4 : TreePath4 a b c d e leaf -> Tree4 c d e leaf
getFocusedTree4 (TreePath4 { tree, path }) =
    getFocusedTree5 (TreePath5 { tree = tree, path = path })
        |> treeChildren5
        |> Array.get (Array.get 1 path |> unsafe "getFocusedTree4")
        |> unsafe "getFocusedTree4"


treeChildren4 : Tree4 a b c leaf -> Array (Tree3 b c leaf)
treeChildren4 (Tree4 { children }) =
    children


treeData4 : Tree4 a b c leaf -> Data a leaf
treeData4 (Tree4 { data }) =
    data


data4 : TreePath4 a b c d e leaf -> Data c leaf
data4 =
    getFocusedTree4 >> treeData4


top4 : TreePath4 a b c d e leaf -> TreePath6 a b c d e leaf
top4 (TreePath4 { tree, path }) =
    TreePath6 { tree = tree, path = Array.empty }


offset4 : Int -> TreePath4 a b c d e leaf -> Maybe (TreePath4 a b c d e leaf)
offset4 dx ((TreePath4 { tree, path }) as treePath) =
    treePath
        |> up4
        |> Maybe.andThen (down5 <| (Array.get 1 path |> unsafe "offset4") + dx)


down4 : Int -> TreePath4 a b c d e leaf -> Maybe (TreePath3 a b c d e leaf)
down4 idx ((TreePath4 { tree, path }) as treePath) =
    getFocusedTree4 treePath
        |> treeChildren4
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath3 { tree = tree, path = Array.push idx path })


up4 : TreePath4 a b c d e leaf -> Maybe (TreePath5 a b c d e leaf)
up4 (TreePath4 { tree, path }) =
    Just <| TreePath5 { tree = tree, path = Array.slice 0 -1 path }


decoder5 : ( Decoder a, String ) -> ( Decoder b, String ) -> ( Decoder c, String ) -> ( Decoder d, String ) -> Decoder leaf -> Decoder (Tree5 a b c d leaf)
decoder5 ( aDecoder, aChildrenField ) ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) ( dDecoder, dChildrenField ) leafDecoder =
    JD.oneOf
        [ JD.map2 (\data children -> Tree5 { data = data, children = children })
            (aDecoder |> JD.map Data.BranchData)
            (JD.field aChildrenField (JD.array <| decoder4 ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) ( dDecoder, dChildrenField ) leafDecoder))
        , JD.map (\data -> Tree5 { data = data, children = Array.empty })
            (leafDecoder |> JD.map Data.LeafData)
        ]


getFocusedTree5 : TreePath5 a b c d e leaf -> Tree5 b c d e leaf
getFocusedTree5 (TreePath5 { tree, path }) =
    getFocusedTree6 (TreePath6 { tree = tree, path = path })
        |> treeChildren6
        |> Array.get (Array.get 0 path |> unsafe "getFocusedTree5")
        |> unsafe "getFocusedTree5"


treeChildren5 : Tree5 a b c d leaf -> Array (Tree4 b c d leaf)
treeChildren5 (Tree5 { children }) =
    children


treeData5 : Tree5 a b c d leaf -> Data a leaf
treeData5 (Tree5 { data }) =
    data


data5 : TreePath5 a b c d e leaf -> Data b leaf
data5 =
    getFocusedTree5 >> treeData5


top5 : TreePath5 a b c d e leaf -> TreePath6 a b c d e leaf
top5 (TreePath5 { tree, path }) =
    TreePath6 { tree = tree, path = Array.empty }


offset5 : Int -> TreePath5 a b c d e leaf -> Maybe (TreePath5 a b c d e leaf)
offset5 dx ((TreePath5 { tree, path }) as treePath) =
    treePath
        |> up5
        |> Maybe.andThen (down6 <| (Array.get 0 path |> unsafe "offset5") + dx)


down5 : Int -> TreePath5 a b c d e leaf -> Maybe (TreePath4 a b c d e leaf)
down5 idx ((TreePath5 { tree, path }) as treePath) =
    getFocusedTree5 treePath
        |> treeChildren5
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath4 { tree = tree, path = Array.push idx path })


up5 : TreePath5 a b c d e leaf -> Maybe (TreePath6 a b c d e leaf)
up5 (TreePath5 { tree, path }) =
    Just <| TreePath6 { tree = tree, path = Array.slice 0 -1 path }


decoder6 : ( Decoder a, String ) -> ( Decoder b, String ) -> ( Decoder c, String ) -> ( Decoder d, String ) -> ( Decoder e, String ) -> Decoder leaf -> Decoder (Tree6 a b c d e leaf)
decoder6 ( aDecoder, aChildrenField ) ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) ( dDecoder, dChildrenField ) ( eDecoder, eChildrenField ) leafDecoder =
    JD.oneOf
        [ JD.map2 (\data children -> Tree6 { data = data, children = children })
            (aDecoder |> JD.map Data.BranchData)
            (JD.field aChildrenField (JD.array <| decoder5 ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) ( dDecoder, dChildrenField ) ( eDecoder, eChildrenField ) leafDecoder))
        , JD.map (\data -> Tree6 { data = data, children = Array.empty })
            (leafDecoder |> JD.map Data.LeafData)
        ]


getFocusedTree6 : TreePath6 a b c d e leaf -> Tree6 a b c d e leaf
getFocusedTree6 (TreePath6 { tree, path }) =
    tree


treeChildren6 : Tree6 a b c d e leaf -> Array (Tree5 b c d e leaf)
treeChildren6 (Tree6 { children }) =
    children


treeData6 : Tree6 a b c d e leaf -> Data a leaf
treeData6 (Tree6 { data }) =
    data


data6 : TreePath6 a b c d e leaf -> Data a leaf
data6 =
    getFocusedTree6 >> treeData6


top6 : TreePath6 a b c d e leaf -> TreePath6 a b c d e leaf
top6 (TreePath6 { tree, path }) =
    TreePath6 { tree = tree, path = Array.empty }


offset6 : Int -> TreePath6 a b c d e leaf -> Maybe (TreePath6 a b c d e leaf)
offset6 dx ((TreePath6 { tree, path }) as treePath) =
    Nothing


down6 : Int -> TreePath6 a b c d e leaf -> Maybe (TreePath5 a b c d e leaf)
down6 idx ((TreePath6 { tree, path }) as treePath) =
    getFocusedTree6 treePath
        |> treeChildren6
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath5 { tree = tree, path = Array.push idx path })


up6 : TreePath6 a b c d e leaf -> Maybe Never
up6 (TreePath6 { tree, path }) =
    Nothing


unsafe : String -> Maybe a -> a
unsafe msg maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.crash msg
