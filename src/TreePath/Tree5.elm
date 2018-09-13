module TreePath.Tree5
    exposing
        ( DecoderConfig
        , decoder
        , pathDecoder
        , toRootPath
        , Tree
        , TreePath5
        , pathEncode5
        , data5
        , top5
        , up5
        , offset5
        , down5
        , downs5
        , TreePath4
        , pathEncode4
        , data4
        , top4
        , up4
        , offset4
        , down4
        , downs4
        , TreePath3
        , pathEncode3
        , data3
        , top3
        , up3
        , offset3
        , down3
        , downs3
        , TreePath2
        , pathEncode2
        , data2
        , top2
        , up2
        , offset2
        , down2
        , downs2
        , TreePath1
        , pathEncode1
        , data1
        , top1
        , up1
        , offset1
        , down1
        , downs1
        )

{-| This module provides types and functions for managing a strongly typed tree
of depth 5. Each level of the tree can have its own type, and each level can
contain Data either of that type, or the leaf type.


# Definition

@docs Tree, TreePath1, TreePath2, TreePath3, TreePath4, TreePath5


# Encoders and decoders

@docs DecoderConfig, decoder, pathDecoder, pathEncode1, pathEncode2, pathEncode3, pathEncode4, pathEncode5


# Path constructor

@docs toRootPath


# Data

@docs data1, data2, data3, data4, data5


# Navigation

@docs top1, up1, offset1, down1, downs1, top2, up2, offset2, down2, downs2, top3, up3, offset3, down3, downs3, top4, up4, offset4, down4, downs4, top5, up5, offset5, down5, downs5

-}

import TreePath.Data as Data exposing (Data)
import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| -}
type alias Tree a b c d leaf =
    Tree5 a b c d leaf


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


{-| -}
type TreePath1 a b c d leaf
    = TreePath1
        { tree : Tree5 a b c d leaf
        , path : Array Int
        }


{-| -}
type TreePath2 a b c d leaf
    = TreePath2
        { tree : Tree5 a b c d leaf
        , path : Array Int
        }


{-| -}
type TreePath3 a b c d leaf
    = TreePath3
        { tree : Tree5 a b c d leaf
        , path : Array Int
        }


{-| -}
type TreePath4 a b c d leaf
    = TreePath4
        { tree : Tree5 a b c d leaf
        , path : Array Int
        }


{-| -}
type TreePath5 a b c d leaf
    = TreePath5
        { tree : Tree5 a b c d leaf
        , path : Array Int
        }


{-| -}
type alias DecoderConfig a b c d leaf path =
    { level5 :
        { decoder : Decoder a
        , encoders : a -> List ( String, Value )
        , pathType : TreePath5 a b c d leaf -> path
        , childrenField : String
        }
    , level4 :
        { decoder : Decoder b
        , encoders : b -> List ( String, Value )
        , pathType : TreePath4 a b c d leaf -> path
        , childrenField : String
        }
    , level3 :
        { decoder : Decoder c
        , encoders : c -> List ( String, Value )
        , pathType : TreePath3 a b c d leaf -> path
        , childrenField : String
        }
    , level2 :
        { decoder : Decoder d
        , encoders : d -> List ( String, Value )
        , pathType : TreePath2 a b c d leaf -> path
        , childrenField : String
        }
    , leaf :
        { decoder : Decoder leaf
        , encode : leaf -> Value
        , pathType : TreePath1 a b c d leaf -> path
        }
    }


{-| -}
decoder : DecoderConfig a b c d leaf path -> Decoder (Tree5 a b c d leaf)
decoder config =
    decoder5
        ( config.level5.decoder, config.level5.childrenField )
        ( config.level4.decoder, config.level4.childrenField )
        ( config.level3.decoder, config.level3.childrenField )
        ( config.level2.decoder, config.level2.childrenField )
        config.leaf.decoder


encode : DecoderConfig a b c d leaf path -> Tree5 a b c d leaf -> Value
encode config tree =
    encode5
        ( config.level5.encoders, config.level5.childrenField )
        ( config.level4.encoders, config.level4.childrenField )
        ( config.level3.encoders, config.level3.childrenField )
        ( config.level2.encoders, config.level2.childrenField )
        config.leaf.encode
        tree


{-| -}
pathDecoder : DecoderConfig a b c d leaf path -> Decoder path
pathDecoder config =
    (JD.field "path" <| JD.array JD.int)
        |> JD.andThen
            (\path ->
                case Array.length path of
                    0 ->
                        JD.succeed (config.level5.pathType << TreePath5)

                    1 ->
                        JD.succeed (config.level4.pathType << TreePath4)

                    2 ->
                        JD.succeed (config.level3.pathType << TreePath3)

                    3 ->
                        JD.succeed (config.level2.pathType << TreePath2)

                    4 ->
                        JD.succeed (config.leaf.pathType << TreePath1)

                    otherwise ->
                        JD.fail <| "Illegal path length " ++ toString (Array.length path)
            )
        |> JD.andThen
            (\pathConstructor ->
                JD.map2 (\tree path -> pathConstructor { tree = tree, path = path })
                    (JD.field "tree" <| decoder config)
                    (JD.field "path" <| JD.array JD.int)
            )


{-| -}
toRootPath : Tree5 a b c d leaf -> TreePath5 a b c d leaf
toRootPath tree =
    TreePath5
        { tree = tree
        , path = Array.empty
        }


decoder1 : Decoder leaf -> Decoder (Tree1 leaf)
decoder1 leafDecoder =
    leafDecoder
        |> JD.map (\data -> Tree1 { data = data })


encode1 : (leaf -> Value) -> Tree1 leaf -> Value
encode1 leafEncode (Tree1 { data }) =
    leafEncode data


{-| -}
pathEncode1 : DecoderConfig a b c d leaf path -> TreePath1 a b c d leaf -> Value
pathEncode1 config (TreePath1 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree1 : TreePath1 a b c d leaf -> Tree1 leaf
getFocusedTree1 (TreePath1 { tree, path }) =
    getFocusedTree2 (TreePath2 { tree = tree, path = path })
        |> treeChildren2
        |> Array.get (Array.get 3 path |> unsafe "getFocusedTree1")
        |> unsafe "getFocusedTree1"


treeData1 : Tree1 leaf -> leaf
treeData1 (Tree1 { data }) =
    data


{-| -}
data1 : TreePath1 a b c d leaf -> leaf
data1 =
    getFocusedTree1 >> treeData1


{-| -}
top1 : TreePath1 a b c d leaf -> TreePath5 a b c d leaf
top1 (TreePath1 { tree, path }) =
    TreePath5 { tree = tree, path = Array.empty }


{-| -}
offset1 : Int -> TreePath1 a b c d leaf -> Maybe (TreePath1 a b c d leaf)
offset1 dx ((TreePath1 { tree, path }) as treePath) =
    treePath
        |> up1
        |> Maybe.andThen (down2 <| (Array.get 3 path |> unsafe "offset1") + dx)


{-| -}
down1 : Int -> TreePath1 a b c d leaf -> Maybe Never
down1 idx ((TreePath1 { tree, path }) as treePath) =
    Nothing


{-| -}
downs1 : TreePath1 a b c d leaf -> List Never
downs1 ((TreePath1 { tree, path }) as treePath) =
    []


{-| -}
up1 : TreePath1 a b c d leaf -> Maybe (TreePath2 a b c d leaf)
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


encode2 : ( a -> List ( String, Value ), String ) -> (leaf -> Value) -> Tree2 a leaf -> Value
encode2 ( aEncoders, aChildrenField ) leafEncode (Tree2 { data, children }) =
    case data of
        Data.BranchData b ->
            JE.object <|
                ( aChildrenField, JE.array <| Array.map (encode1 leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode2 : DecoderConfig a b c d leaf path -> TreePath2 a b c d leaf -> Value
pathEncode2 config (TreePath2 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree2 : TreePath2 a b c d leaf -> Tree2 d leaf
getFocusedTree2 (TreePath2 { tree, path }) =
    getFocusedTree3 (TreePath3 { tree = tree, path = path })
        |> treeChildren3
        |> Array.get (Array.get 2 path |> unsafe "getFocusedTree2")
        |> unsafe "getFocusedTree2"


treeChildren2 : Tree2 a leaf -> Array (Tree1 leaf)
treeChildren2 (Tree2 { children }) =
    children


treeData2 : Tree2 a leaf -> Data a leaf
treeData2 (Tree2 { data }) =
    data


{-| -}
data2 : TreePath2 a b c d leaf -> Data d leaf
data2 =
    getFocusedTree2 >> treeData2


{-| -}
top2 : TreePath2 a b c d leaf -> TreePath5 a b c d leaf
top2 (TreePath2 { tree, path }) =
    TreePath5 { tree = tree, path = Array.empty }


{-| -}
offset2 : Int -> TreePath2 a b c d leaf -> Maybe (TreePath2 a b c d leaf)
offset2 dx ((TreePath2 { tree, path }) as treePath) =
    treePath
        |> up2
        |> Maybe.andThen (down3 <| (Array.get 2 path |> unsafe "offset2") + dx)


{-| -}
down2 : Int -> TreePath2 a b c d leaf -> Maybe (TreePath1 a b c d leaf)
down2 idx ((TreePath2 { tree, path }) as treePath) =
    getFocusedTree2 treePath
        |> treeChildren2
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath1 { tree = tree, path = Array.push idx path })


{-| -}
downs2 : TreePath2 a b c d leaf -> List (TreePath1 a b c d leaf)
downs2 ((TreePath2 { tree, path }) as treePath) =
    getFocusedTree2 treePath
        |> treeChildren2
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath1 { tree = tree, path = Array.push idx path })


{-| -}
up2 : TreePath2 a b c d leaf -> Maybe (TreePath3 a b c d leaf)
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


encode3 : ( a -> List ( String, Value ), String ) -> ( b -> List ( String, Value ), String ) -> (leaf -> Value) -> Tree3 a b leaf -> Value
encode3 ( aEncoders, aChildrenField ) ( bEncoders, bChildrenField ) leafEncode (Tree3 { data, children }) =
    case data of
        Data.BranchData b ->
            JE.object <|
                ( aChildrenField, JE.array <| Array.map (encode2 ( bEncoders, bChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode3 : DecoderConfig a b c d leaf path -> TreePath3 a b c d leaf -> Value
pathEncode3 config (TreePath3 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree3 : TreePath3 a b c d leaf -> Tree3 c d leaf
getFocusedTree3 (TreePath3 { tree, path }) =
    getFocusedTree4 (TreePath4 { tree = tree, path = path })
        |> treeChildren4
        |> Array.get (Array.get 1 path |> unsafe "getFocusedTree3")
        |> unsafe "getFocusedTree3"


treeChildren3 : Tree3 a b leaf -> Array (Tree2 b leaf)
treeChildren3 (Tree3 { children }) =
    children


treeData3 : Tree3 a b leaf -> Data a leaf
treeData3 (Tree3 { data }) =
    data


{-| -}
data3 : TreePath3 a b c d leaf -> Data c leaf
data3 =
    getFocusedTree3 >> treeData3


{-| -}
top3 : TreePath3 a b c d leaf -> TreePath5 a b c d leaf
top3 (TreePath3 { tree, path }) =
    TreePath5 { tree = tree, path = Array.empty }


{-| -}
offset3 : Int -> TreePath3 a b c d leaf -> Maybe (TreePath3 a b c d leaf)
offset3 dx ((TreePath3 { tree, path }) as treePath) =
    treePath
        |> up3
        |> Maybe.andThen (down4 <| (Array.get 1 path |> unsafe "offset3") + dx)


{-| -}
down3 : Int -> TreePath3 a b c d leaf -> Maybe (TreePath2 a b c d leaf)
down3 idx ((TreePath3 { tree, path }) as treePath) =
    getFocusedTree3 treePath
        |> treeChildren3
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath2 { tree = tree, path = Array.push idx path })


{-| -}
downs3 : TreePath3 a b c d leaf -> List (TreePath2 a b c d leaf)
downs3 ((TreePath3 { tree, path }) as treePath) =
    getFocusedTree3 treePath
        |> treeChildren3
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath2 { tree = tree, path = Array.push idx path })


{-| -}
up3 : TreePath3 a b c d leaf -> Maybe (TreePath4 a b c d leaf)
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


encode4 : ( a -> List ( String, Value ), String ) -> ( b -> List ( String, Value ), String ) -> ( c -> List ( String, Value ), String ) -> (leaf -> Value) -> Tree4 a b c leaf -> Value
encode4 ( aEncoders, aChildrenField ) ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) leafEncode (Tree4 { data, children }) =
    case data of
        Data.BranchData b ->
            JE.object <|
                ( aChildrenField, JE.array <| Array.map (encode3 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode4 : DecoderConfig a b c d leaf path -> TreePath4 a b c d leaf -> Value
pathEncode4 config (TreePath4 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree4 : TreePath4 a b c d leaf -> Tree4 b c d leaf
getFocusedTree4 (TreePath4 { tree, path }) =
    getFocusedTree5 (TreePath5 { tree = tree, path = path })
        |> treeChildren5
        |> Array.get (Array.get 0 path |> unsafe "getFocusedTree4")
        |> unsafe "getFocusedTree4"


treeChildren4 : Tree4 a b c leaf -> Array (Tree3 b c leaf)
treeChildren4 (Tree4 { children }) =
    children


treeData4 : Tree4 a b c leaf -> Data a leaf
treeData4 (Tree4 { data }) =
    data


{-| -}
data4 : TreePath4 a b c d leaf -> Data b leaf
data4 =
    getFocusedTree4 >> treeData4


{-| -}
top4 : TreePath4 a b c d leaf -> TreePath5 a b c d leaf
top4 (TreePath4 { tree, path }) =
    TreePath5 { tree = tree, path = Array.empty }


{-| -}
offset4 : Int -> TreePath4 a b c d leaf -> Maybe (TreePath4 a b c d leaf)
offset4 dx ((TreePath4 { tree, path }) as treePath) =
    treePath
        |> up4
        |> Maybe.andThen (down5 <| (Array.get 0 path |> unsafe "offset4") + dx)


{-| -}
down4 : Int -> TreePath4 a b c d leaf -> Maybe (TreePath3 a b c d leaf)
down4 idx ((TreePath4 { tree, path }) as treePath) =
    getFocusedTree4 treePath
        |> treeChildren4
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath3 { tree = tree, path = Array.push idx path })


{-| -}
downs4 : TreePath4 a b c d leaf -> List (TreePath3 a b c d leaf)
downs4 ((TreePath4 { tree, path }) as treePath) =
    getFocusedTree4 treePath
        |> treeChildren4
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath3 { tree = tree, path = Array.push idx path })


{-| -}
up4 : TreePath4 a b c d leaf -> Maybe (TreePath5 a b c d leaf)
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


encode5 : ( a -> List ( String, Value ), String ) -> ( b -> List ( String, Value ), String ) -> ( c -> List ( String, Value ), String ) -> ( d -> List ( String, Value ), String ) -> (leaf -> Value) -> Tree5 a b c d leaf -> Value
encode5 ( aEncoders, aChildrenField ) ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) leafEncode (Tree5 { data, children }) =
    case data of
        Data.BranchData b ->
            JE.object <|
                ( aChildrenField, JE.array <| Array.map (encode4 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode5 : DecoderConfig a b c d leaf path -> TreePath5 a b c d leaf -> Value
pathEncode5 config (TreePath5 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree5 : TreePath5 a b c d leaf -> Tree5 a b c d leaf
getFocusedTree5 (TreePath5 { tree, path }) =
    tree


treeChildren5 : Tree5 a b c d leaf -> Array (Tree4 b c d leaf)
treeChildren5 (Tree5 { children }) =
    children


treeData5 : Tree5 a b c d leaf -> Data a leaf
treeData5 (Tree5 { data }) =
    data


{-| -}
data5 : TreePath5 a b c d leaf -> Data a leaf
data5 =
    getFocusedTree5 >> treeData5


{-| -}
top5 : TreePath5 a b c d leaf -> TreePath5 a b c d leaf
top5 (TreePath5 { tree, path }) =
    TreePath5 { tree = tree, path = Array.empty }


{-| -}
offset5 : Int -> TreePath5 a b c d leaf -> Maybe (TreePath5 a b c d leaf)
offset5 dx ((TreePath5 { tree, path }) as treePath) =
    Nothing


{-| -}
down5 : Int -> TreePath5 a b c d leaf -> Maybe (TreePath4 a b c d leaf)
down5 idx ((TreePath5 { tree, path }) as treePath) =
    getFocusedTree5 treePath
        |> treeChildren5
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath4 { tree = tree, path = Array.push idx path })


{-| -}
downs5 : TreePath5 a b c d leaf -> List (TreePath4 a b c d leaf)
downs5 ((TreePath5 { tree, path }) as treePath) =
    getFocusedTree5 treePath
        |> treeChildren5
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath4 { tree = tree, path = Array.push idx path })


{-| -}
up5 : TreePath5 a b c d leaf -> Maybe Never
up5 (TreePath5 { tree, path }) =
    Nothing


unsafe : String -> Maybe a -> a
unsafe msg maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.crash msg
