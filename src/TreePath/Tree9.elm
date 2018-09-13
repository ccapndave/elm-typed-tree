module TreePath.Tree9
    exposing
        ( DecoderConfig
        , decoder
        , pathDecoder
        , toRootPath
        , Tree
        , TreePath9
        , pathEncode9
        , data9
        , top9
        , up9
        , offset9
        , down9
        , downs9
        , TreePath8
        , pathEncode8
        , data8
        , top8
        , up8
        , offset8
        , down8
        , downs8
        , TreePath7
        , pathEncode7
        , data7
        , top7
        , up7
        , offset7
        , down7
        , downs7
        , TreePath6
        , pathEncode6
        , data6
        , top6
        , up6
        , offset6
        , down6
        , downs6
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
of depth 9. Each level of the tree can have its own type, and each level can
contain Data either of that type, or the leaf type.


# Definition

@docs Tree, TreePath1, TreePath2, TreePath3, TreePath4, TreePath5, TreePath6, TreePath7, TreePath8, TreePath9


# Encoders and decoders

@docs DecoderConfig, decoder, pathDecoder, pathEncode1, pathEncode2, pathEncode3, pathEncode4, pathEncode5, pathEncode6, pathEncode7, pathEncode8, pathEncode9


# Path constructor

@docs toRootPath


# Data

@docs data1, data2, data3, data4, data5, data6, data7, data8, data9


# Navigation

@docs top1, up1, offset1, down1, downs1, top2, up2, offset2, down2, downs2, top3, up3, offset3, down3, downs3, top4, up4, offset4, down4, downs4, top5, up5, offset5, down5, downs5, top6, up6, offset6, down6, downs6, top7, up7, offset7, down7, downs7, top8, up8, offset8, down8, downs8, top9, up9, offset9, down9, downs9

-}

import TreePath.Data as Data exposing (Data)
import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| -}
type alias Tree a b c d e f g h leaf =
    Tree9 a b c d e f g h leaf


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


type Tree7 a b c d e f leaf
    = Tree7
        { data : Data a leaf
        , children : Array (Tree6 b c d e f leaf)
        }


type Tree8 a b c d e f g leaf
    = Tree8
        { data : Data a leaf
        , children : Array (Tree7 b c d e f g leaf)
        }


type Tree9 a b c d e f g h leaf
    = Tree9
        { data : Data a leaf
        , children : Array (Tree8 b c d e f g h leaf)
        }


{-| -}
type TreePath1 a b c d e f g h leaf
    = TreePath1
        { tree : Tree9 a b c d e f g h leaf
        , path : Array Int
        }


{-| -}
type TreePath2 a b c d e f g h leaf
    = TreePath2
        { tree : Tree9 a b c d e f g h leaf
        , path : Array Int
        }


{-| -}
type TreePath3 a b c d e f g h leaf
    = TreePath3
        { tree : Tree9 a b c d e f g h leaf
        , path : Array Int
        }


{-| -}
type TreePath4 a b c d e f g h leaf
    = TreePath4
        { tree : Tree9 a b c d e f g h leaf
        , path : Array Int
        }


{-| -}
type TreePath5 a b c d e f g h leaf
    = TreePath5
        { tree : Tree9 a b c d e f g h leaf
        , path : Array Int
        }


{-| -}
type TreePath6 a b c d e f g h leaf
    = TreePath6
        { tree : Tree9 a b c d e f g h leaf
        , path : Array Int
        }


{-| -}
type TreePath7 a b c d e f g h leaf
    = TreePath7
        { tree : Tree9 a b c d e f g h leaf
        , path : Array Int
        }


{-| -}
type TreePath8 a b c d e f g h leaf
    = TreePath8
        { tree : Tree9 a b c d e f g h leaf
        , path : Array Int
        }


{-| -}
type TreePath9 a b c d e f g h leaf
    = TreePath9
        { tree : Tree9 a b c d e f g h leaf
        , path : Array Int
        }


{-| -}
type alias DecoderConfig a b c d e f g h leaf path =
    { level9 :
        { decoder : Decoder a
        , encoders : a -> List ( String, Value )
        , pathType : TreePath9 a b c d e f g h leaf -> path
        , childrenField : String
        }
    , level8 :
        { decoder : Decoder b
        , encoders : b -> List ( String, Value )
        , pathType : TreePath8 a b c d e f g h leaf -> path
        , childrenField : String
        }
    , level7 :
        { decoder : Decoder c
        , encoders : c -> List ( String, Value )
        , pathType : TreePath7 a b c d e f g h leaf -> path
        , childrenField : String
        }
    , level6 :
        { decoder : Decoder d
        , encoders : d -> List ( String, Value )
        , pathType : TreePath6 a b c d e f g h leaf -> path
        , childrenField : String
        }
    , level5 :
        { decoder : Decoder e
        , encoders : e -> List ( String, Value )
        , pathType : TreePath5 a b c d e f g h leaf -> path
        , childrenField : String
        }
    , level4 :
        { decoder : Decoder f
        , encoders : f -> List ( String, Value )
        , pathType : TreePath4 a b c d e f g h leaf -> path
        , childrenField : String
        }
    , level3 :
        { decoder : Decoder g
        , encoders : g -> List ( String, Value )
        , pathType : TreePath3 a b c d e f g h leaf -> path
        , childrenField : String
        }
    , level2 :
        { decoder : Decoder h
        , encoders : h -> List ( String, Value )
        , pathType : TreePath2 a b c d e f g h leaf -> path
        , childrenField : String
        }
    , leaf :
        { decoder : Decoder leaf
        , encode : leaf -> Value
        , pathType : TreePath1 a b c d e f g h leaf -> path
        }
    }


{-| -}
decoder : DecoderConfig a b c d e f g h leaf path -> Decoder (Tree9 a b c d e f g h leaf)
decoder config =
    decoder9
        ( config.level9.decoder, config.level9.childrenField )
        ( config.level8.decoder, config.level8.childrenField )
        ( config.level7.decoder, config.level7.childrenField )
        ( config.level6.decoder, config.level6.childrenField )
        ( config.level5.decoder, config.level5.childrenField )
        ( config.level4.decoder, config.level4.childrenField )
        ( config.level3.decoder, config.level3.childrenField )
        ( config.level2.decoder, config.level2.childrenField )
        config.leaf.decoder


encode : DecoderConfig a b c d e f g h leaf path -> Tree9 a b c d e f g h leaf -> Value
encode config tree =
    encode9
        ( config.level9.encoders, config.level9.childrenField )
        ( config.level8.encoders, config.level8.childrenField )
        ( config.level7.encoders, config.level7.childrenField )
        ( config.level6.encoders, config.level6.childrenField )
        ( config.level5.encoders, config.level5.childrenField )
        ( config.level4.encoders, config.level4.childrenField )
        ( config.level3.encoders, config.level3.childrenField )
        ( config.level2.encoders, config.level2.childrenField )
        config.leaf.encode
        tree


{-| -}
pathDecoder : DecoderConfig a b c d e f g h leaf path -> Decoder path
pathDecoder config =
    (JD.field "path" <| JD.array JD.int)
        |> JD.andThen
            (\path ->
                case Array.length path of
                    0 ->
                        JD.succeed (config.level9.pathType << TreePath9)

                    1 ->
                        JD.succeed (config.level8.pathType << TreePath8)

                    2 ->
                        JD.succeed (config.level7.pathType << TreePath7)

                    3 ->
                        JD.succeed (config.level6.pathType << TreePath6)

                    4 ->
                        JD.succeed (config.level5.pathType << TreePath5)

                    5 ->
                        JD.succeed (config.level4.pathType << TreePath4)

                    6 ->
                        JD.succeed (config.level3.pathType << TreePath3)

                    7 ->
                        JD.succeed (config.level2.pathType << TreePath2)

                    8 ->
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
toRootPath : Tree9 a b c d e f g h leaf -> TreePath9 a b c d e f g h leaf
toRootPath tree =
    TreePath9
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
pathEncode1 : DecoderConfig a b c d e f g h leaf path -> TreePath1 a b c d e f g h leaf -> Value
pathEncode1 config (TreePath1 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree1 : TreePath1 a b c d e f g h leaf -> Tree1 leaf
getFocusedTree1 (TreePath1 { tree, path }) =
    getFocusedTree2 (TreePath2 { tree = tree, path = path })
        |> treeChildren2
        |> Array.get (Array.get 7 path |> unsafe "getFocusedTree1")
        |> unsafe "getFocusedTree1"


treeData1 : Tree1 leaf -> leaf
treeData1 (Tree1 { data }) =
    data


{-| -}
data1 : TreePath1 a b c d e f g h leaf -> leaf
data1 =
    getFocusedTree1 >> treeData1


{-| -}
top1 : TreePath1 a b c d e f g h leaf -> TreePath9 a b c d e f g h leaf
top1 (TreePath1 { tree, path }) =
    TreePath9 { tree = tree, path = Array.empty }


{-| -}
offset1 : Int -> TreePath1 a b c d e f g h leaf -> Maybe (TreePath1 a b c d e f g h leaf)
offset1 dx ((TreePath1 { tree, path }) as treePath) =
    treePath
        |> up1
        |> Maybe.andThen (down2 <| (Array.get 7 path |> unsafe "offset1") + dx)


{-| -}
down1 : Int -> TreePath1 a b c d e f g h leaf -> Maybe Never
down1 idx ((TreePath1 { tree, path }) as treePath) =
    Nothing


{-| -}
downs1 : TreePath1 a b c d e f g h leaf -> List Never
downs1 ((TreePath1 { tree, path }) as treePath) =
    []


{-| -}
up1 : TreePath1 a b c d e f g h leaf -> Maybe (TreePath2 a b c d e f g h leaf)
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
pathEncode2 : DecoderConfig a b c d e f g h leaf path -> TreePath2 a b c d e f g h leaf -> Value
pathEncode2 config (TreePath2 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree2 : TreePath2 a b c d e f g h leaf -> Tree2 h leaf
getFocusedTree2 (TreePath2 { tree, path }) =
    getFocusedTree3 (TreePath3 { tree = tree, path = path })
        |> treeChildren3
        |> Array.get (Array.get 6 path |> unsafe "getFocusedTree2")
        |> unsafe "getFocusedTree2"


treeChildren2 : Tree2 a leaf -> Array (Tree1 leaf)
treeChildren2 (Tree2 { children }) =
    children


treeData2 : Tree2 a leaf -> Data a leaf
treeData2 (Tree2 { data }) =
    data


{-| -}
data2 : TreePath2 a b c d e f g h leaf -> Data h leaf
data2 =
    getFocusedTree2 >> treeData2


{-| -}
top2 : TreePath2 a b c d e f g h leaf -> TreePath9 a b c d e f g h leaf
top2 (TreePath2 { tree, path }) =
    TreePath9 { tree = tree, path = Array.empty }


{-| -}
offset2 : Int -> TreePath2 a b c d e f g h leaf -> Maybe (TreePath2 a b c d e f g h leaf)
offset2 dx ((TreePath2 { tree, path }) as treePath) =
    treePath
        |> up2
        |> Maybe.andThen (down3 <| (Array.get 6 path |> unsafe "offset2") + dx)


{-| -}
down2 : Int -> TreePath2 a b c d e f g h leaf -> Maybe (TreePath1 a b c d e f g h leaf)
down2 idx ((TreePath2 { tree, path }) as treePath) =
    getFocusedTree2 treePath
        |> treeChildren2
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath1 { tree = tree, path = Array.push idx path })


{-| -}
downs2 : TreePath2 a b c d e f g h leaf -> List (TreePath1 a b c d e f g h leaf)
downs2 ((TreePath2 { tree, path }) as treePath) =
    getFocusedTree2 treePath
        |> treeChildren2
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath1 { tree = tree, path = Array.push idx path })


{-| -}
up2 : TreePath2 a b c d e f g h leaf -> Maybe (TreePath3 a b c d e f g h leaf)
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
pathEncode3 : DecoderConfig a b c d e f g h leaf path -> TreePath3 a b c d e f g h leaf -> Value
pathEncode3 config (TreePath3 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree3 : TreePath3 a b c d e f g h leaf -> Tree3 g h leaf
getFocusedTree3 (TreePath3 { tree, path }) =
    getFocusedTree4 (TreePath4 { tree = tree, path = path })
        |> treeChildren4
        |> Array.get (Array.get 5 path |> unsafe "getFocusedTree3")
        |> unsafe "getFocusedTree3"


treeChildren3 : Tree3 a b leaf -> Array (Tree2 b leaf)
treeChildren3 (Tree3 { children }) =
    children


treeData3 : Tree3 a b leaf -> Data a leaf
treeData3 (Tree3 { data }) =
    data


{-| -}
data3 : TreePath3 a b c d e f g h leaf -> Data g leaf
data3 =
    getFocusedTree3 >> treeData3


{-| -}
top3 : TreePath3 a b c d e f g h leaf -> TreePath9 a b c d e f g h leaf
top3 (TreePath3 { tree, path }) =
    TreePath9 { tree = tree, path = Array.empty }


{-| -}
offset3 : Int -> TreePath3 a b c d e f g h leaf -> Maybe (TreePath3 a b c d e f g h leaf)
offset3 dx ((TreePath3 { tree, path }) as treePath) =
    treePath
        |> up3
        |> Maybe.andThen (down4 <| (Array.get 5 path |> unsafe "offset3") + dx)


{-| -}
down3 : Int -> TreePath3 a b c d e f g h leaf -> Maybe (TreePath2 a b c d e f g h leaf)
down3 idx ((TreePath3 { tree, path }) as treePath) =
    getFocusedTree3 treePath
        |> treeChildren3
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath2 { tree = tree, path = Array.push idx path })


{-| -}
downs3 : TreePath3 a b c d e f g h leaf -> List (TreePath2 a b c d e f g h leaf)
downs3 ((TreePath3 { tree, path }) as treePath) =
    getFocusedTree3 treePath
        |> treeChildren3
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath2 { tree = tree, path = Array.push idx path })


{-| -}
up3 : TreePath3 a b c d e f g h leaf -> Maybe (TreePath4 a b c d e f g h leaf)
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
pathEncode4 : DecoderConfig a b c d e f g h leaf path -> TreePath4 a b c d e f g h leaf -> Value
pathEncode4 config (TreePath4 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree4 : TreePath4 a b c d e f g h leaf -> Tree4 f g h leaf
getFocusedTree4 (TreePath4 { tree, path }) =
    getFocusedTree5 (TreePath5 { tree = tree, path = path })
        |> treeChildren5
        |> Array.get (Array.get 4 path |> unsafe "getFocusedTree4")
        |> unsafe "getFocusedTree4"


treeChildren4 : Tree4 a b c leaf -> Array (Tree3 b c leaf)
treeChildren4 (Tree4 { children }) =
    children


treeData4 : Tree4 a b c leaf -> Data a leaf
treeData4 (Tree4 { data }) =
    data


{-| -}
data4 : TreePath4 a b c d e f g h leaf -> Data f leaf
data4 =
    getFocusedTree4 >> treeData4


{-| -}
top4 : TreePath4 a b c d e f g h leaf -> TreePath9 a b c d e f g h leaf
top4 (TreePath4 { tree, path }) =
    TreePath9 { tree = tree, path = Array.empty }


{-| -}
offset4 : Int -> TreePath4 a b c d e f g h leaf -> Maybe (TreePath4 a b c d e f g h leaf)
offset4 dx ((TreePath4 { tree, path }) as treePath) =
    treePath
        |> up4
        |> Maybe.andThen (down5 <| (Array.get 4 path |> unsafe "offset4") + dx)


{-| -}
down4 : Int -> TreePath4 a b c d e f g h leaf -> Maybe (TreePath3 a b c d e f g h leaf)
down4 idx ((TreePath4 { tree, path }) as treePath) =
    getFocusedTree4 treePath
        |> treeChildren4
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath3 { tree = tree, path = Array.push idx path })


{-| -}
downs4 : TreePath4 a b c d e f g h leaf -> List (TreePath3 a b c d e f g h leaf)
downs4 ((TreePath4 { tree, path }) as treePath) =
    getFocusedTree4 treePath
        |> treeChildren4
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath3 { tree = tree, path = Array.push idx path })


{-| -}
up4 : TreePath4 a b c d e f g h leaf -> Maybe (TreePath5 a b c d e f g h leaf)
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
pathEncode5 : DecoderConfig a b c d e f g h leaf path -> TreePath5 a b c d e f g h leaf -> Value
pathEncode5 config (TreePath5 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree5 : TreePath5 a b c d e f g h leaf -> Tree5 e f g h leaf
getFocusedTree5 (TreePath5 { tree, path }) =
    getFocusedTree6 (TreePath6 { tree = tree, path = path })
        |> treeChildren6
        |> Array.get (Array.get 3 path |> unsafe "getFocusedTree5")
        |> unsafe "getFocusedTree5"


treeChildren5 : Tree5 a b c d leaf -> Array (Tree4 b c d leaf)
treeChildren5 (Tree5 { children }) =
    children


treeData5 : Tree5 a b c d leaf -> Data a leaf
treeData5 (Tree5 { data }) =
    data


{-| -}
data5 : TreePath5 a b c d e f g h leaf -> Data e leaf
data5 =
    getFocusedTree5 >> treeData5


{-| -}
top5 : TreePath5 a b c d e f g h leaf -> TreePath9 a b c d e f g h leaf
top5 (TreePath5 { tree, path }) =
    TreePath9 { tree = tree, path = Array.empty }


{-| -}
offset5 : Int -> TreePath5 a b c d e f g h leaf -> Maybe (TreePath5 a b c d e f g h leaf)
offset5 dx ((TreePath5 { tree, path }) as treePath) =
    treePath
        |> up5
        |> Maybe.andThen (down6 <| (Array.get 3 path |> unsafe "offset5") + dx)


{-| -}
down5 : Int -> TreePath5 a b c d e f g h leaf -> Maybe (TreePath4 a b c d e f g h leaf)
down5 idx ((TreePath5 { tree, path }) as treePath) =
    getFocusedTree5 treePath
        |> treeChildren5
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath4 { tree = tree, path = Array.push idx path })


{-| -}
downs5 : TreePath5 a b c d e f g h leaf -> List (TreePath4 a b c d e f g h leaf)
downs5 ((TreePath5 { tree, path }) as treePath) =
    getFocusedTree5 treePath
        |> treeChildren5
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath4 { tree = tree, path = Array.push idx path })


{-| -}
up5 : TreePath5 a b c d e f g h leaf -> Maybe (TreePath6 a b c d e f g h leaf)
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


encode6 : ( a -> List ( String, Value ), String ) -> ( b -> List ( String, Value ), String ) -> ( c -> List ( String, Value ), String ) -> ( d -> List ( String, Value ), String ) -> ( e -> List ( String, Value ), String ) -> (leaf -> Value) -> Tree6 a b c d e leaf -> Value
encode6 ( aEncoders, aChildrenField ) ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) leafEncode (Tree6 { data, children }) =
    case data of
        Data.BranchData b ->
            JE.object <|
                ( aChildrenField, JE.array <| Array.map (encode5 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode6 : DecoderConfig a b c d e f g h leaf path -> TreePath6 a b c d e f g h leaf -> Value
pathEncode6 config (TreePath6 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree6 : TreePath6 a b c d e f g h leaf -> Tree6 d e f g h leaf
getFocusedTree6 (TreePath6 { tree, path }) =
    getFocusedTree7 (TreePath7 { tree = tree, path = path })
        |> treeChildren7
        |> Array.get (Array.get 2 path |> unsafe "getFocusedTree6")
        |> unsafe "getFocusedTree6"


treeChildren6 : Tree6 a b c d e leaf -> Array (Tree5 b c d e leaf)
treeChildren6 (Tree6 { children }) =
    children


treeData6 : Tree6 a b c d e leaf -> Data a leaf
treeData6 (Tree6 { data }) =
    data


{-| -}
data6 : TreePath6 a b c d e f g h leaf -> Data d leaf
data6 =
    getFocusedTree6 >> treeData6


{-| -}
top6 : TreePath6 a b c d e f g h leaf -> TreePath9 a b c d e f g h leaf
top6 (TreePath6 { tree, path }) =
    TreePath9 { tree = tree, path = Array.empty }


{-| -}
offset6 : Int -> TreePath6 a b c d e f g h leaf -> Maybe (TreePath6 a b c d e f g h leaf)
offset6 dx ((TreePath6 { tree, path }) as treePath) =
    treePath
        |> up6
        |> Maybe.andThen (down7 <| (Array.get 2 path |> unsafe "offset6") + dx)


{-| -}
down6 : Int -> TreePath6 a b c d e f g h leaf -> Maybe (TreePath5 a b c d e f g h leaf)
down6 idx ((TreePath6 { tree, path }) as treePath) =
    getFocusedTree6 treePath
        |> treeChildren6
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath5 { tree = tree, path = Array.push idx path })


{-| -}
downs6 : TreePath6 a b c d e f g h leaf -> List (TreePath5 a b c d e f g h leaf)
downs6 ((TreePath6 { tree, path }) as treePath) =
    getFocusedTree6 treePath
        |> treeChildren6
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath5 { tree = tree, path = Array.push idx path })


{-| -}
up6 : TreePath6 a b c d e f g h leaf -> Maybe (TreePath7 a b c d e f g h leaf)
up6 (TreePath6 { tree, path }) =
    Just <| TreePath7 { tree = tree, path = Array.slice 0 -1 path }


decoder7 : ( Decoder a, String ) -> ( Decoder b, String ) -> ( Decoder c, String ) -> ( Decoder d, String ) -> ( Decoder e, String ) -> ( Decoder f, String ) -> Decoder leaf -> Decoder (Tree7 a b c d e f leaf)
decoder7 ( aDecoder, aChildrenField ) ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) ( dDecoder, dChildrenField ) ( eDecoder, eChildrenField ) ( fDecoder, fChildrenField ) leafDecoder =
    JD.oneOf
        [ JD.map2 (\data children -> Tree7 { data = data, children = children })
            (aDecoder |> JD.map Data.BranchData)
            (JD.field aChildrenField (JD.array <| decoder6 ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) ( dDecoder, dChildrenField ) ( eDecoder, eChildrenField ) ( fDecoder, fChildrenField ) leafDecoder))
        , JD.map (\data -> Tree7 { data = data, children = Array.empty })
            (leafDecoder |> JD.map Data.LeafData)
        ]


encode7 : ( a -> List ( String, Value ), String ) -> ( b -> List ( String, Value ), String ) -> ( c -> List ( String, Value ), String ) -> ( d -> List ( String, Value ), String ) -> ( e -> List ( String, Value ), String ) -> ( f -> List ( String, Value ), String ) -> (leaf -> Value) -> Tree7 a b c d e f leaf -> Value
encode7 ( aEncoders, aChildrenField ) ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) ( fEncoders, fChildrenField ) leafEncode (Tree7 { data, children }) =
    case data of
        Data.BranchData b ->
            JE.object <|
                ( aChildrenField, JE.array <| Array.map (encode6 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) ( fEncoders, fChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode7 : DecoderConfig a b c d e f g h leaf path -> TreePath7 a b c d e f g h leaf -> Value
pathEncode7 config (TreePath7 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree7 : TreePath7 a b c d e f g h leaf -> Tree7 c d e f g h leaf
getFocusedTree7 (TreePath7 { tree, path }) =
    getFocusedTree8 (TreePath8 { tree = tree, path = path })
        |> treeChildren8
        |> Array.get (Array.get 1 path |> unsafe "getFocusedTree7")
        |> unsafe "getFocusedTree7"


treeChildren7 : Tree7 a b c d e f leaf -> Array (Tree6 b c d e f leaf)
treeChildren7 (Tree7 { children }) =
    children


treeData7 : Tree7 a b c d e f leaf -> Data a leaf
treeData7 (Tree7 { data }) =
    data


{-| -}
data7 : TreePath7 a b c d e f g h leaf -> Data c leaf
data7 =
    getFocusedTree7 >> treeData7


{-| -}
top7 : TreePath7 a b c d e f g h leaf -> TreePath9 a b c d e f g h leaf
top7 (TreePath7 { tree, path }) =
    TreePath9 { tree = tree, path = Array.empty }


{-| -}
offset7 : Int -> TreePath7 a b c d e f g h leaf -> Maybe (TreePath7 a b c d e f g h leaf)
offset7 dx ((TreePath7 { tree, path }) as treePath) =
    treePath
        |> up7
        |> Maybe.andThen (down8 <| (Array.get 1 path |> unsafe "offset7") + dx)


{-| -}
down7 : Int -> TreePath7 a b c d e f g h leaf -> Maybe (TreePath6 a b c d e f g h leaf)
down7 idx ((TreePath7 { tree, path }) as treePath) =
    getFocusedTree7 treePath
        |> treeChildren7
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath6 { tree = tree, path = Array.push idx path })


{-| -}
downs7 : TreePath7 a b c d e f g h leaf -> List (TreePath6 a b c d e f g h leaf)
downs7 ((TreePath7 { tree, path }) as treePath) =
    getFocusedTree7 treePath
        |> treeChildren7
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath6 { tree = tree, path = Array.push idx path })


{-| -}
up7 : TreePath7 a b c d e f g h leaf -> Maybe (TreePath8 a b c d e f g h leaf)
up7 (TreePath7 { tree, path }) =
    Just <| TreePath8 { tree = tree, path = Array.slice 0 -1 path }


decoder8 : ( Decoder a, String ) -> ( Decoder b, String ) -> ( Decoder c, String ) -> ( Decoder d, String ) -> ( Decoder e, String ) -> ( Decoder f, String ) -> ( Decoder g, String ) -> Decoder leaf -> Decoder (Tree8 a b c d e f g leaf)
decoder8 ( aDecoder, aChildrenField ) ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) ( dDecoder, dChildrenField ) ( eDecoder, eChildrenField ) ( fDecoder, fChildrenField ) ( gDecoder, gChildrenField ) leafDecoder =
    JD.oneOf
        [ JD.map2 (\data children -> Tree8 { data = data, children = children })
            (aDecoder |> JD.map Data.BranchData)
            (JD.field aChildrenField (JD.array <| decoder7 ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) ( dDecoder, dChildrenField ) ( eDecoder, eChildrenField ) ( fDecoder, fChildrenField ) ( gDecoder, gChildrenField ) leafDecoder))
        , JD.map (\data -> Tree8 { data = data, children = Array.empty })
            (leafDecoder |> JD.map Data.LeafData)
        ]


encode8 : ( a -> List ( String, Value ), String ) -> ( b -> List ( String, Value ), String ) -> ( c -> List ( String, Value ), String ) -> ( d -> List ( String, Value ), String ) -> ( e -> List ( String, Value ), String ) -> ( f -> List ( String, Value ), String ) -> ( g -> List ( String, Value ), String ) -> (leaf -> Value) -> Tree8 a b c d e f g leaf -> Value
encode8 ( aEncoders, aChildrenField ) ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) ( fEncoders, fChildrenField ) ( gEncoders, gChildrenField ) leafEncode (Tree8 { data, children }) =
    case data of
        Data.BranchData b ->
            JE.object <|
                ( aChildrenField, JE.array <| Array.map (encode7 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) ( fEncoders, fChildrenField ) ( gEncoders, gChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode8 : DecoderConfig a b c d e f g h leaf path -> TreePath8 a b c d e f g h leaf -> Value
pathEncode8 config (TreePath8 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree8 : TreePath8 a b c d e f g h leaf -> Tree8 b c d e f g h leaf
getFocusedTree8 (TreePath8 { tree, path }) =
    getFocusedTree9 (TreePath9 { tree = tree, path = path })
        |> treeChildren9
        |> Array.get (Array.get 0 path |> unsafe "getFocusedTree8")
        |> unsafe "getFocusedTree8"


treeChildren8 : Tree8 a b c d e f g leaf -> Array (Tree7 b c d e f g leaf)
treeChildren8 (Tree8 { children }) =
    children


treeData8 : Tree8 a b c d e f g leaf -> Data a leaf
treeData8 (Tree8 { data }) =
    data


{-| -}
data8 : TreePath8 a b c d e f g h leaf -> Data b leaf
data8 =
    getFocusedTree8 >> treeData8


{-| -}
top8 : TreePath8 a b c d e f g h leaf -> TreePath9 a b c d e f g h leaf
top8 (TreePath8 { tree, path }) =
    TreePath9 { tree = tree, path = Array.empty }


{-| -}
offset8 : Int -> TreePath8 a b c d e f g h leaf -> Maybe (TreePath8 a b c d e f g h leaf)
offset8 dx ((TreePath8 { tree, path }) as treePath) =
    treePath
        |> up8
        |> Maybe.andThen (down9 <| (Array.get 0 path |> unsafe "offset8") + dx)


{-| -}
down8 : Int -> TreePath8 a b c d e f g h leaf -> Maybe (TreePath7 a b c d e f g h leaf)
down8 idx ((TreePath8 { tree, path }) as treePath) =
    getFocusedTree8 treePath
        |> treeChildren8
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath7 { tree = tree, path = Array.push idx path })


{-| -}
downs8 : TreePath8 a b c d e f g h leaf -> List (TreePath7 a b c d e f g h leaf)
downs8 ((TreePath8 { tree, path }) as treePath) =
    getFocusedTree8 treePath
        |> treeChildren8
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath7 { tree = tree, path = Array.push idx path })


{-| -}
up8 : TreePath8 a b c d e f g h leaf -> Maybe (TreePath9 a b c d e f g h leaf)
up8 (TreePath8 { tree, path }) =
    Just <| TreePath9 { tree = tree, path = Array.slice 0 -1 path }


decoder9 : ( Decoder a, String ) -> ( Decoder b, String ) -> ( Decoder c, String ) -> ( Decoder d, String ) -> ( Decoder e, String ) -> ( Decoder f, String ) -> ( Decoder g, String ) -> ( Decoder h, String ) -> Decoder leaf -> Decoder (Tree9 a b c d e f g h leaf)
decoder9 ( aDecoder, aChildrenField ) ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) ( dDecoder, dChildrenField ) ( eDecoder, eChildrenField ) ( fDecoder, fChildrenField ) ( gDecoder, gChildrenField ) ( hDecoder, hChildrenField ) leafDecoder =
    JD.oneOf
        [ JD.map2 (\data children -> Tree9 { data = data, children = children })
            (aDecoder |> JD.map Data.BranchData)
            (JD.field aChildrenField (JD.array <| decoder8 ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) ( dDecoder, dChildrenField ) ( eDecoder, eChildrenField ) ( fDecoder, fChildrenField ) ( gDecoder, gChildrenField ) ( hDecoder, hChildrenField ) leafDecoder))
        , JD.map (\data -> Tree9 { data = data, children = Array.empty })
            (leafDecoder |> JD.map Data.LeafData)
        ]


encode9 : ( a -> List ( String, Value ), String ) -> ( b -> List ( String, Value ), String ) -> ( c -> List ( String, Value ), String ) -> ( d -> List ( String, Value ), String ) -> ( e -> List ( String, Value ), String ) -> ( f -> List ( String, Value ), String ) -> ( g -> List ( String, Value ), String ) -> ( h -> List ( String, Value ), String ) -> (leaf -> Value) -> Tree9 a b c d e f g h leaf -> Value
encode9 ( aEncoders, aChildrenField ) ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) ( fEncoders, fChildrenField ) ( gEncoders, gChildrenField ) ( hEncoders, hChildrenField ) leafEncode (Tree9 { data, children }) =
    case data of
        Data.BranchData b ->
            JE.object <|
                ( aChildrenField, JE.array <| Array.map (encode8 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) ( fEncoders, fChildrenField ) ( gEncoders, gChildrenField ) ( hEncoders, hChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode9 : DecoderConfig a b c d e f g h leaf path -> TreePath9 a b c d e f g h leaf -> Value
pathEncode9 config (TreePath9 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree9 : TreePath9 a b c d e f g h leaf -> Tree9 a b c d e f g h leaf
getFocusedTree9 (TreePath9 { tree, path }) =
    tree


treeChildren9 : Tree9 a b c d e f g h leaf -> Array (Tree8 b c d e f g h leaf)
treeChildren9 (Tree9 { children }) =
    children


treeData9 : Tree9 a b c d e f g h leaf -> Data a leaf
treeData9 (Tree9 { data }) =
    data


{-| -}
data9 : TreePath9 a b c d e f g h leaf -> Data a leaf
data9 =
    getFocusedTree9 >> treeData9


{-| -}
top9 : TreePath9 a b c d e f g h leaf -> TreePath9 a b c d e f g h leaf
top9 (TreePath9 { tree, path }) =
    TreePath9 { tree = tree, path = Array.empty }


{-| -}
offset9 : Int -> TreePath9 a b c d e f g h leaf -> Maybe (TreePath9 a b c d e f g h leaf)
offset9 dx ((TreePath9 { tree, path }) as treePath) =
    Nothing


{-| -}
down9 : Int -> TreePath9 a b c d e f g h leaf -> Maybe (TreePath8 a b c d e f g h leaf)
down9 idx ((TreePath9 { tree, path }) as treePath) =
    getFocusedTree9 treePath
        |> treeChildren9
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath8 { tree = tree, path = Array.push idx path })


{-| -}
downs9 : TreePath9 a b c d e f g h leaf -> List (TreePath8 a b c d e f g h leaf)
downs9 ((TreePath9 { tree, path }) as treePath) =
    getFocusedTree9 treePath
        |> treeChildren9
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath8 { tree = tree, path = Array.push idx path })


{-| -}
up9 : TreePath9 a b c d e f g h leaf -> Maybe Never
up9 (TreePath9 { tree, path }) =
    Nothing


unsafe : String -> Maybe a -> a
unsafe msg maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.crash msg
