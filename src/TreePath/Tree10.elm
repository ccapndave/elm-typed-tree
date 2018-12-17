module TreePath.Tree10 exposing
    ( Tree, TreePath1, TreePath2, TreePath3, TreePath4, TreePath5, TreePath6, TreePath7, TreePath8, TreePath9, TreePath10
    , DecoderConfig, decoder, pathDecoder, pathEncode1, pathEncode2, pathEncode3, pathEncode4, pathEncode5, pathEncode6, pathEncode7, pathEncode8, pathEncode9, pathEncode10
    , toRootPath
    , data1, data2, data3, data4, data5, data6, data7, data8, data9, data10
    , top1, up1, offset1, down1, downs1, top2, up2, offset2, down2, downs2, top3, up3, offset3, down3, downs3, top4, up4, offset4, down4, downs4, top5, up5, offset5, down5, downs5, top6, up6, offset6, down6, downs6, top7, up7, offset7, down7, downs7, top8, up8, offset8, down8, downs8, top9, up9, offset9, down9, downs9, top10, up10, offset10, down10, downs10
    )

{-| This module provides types and functions for managing a strongly typed tree
of depth 10. Each level of the tree can have its own type, and each level can
contain Data either of that type, or the leaf type.


# Definition

@docs Tree, TreePath1, TreePath2, TreePath3, TreePath4, TreePath5, TreePath6, TreePath7, TreePath8, TreePath9, TreePath10


# Encoders and decoders

@docs DecoderConfig, decoder, pathDecoder, pathEncode1, pathEncode2, pathEncode3, pathEncode4, pathEncode5, pathEncode6, pathEncode7, pathEncode8, pathEncode9, pathEncode10


# Path constructor

@docs toRootPath


# Data

@docs data1, data2, data3, data4, data5, data6, data7, data8, data9, data10


# Navigation

@docs top1, up1, offset1, down1, downs1, top2, up2, offset2, down2, downs2, top3, up3, offset3, down3, downs3, top4, up4, offset4, down4, downs4, top5, up5, offset5, down5, downs5, top6, up6, offset6, down6, downs6, top7, up7, offset7, down7, downs7, top8, up8, offset8, down8, downs8, top9, up9, offset9, down9, downs9, top10, up10, offset10, down10, downs10

-}

import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TreePath.Data as Data exposing (Data)


{-| -}
type alias Tree a b c d e f g h i leaf =
    Tree10 a b c d e f g h i leaf


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


type Tree10 a b c d e f g h i leaf
    = Tree10
        { data : Data a leaf
        , children : Array (Tree9 b c d e f g h i leaf)
        }


{-| -}
type TreePath1 a b c d e f g h i leaf
    = TreePath1 (Tree1 leaf) Int (TreePath2 a b c d e f g h i leaf)


{-| -}
type TreePath2 a b c d e f g h i leaf
    = TreePath2 (Tree2 i leaf) Int (TreePath3 a b c d e f g h i leaf)


{-| -}
type TreePath3 a b c d e f g h i leaf
    = TreePath3 (Tree3 h i leaf) Int (TreePath4 a b c d e f g h i leaf)


{-| -}
type TreePath4 a b c d e f g h i leaf
    = TreePath4 (Tree4 g h i leaf) Int (TreePath5 a b c d e f g h i leaf)


{-| -}
type TreePath5 a b c d e f g h i leaf
    = TreePath5 (Tree5 f g h i leaf) Int (TreePath6 a b c d e f g h i leaf)


{-| -}
type TreePath6 a b c d e f g h i leaf
    = TreePath6 (Tree6 e f g h i leaf) Int (TreePath7 a b c d e f g h i leaf)


{-| -}
type TreePath7 a b c d e f g h i leaf
    = TreePath7 (Tree7 d e f g h i leaf) Int (TreePath8 a b c d e f g h i leaf)


{-| -}
type TreePath8 a b c d e f g h i leaf
    = TreePath8 (Tree8 c d e f g h i leaf) Int (TreePath9 a b c d e f g h i leaf)


{-| -}
type TreePath9 a b c d e f g h i leaf
    = TreePath9 (Tree9 b c d e f g h i leaf) Int (TreePath10 a b c d e f g h i leaf)


{-| -}
type TreePath10 a b c d e f g h i leaf
    = TreePath10 (Tree10 a b c d e f g h i leaf) () ()


{-| -}
type alias DecoderConfig a b c d e f g h i leaf path =
    { level10 :
        { decoder : Decoder a
        , encoders : a -> List ( String, Value )
        , pathType : TreePath10 a b c d e f g h i leaf -> path
        , childrenField : String
        }
    , level9 :
        { decoder : Decoder b
        , encoders : b -> List ( String, Value )
        , pathType : TreePath9 a b c d e f g h i leaf -> path
        , childrenField : String
        }
    , level8 :
        { decoder : Decoder c
        , encoders : c -> List ( String, Value )
        , pathType : TreePath8 a b c d e f g h i leaf -> path
        , childrenField : String
        }
    , level7 :
        { decoder : Decoder d
        , encoders : d -> List ( String, Value )
        , pathType : TreePath7 a b c d e f g h i leaf -> path
        , childrenField : String
        }
    , level6 :
        { decoder : Decoder e
        , encoders : e -> List ( String, Value )
        , pathType : TreePath6 a b c d e f g h i leaf -> path
        , childrenField : String
        }
    , level5 :
        { decoder : Decoder f
        , encoders : f -> List ( String, Value )
        , pathType : TreePath5 a b c d e f g h i leaf -> path
        , childrenField : String
        }
    , level4 :
        { decoder : Decoder g
        , encoders : g -> List ( String, Value )
        , pathType : TreePath4 a b c d e f g h i leaf -> path
        , childrenField : String
        }
    , level3 :
        { decoder : Decoder h
        , encoders : h -> List ( String, Value )
        , pathType : TreePath3 a b c d e f g h i leaf -> path
        , childrenField : String
        }
    , level2 :
        { decoder : Decoder i
        , encoders : i -> List ( String, Value )
        , pathType : TreePath2 a b c d e f g h i leaf -> path
        , childrenField : String
        }
    , leaf :
        { decoder : Decoder leaf
        , encode : leaf -> Value
        , pathType : TreePath1 a b c d e f g h i leaf -> path
        }
    }


{-| -}
decoder : DecoderConfig a b c d e f g h i leaf path -> Decoder (Tree10 a b c d e f g h i leaf)
decoder config =
    decoder10
        ( config.level10.decoder, config.level10.childrenField )
        ( config.level9.decoder, config.level9.childrenField )
        ( config.level8.decoder, config.level8.childrenField )
        ( config.level7.decoder, config.level7.childrenField )
        ( config.level6.decoder, config.level6.childrenField )
        ( config.level5.decoder, config.level5.childrenField )
        ( config.level4.decoder, config.level4.childrenField )
        ( config.level3.decoder, config.level3.childrenField )
        ( config.level2.decoder, config.level2.childrenField )
        config.leaf.decoder


encode : DecoderConfig a b c d e f g h i leaf path -> Tree10 a b c d e f g h i leaf -> Value
encode config tree =
    encode10
        ( config.level10.encoders, config.level10.childrenField )
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
pathDecoder : DecoderConfig a b c d e f g h i leaf path -> Decoder path
pathDecoder config =
    JD.field "tree" (decoder config)
        |> JD.andThen
            (\tree ->
                JD.field "path" (JD.list JD.int)
                    |> JD.andThen
                        (\path ->
                            case path of
                                [] ->
                                    Just (TreePath10 tree () ())
                                        |> Maybe.map config.level10.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1 ] ->
                                    Just (TreePath10 tree () ())
                                        |> Maybe.andThen (down10 b1)
                                        |> Maybe.map config.level9.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1, b2 ] ->
                                    Just (TreePath10 tree () ())
                                        |> Maybe.andThen (down10 b1)
                                        |> Maybe.andThen (down9 b2)
                                        |> Maybe.map config.level8.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1, b2, b3 ] ->
                                    Just (TreePath10 tree () ())
                                        |> Maybe.andThen (down10 b1)
                                        |> Maybe.andThen (down9 b2)
                                        |> Maybe.andThen (down8 b3)
                                        |> Maybe.map config.level7.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1, b2, b3, b4 ] ->
                                    Just (TreePath10 tree () ())
                                        |> Maybe.andThen (down10 b1)
                                        |> Maybe.andThen (down9 b2)
                                        |> Maybe.andThen (down8 b3)
                                        |> Maybe.andThen (down7 b4)
                                        |> Maybe.map config.level6.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1, b2, b3, b4, b5 ] ->
                                    Just (TreePath10 tree () ())
                                        |> Maybe.andThen (down10 b1)
                                        |> Maybe.andThen (down9 b2)
                                        |> Maybe.andThen (down8 b3)
                                        |> Maybe.andThen (down7 b4)
                                        |> Maybe.andThen (down6 b5)
                                        |> Maybe.map config.level5.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1, b2, b3, b4, b5, b6 ] ->
                                    Just (TreePath10 tree () ())
                                        |> Maybe.andThen (down10 b1)
                                        |> Maybe.andThen (down9 b2)
                                        |> Maybe.andThen (down8 b3)
                                        |> Maybe.andThen (down7 b4)
                                        |> Maybe.andThen (down6 b5)
                                        |> Maybe.andThen (down5 b6)
                                        |> Maybe.map config.level4.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1, b2, b3, b4, b5, b6, b7 ] ->
                                    Just (TreePath10 tree () ())
                                        |> Maybe.andThen (down10 b1)
                                        |> Maybe.andThen (down9 b2)
                                        |> Maybe.andThen (down8 b3)
                                        |> Maybe.andThen (down7 b4)
                                        |> Maybe.andThen (down6 b5)
                                        |> Maybe.andThen (down5 b6)
                                        |> Maybe.andThen (down4 b7)
                                        |> Maybe.map config.level3.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1, b2, b3, b4, b5, b6, b7, b8 ] ->
                                    Just (TreePath10 tree () ())
                                        |> Maybe.andThen (down10 b1)
                                        |> Maybe.andThen (down9 b2)
                                        |> Maybe.andThen (down8 b3)
                                        |> Maybe.andThen (down7 b4)
                                        |> Maybe.andThen (down6 b5)
                                        |> Maybe.andThen (down5 b6)
                                        |> Maybe.andThen (down4 b7)
                                        |> Maybe.andThen (down3 b8)
                                        |> Maybe.map config.level2.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1, b2, b3, b4, b5, b6, b7, b8, b9 ] ->
                                    Just (TreePath10 tree () ())
                                        |> Maybe.andThen (down10 b1)
                                        |> Maybe.andThen (down9 b2)
                                        |> Maybe.andThen (down8 b3)
                                        |> Maybe.andThen (down7 b4)
                                        |> Maybe.andThen (down6 b5)
                                        |> Maybe.andThen (down5 b6)
                                        |> Maybe.andThen (down4 b7)
                                        |> Maybe.andThen (down3 b8)
                                        |> Maybe.andThen (down2 b9)
                                        |> Maybe.map config.leaf.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                otherwise ->
                                    JD.fail <| "Illegal path length " ++ String.fromInt (List.length path)
                        )
            )


{-| -}
toRootPath : Tree10 a b c d e f g h i leaf -> TreePath10 a b c d e f g h i leaf
toRootPath tree =
    TreePath10 tree () ()


decoder1 : Decoder leaf -> Decoder (Tree1 leaf)
decoder1 leafDecoder =
    leafDecoder
        |> JD.map (\data -> Tree1 { data = data })


encode1 : (leaf -> Value) -> Tree1 leaf -> Value
encode1 leafEncode (Tree1 { data }) =
    leafEncode data


{-| -}
pathEncode1 : DecoderConfig a b c d e f g h i leaf path -> TreePath1 a b c d e f g h i leaf -> Value
pathEncode1 config ((TreePath1 tree1 idx1 ((TreePath2 tree2 idx2 ((TreePath3 tree3 idx3 ((TreePath4 tree4 idx4 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5)) as treePath4)) as treePath3)) as treePath2)) as treePath1) =
    JE.object
        [ ( "tree", tree10 |> encode config )
        , ( "path", [ idx9, idx8, idx7, idx6, idx5, idx4, idx3, idx2, idx1 ] |> JE.list JE.int )
        ]


{-| -}
data1 : TreePath1 a b c d e f g h i leaf -> leaf
data1 (TreePath1 (Tree1 { data }) _ _) =
    data


{-| -}
top1 : TreePath1 a b c d e f g h i leaf -> TreePath10 a b c d e f g h i leaf
top1 ((TreePath1 tree1 idx1 ((TreePath2 tree2 idx2 ((TreePath3 tree3 idx3 ((TreePath4 tree4 idx4 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5)) as treePath4)) as treePath3)) as treePath2)) as treePath1) =
    treePath10


{-| -}
offset1 : Int -> TreePath1 a b c d e f g h i leaf -> Maybe (TreePath1 a b c d e f g h i leaf)
offset1 dx (TreePath1 _ idx parentPath) =
    parentPath
        |> down2 (idx + dx)


{-| -}
down1 : Int -> TreePath1 a b c d e f g h i leaf -> Maybe Never
down1 _ _ =
    Nothing


{-| -}
downs1 : TreePath1 a b c d e f g h i leaf -> List Never
downs1 _ =
    []


{-| -}
up1 : TreePath1 a b c d e f g h i leaf -> Maybe (TreePath2 a b c d e f g h i leaf)
up1 ((TreePath1 tree1 idx1 ((TreePath2 tree2 idx2 ((TreePath3 tree3 idx3 ((TreePath4 tree4 idx4 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5)) as treePath4)) as treePath3)) as treePath2)) as treePath1) =
    Just treePath2


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
                ( aChildrenField, JE.array (encode1 leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode2 : DecoderConfig a b c d e f g h i leaf path -> TreePath2 a b c d e f g h i leaf -> Value
pathEncode2 config ((TreePath2 tree2 idx2 ((TreePath3 tree3 idx3 ((TreePath4 tree4 idx4 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5)) as treePath4)) as treePath3)) as treePath2) =
    JE.object
        [ ( "tree", tree10 |> encode config )
        , ( "path", [ idx9, idx8, idx7, idx6, idx5, idx4, idx3, idx2 ] |> JE.list JE.int )
        ]


{-| -}
data2 : TreePath2 a b c d e f g h i leaf -> Data i leaf
data2 (TreePath2 (Tree2 { data }) _ _) =
    data


{-| -}
top2 : TreePath2 a b c d e f g h i leaf -> TreePath10 a b c d e f g h i leaf
top2 ((TreePath2 tree2 idx2 ((TreePath3 tree3 idx3 ((TreePath4 tree4 idx4 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5)) as treePath4)) as treePath3)) as treePath2) =
    treePath10


{-| -}
offset2 : Int -> TreePath2 a b c d e f g h i leaf -> Maybe (TreePath2 a b c d e f g h i leaf)
offset2 dx (TreePath2 _ idx parentPath) =
    parentPath
        |> down3 (idx + dx)


{-| -}
down2 : Int -> TreePath2 a b c d e f g h i leaf -> Maybe (TreePath1 a b c d e f g h i leaf)
down2 idx ((TreePath2 ((Tree2 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath1 childTree idx treePath)


{-| -}
downs2 : TreePath2 a b c d e f g h i leaf -> List (TreePath1 a b c d e f g h i leaf)
downs2 ((TreePath2 ((Tree2 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down2 idx treePath)


{-| -}
up2 : TreePath2 a b c d e f g h i leaf -> Maybe (TreePath3 a b c d e f g h i leaf)
up2 ((TreePath2 tree2 idx2 ((TreePath3 tree3 idx3 ((TreePath4 tree4 idx4 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5)) as treePath4)) as treePath3)) as treePath2) =
    Just treePath3


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
                ( aChildrenField, JE.array (encode2 ( bEncoders, bChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode3 : DecoderConfig a b c d e f g h i leaf path -> TreePath3 a b c d e f g h i leaf -> Value
pathEncode3 config ((TreePath3 tree3 idx3 ((TreePath4 tree4 idx4 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5)) as treePath4)) as treePath3) =
    JE.object
        [ ( "tree", tree10 |> encode config )
        , ( "path", [ idx9, idx8, idx7, idx6, idx5, idx4, idx3 ] |> JE.list JE.int )
        ]


{-| -}
data3 : TreePath3 a b c d e f g h i leaf -> Data h leaf
data3 (TreePath3 (Tree3 { data }) _ _) =
    data


{-| -}
top3 : TreePath3 a b c d e f g h i leaf -> TreePath10 a b c d e f g h i leaf
top3 ((TreePath3 tree3 idx3 ((TreePath4 tree4 idx4 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5)) as treePath4)) as treePath3) =
    treePath10


{-| -}
offset3 : Int -> TreePath3 a b c d e f g h i leaf -> Maybe (TreePath3 a b c d e f g h i leaf)
offset3 dx (TreePath3 _ idx parentPath) =
    parentPath
        |> down4 (idx + dx)


{-| -}
down3 : Int -> TreePath3 a b c d e f g h i leaf -> Maybe (TreePath2 a b c d e f g h i leaf)
down3 idx ((TreePath3 ((Tree3 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath2 childTree idx treePath)


{-| -}
downs3 : TreePath3 a b c d e f g h i leaf -> List (TreePath2 a b c d e f g h i leaf)
downs3 ((TreePath3 ((Tree3 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down3 idx treePath)


{-| -}
up3 : TreePath3 a b c d e f g h i leaf -> Maybe (TreePath4 a b c d e f g h i leaf)
up3 ((TreePath3 tree3 idx3 ((TreePath4 tree4 idx4 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5)) as treePath4)) as treePath3) =
    Just treePath4


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
                ( aChildrenField, JE.array (encode3 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode4 : DecoderConfig a b c d e f g h i leaf path -> TreePath4 a b c d e f g h i leaf -> Value
pathEncode4 config ((TreePath4 tree4 idx4 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5)) as treePath4) =
    JE.object
        [ ( "tree", tree10 |> encode config )
        , ( "path", [ idx9, idx8, idx7, idx6, idx5, idx4 ] |> JE.list JE.int )
        ]


{-| -}
data4 : TreePath4 a b c d e f g h i leaf -> Data g leaf
data4 (TreePath4 (Tree4 { data }) _ _) =
    data


{-| -}
top4 : TreePath4 a b c d e f g h i leaf -> TreePath10 a b c d e f g h i leaf
top4 ((TreePath4 tree4 idx4 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5)) as treePath4) =
    treePath10


{-| -}
offset4 : Int -> TreePath4 a b c d e f g h i leaf -> Maybe (TreePath4 a b c d e f g h i leaf)
offset4 dx (TreePath4 _ idx parentPath) =
    parentPath
        |> down5 (idx + dx)


{-| -}
down4 : Int -> TreePath4 a b c d e f g h i leaf -> Maybe (TreePath3 a b c d e f g h i leaf)
down4 idx ((TreePath4 ((Tree4 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath3 childTree idx treePath)


{-| -}
downs4 : TreePath4 a b c d e f g h i leaf -> List (TreePath3 a b c d e f g h i leaf)
downs4 ((TreePath4 ((Tree4 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down4 idx treePath)


{-| -}
up4 : TreePath4 a b c d e f g h i leaf -> Maybe (TreePath5 a b c d e f g h i leaf)
up4 ((TreePath4 tree4 idx4 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5)) as treePath4) =
    Just treePath5


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
                ( aChildrenField, JE.array (encode4 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode5 : DecoderConfig a b c d e f g h i leaf path -> TreePath5 a b c d e f g h i leaf -> Value
pathEncode5 config ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5) =
    JE.object
        [ ( "tree", tree10 |> encode config )
        , ( "path", [ idx9, idx8, idx7, idx6, idx5 ] |> JE.list JE.int )
        ]


{-| -}
data5 : TreePath5 a b c d e f g h i leaf -> Data f leaf
data5 (TreePath5 (Tree5 { data }) _ _) =
    data


{-| -}
top5 : TreePath5 a b c d e f g h i leaf -> TreePath10 a b c d e f g h i leaf
top5 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5) =
    treePath10


{-| -}
offset5 : Int -> TreePath5 a b c d e f g h i leaf -> Maybe (TreePath5 a b c d e f g h i leaf)
offset5 dx (TreePath5 _ idx parentPath) =
    parentPath
        |> down6 (idx + dx)


{-| -}
down5 : Int -> TreePath5 a b c d e f g h i leaf -> Maybe (TreePath4 a b c d e f g h i leaf)
down5 idx ((TreePath5 ((Tree5 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath4 childTree idx treePath)


{-| -}
downs5 : TreePath5 a b c d e f g h i leaf -> List (TreePath4 a b c d e f g h i leaf)
downs5 ((TreePath5 ((Tree5 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down5 idx treePath)


{-| -}
up5 : TreePath5 a b c d e f g h i leaf -> Maybe (TreePath6 a b c d e f g h i leaf)
up5 ((TreePath5 tree5 idx5 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6)) as treePath5) =
    Just treePath6


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
                ( aChildrenField, JE.array (encode5 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode6 : DecoderConfig a b c d e f g h i leaf path -> TreePath6 a b c d e f g h i leaf -> Value
pathEncode6 config ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6) =
    JE.object
        [ ( "tree", tree10 |> encode config )
        , ( "path", [ idx9, idx8, idx7, idx6 ] |> JE.list JE.int )
        ]


{-| -}
data6 : TreePath6 a b c d e f g h i leaf -> Data e leaf
data6 (TreePath6 (Tree6 { data }) _ _) =
    data


{-| -}
top6 : TreePath6 a b c d e f g h i leaf -> TreePath10 a b c d e f g h i leaf
top6 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6) =
    treePath10


{-| -}
offset6 : Int -> TreePath6 a b c d e f g h i leaf -> Maybe (TreePath6 a b c d e f g h i leaf)
offset6 dx (TreePath6 _ idx parentPath) =
    parentPath
        |> down7 (idx + dx)


{-| -}
down6 : Int -> TreePath6 a b c d e f g h i leaf -> Maybe (TreePath5 a b c d e f g h i leaf)
down6 idx ((TreePath6 ((Tree6 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath5 childTree idx treePath)


{-| -}
downs6 : TreePath6 a b c d e f g h i leaf -> List (TreePath5 a b c d e f g h i leaf)
downs6 ((TreePath6 ((Tree6 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down6 idx treePath)


{-| -}
up6 : TreePath6 a b c d e f g h i leaf -> Maybe (TreePath7 a b c d e f g h i leaf)
up6 ((TreePath6 tree6 idx6 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7)) as treePath6) =
    Just treePath7


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
                ( aChildrenField, JE.array (encode6 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) ( fEncoders, fChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode7 : DecoderConfig a b c d e f g h i leaf path -> TreePath7 a b c d e f g h i leaf -> Value
pathEncode7 config ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7) =
    JE.object
        [ ( "tree", tree10 |> encode config )
        , ( "path", [ idx9, idx8, idx7 ] |> JE.list JE.int )
        ]


{-| -}
data7 : TreePath7 a b c d e f g h i leaf -> Data d leaf
data7 (TreePath7 (Tree7 { data }) _ _) =
    data


{-| -}
top7 : TreePath7 a b c d e f g h i leaf -> TreePath10 a b c d e f g h i leaf
top7 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7) =
    treePath10


{-| -}
offset7 : Int -> TreePath7 a b c d e f g h i leaf -> Maybe (TreePath7 a b c d e f g h i leaf)
offset7 dx (TreePath7 _ idx parentPath) =
    parentPath
        |> down8 (idx + dx)


{-| -}
down7 : Int -> TreePath7 a b c d e f g h i leaf -> Maybe (TreePath6 a b c d e f g h i leaf)
down7 idx ((TreePath7 ((Tree7 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath6 childTree idx treePath)


{-| -}
downs7 : TreePath7 a b c d e f g h i leaf -> List (TreePath6 a b c d e f g h i leaf)
downs7 ((TreePath7 ((Tree7 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down7 idx treePath)


{-| -}
up7 : TreePath7 a b c d e f g h i leaf -> Maybe (TreePath8 a b c d e f g h i leaf)
up7 ((TreePath7 tree7 idx7 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8)) as treePath7) =
    Just treePath8


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
                ( aChildrenField, JE.array (encode7 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) ( fEncoders, fChildrenField ) ( gEncoders, gChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode8 : DecoderConfig a b c d e f g h i leaf path -> TreePath8 a b c d e f g h i leaf -> Value
pathEncode8 config ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8) =
    JE.object
        [ ( "tree", tree10 |> encode config )
        , ( "path", [ idx9, idx8 ] |> JE.list JE.int )
        ]


{-| -}
data8 : TreePath8 a b c d e f g h i leaf -> Data c leaf
data8 (TreePath8 (Tree8 { data }) _ _) =
    data


{-| -}
top8 : TreePath8 a b c d e f g h i leaf -> TreePath10 a b c d e f g h i leaf
top8 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8) =
    treePath10


{-| -}
offset8 : Int -> TreePath8 a b c d e f g h i leaf -> Maybe (TreePath8 a b c d e f g h i leaf)
offset8 dx (TreePath8 _ idx parentPath) =
    parentPath
        |> down9 (idx + dx)


{-| -}
down8 : Int -> TreePath8 a b c d e f g h i leaf -> Maybe (TreePath7 a b c d e f g h i leaf)
down8 idx ((TreePath8 ((Tree8 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath7 childTree idx treePath)


{-| -}
downs8 : TreePath8 a b c d e f g h i leaf -> List (TreePath7 a b c d e f g h i leaf)
downs8 ((TreePath8 ((Tree8 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down8 idx treePath)


{-| -}
up8 : TreePath8 a b c d e f g h i leaf -> Maybe (TreePath9 a b c d e f g h i leaf)
up8 ((TreePath8 tree8 idx8 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9)) as treePath8) =
    Just treePath9


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
                ( aChildrenField, JE.array (encode8 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) ( fEncoders, fChildrenField ) ( gEncoders, gChildrenField ) ( hEncoders, hChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode9 : DecoderConfig a b c d e f g h i leaf path -> TreePath9 a b c d e f g h i leaf -> Value
pathEncode9 config ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9) =
    JE.object
        [ ( "tree", tree10 |> encode config )
        , ( "path", [ idx9 ] |> JE.list JE.int )
        ]


{-| -}
data9 : TreePath9 a b c d e f g h i leaf -> Data b leaf
data9 (TreePath9 (Tree9 { data }) _ _) =
    data


{-| -}
top9 : TreePath9 a b c d e f g h i leaf -> TreePath10 a b c d e f g h i leaf
top9 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9) =
    treePath10


{-| -}
offset9 : Int -> TreePath9 a b c d e f g h i leaf -> Maybe (TreePath9 a b c d e f g h i leaf)
offset9 dx (TreePath9 _ idx parentPath) =
    parentPath
        |> down10 (idx + dx)


{-| -}
down9 : Int -> TreePath9 a b c d e f g h i leaf -> Maybe (TreePath8 a b c d e f g h i leaf)
down9 idx ((TreePath9 ((Tree9 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath8 childTree idx treePath)


{-| -}
downs9 : TreePath9 a b c d e f g h i leaf -> List (TreePath8 a b c d e f g h i leaf)
downs9 ((TreePath9 ((Tree9 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down9 idx treePath)


{-| -}
up9 : TreePath9 a b c d e f g h i leaf -> Maybe (TreePath10 a b c d e f g h i leaf)
up9 ((TreePath9 tree9 idx9 ((TreePath10 tree10 _ _) as treePath10)) as treePath9) =
    Just treePath10


decoder10 : ( Decoder a, String ) -> ( Decoder b, String ) -> ( Decoder c, String ) -> ( Decoder d, String ) -> ( Decoder e, String ) -> ( Decoder f, String ) -> ( Decoder g, String ) -> ( Decoder h, String ) -> ( Decoder i, String ) -> Decoder leaf -> Decoder (Tree10 a b c d e f g h i leaf)
decoder10 ( aDecoder, aChildrenField ) ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) ( dDecoder, dChildrenField ) ( eDecoder, eChildrenField ) ( fDecoder, fChildrenField ) ( gDecoder, gChildrenField ) ( hDecoder, hChildrenField ) ( iDecoder, iChildrenField ) leafDecoder =
    JD.oneOf
        [ JD.map2 (\data children -> Tree10 { data = data, children = children })
            (aDecoder |> JD.map Data.BranchData)
            (JD.field aChildrenField (JD.array <| decoder9 ( bDecoder, bChildrenField ) ( cDecoder, cChildrenField ) ( dDecoder, dChildrenField ) ( eDecoder, eChildrenField ) ( fDecoder, fChildrenField ) ( gDecoder, gChildrenField ) ( hDecoder, hChildrenField ) ( iDecoder, iChildrenField ) leafDecoder))
        , JD.map (\data -> Tree10 { data = data, children = Array.empty })
            (leafDecoder |> JD.map Data.LeafData)
        ]


encode10 : ( a -> List ( String, Value ), String ) -> ( b -> List ( String, Value ), String ) -> ( c -> List ( String, Value ), String ) -> ( d -> List ( String, Value ), String ) -> ( e -> List ( String, Value ), String ) -> ( f -> List ( String, Value ), String ) -> ( g -> List ( String, Value ), String ) -> ( h -> List ( String, Value ), String ) -> ( i -> List ( String, Value ), String ) -> (leaf -> Value) -> Tree10 a b c d e f g h i leaf -> Value
encode10 ( aEncoders, aChildrenField ) ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) ( fEncoders, fChildrenField ) ( gEncoders, gChildrenField ) ( hEncoders, hChildrenField ) ( iEncoders, iChildrenField ) leafEncode (Tree10 { data, children }) =
    case data of
        Data.BranchData b ->
            JE.object <|
                ( aChildrenField, JE.array (encode9 ( bEncoders, bChildrenField ) ( cEncoders, cChildrenField ) ( dEncoders, dChildrenField ) ( eEncoders, eChildrenField ) ( fEncoders, fChildrenField ) ( gEncoders, gChildrenField ) ( hEncoders, hChildrenField ) ( iEncoders, iChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


{-| -}
pathEncode10 : DecoderConfig a b c d e f g h i leaf path -> TreePath10 a b c d e f g h i leaf -> Value
pathEncode10 config ((TreePath10 tree10 _ _) as treePath10) =
    JE.object
        [ ( "tree", tree10 |> encode config )
        , ( "path", [] |> JE.list JE.int )
        ]


{-| -}
data10 : TreePath10 a b c d e f g h i leaf -> Data a leaf
data10 (TreePath10 (Tree10 { data }) _ _) =
    data


{-| -}
top10 : TreePath10 a b c d e f g h i leaf -> TreePath10 a b c d e f g h i leaf
top10 ((TreePath10 tree10 _ _) as treePath10) =
    treePath10


{-| -}
offset10 : Int -> TreePath10 a b c d e f g h i leaf -> Maybe (TreePath10 a b c d e f g h i leaf)
offset10 dx (TreePath10 _ idx parentPath) =
    Nothing


{-| -}
down10 : Int -> TreePath10 a b c d e f g h i leaf -> Maybe (TreePath9 a b c d e f g h i leaf)
down10 idx ((TreePath10 ((Tree10 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath9 childTree idx treePath)


{-| -}
downs10 : TreePath10 a b c d e f g h i leaf -> List (TreePath9 a b c d e f g h i leaf)
downs10 ((TreePath10 ((Tree10 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down10 idx treePath)


{-| -}
up10 : TreePath10 a b c d e f g h i leaf -> Maybe Never
up10 ((TreePath10 tree10 _ _) as treePath10) =
    Nothing
