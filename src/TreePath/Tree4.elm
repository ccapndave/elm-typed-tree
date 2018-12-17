module TreePath.Tree4 exposing
    ( Tree, TreePath1, TreePath2, TreePath3, TreePath4
    , DecoderConfig, decoder, pathDecoder, pathEncode1, pathEncode2, pathEncode3, pathEncode4
    , toRootPath
    , data1, data2, data3, data4
    , top1, up1, offset1, down1, downs1, top2, up2, offset2, down2, downs2, top3, up3, offset3, down3, downs3, top4, up4, offset4, down4, downs4
    )

{-| This module provides types and functions for managing a strongly typed tree
of depth 4. Each level of the tree can have its own type, and each level can
contain Data either of that type, or the leaf type.


# Definition

@docs Tree, TreePath1, TreePath2, TreePath3, TreePath4


# Encoders and decoders

@docs DecoderConfig, decoder, pathDecoder, pathEncode1, pathEncode2, pathEncode3, pathEncode4


# Path constructor

@docs toRootPath


# Data

@docs data1, data2, data3, data4


# Navigation

@docs top1, up1, offset1, down1, downs1, top2, up2, offset2, down2, downs2, top3, up3, offset3, down3, downs3, top4, up4, offset4, down4, downs4

-}

import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TreePath.Data as Data exposing (Data)


{-| -}
type alias Tree a b c leaf =
    Tree4 a b c leaf


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


{-| -}
type TreePath1 a b c leaf
    = TreePath1 (Tree1 leaf) Int (TreePath2 a b c leaf)


{-| -}
type TreePath2 a b c leaf
    = TreePath2 (Tree2 c leaf) Int (TreePath3 a b c leaf)


{-| -}
type TreePath3 a b c leaf
    = TreePath3 (Tree3 b c leaf) Int (TreePath4 a b c leaf)


{-| -}
type TreePath4 a b c leaf
    = TreePath4 (Tree4 a b c leaf) () ()


{-| -}
type alias DecoderConfig a b c leaf path =
    { level4 :
        { decoder : Decoder a
        , encoders : a -> List ( String, Value )
        , pathType : TreePath4 a b c leaf -> path
        , childrenField : String
        }
    , level3 :
        { decoder : Decoder b
        , encoders : b -> List ( String, Value )
        , pathType : TreePath3 a b c leaf -> path
        , childrenField : String
        }
    , level2 :
        { decoder : Decoder c
        , encoders : c -> List ( String, Value )
        , pathType : TreePath2 a b c leaf -> path
        , childrenField : String
        }
    , leaf :
        { decoder : Decoder leaf
        , encode : leaf -> Value
        , pathType : TreePath1 a b c leaf -> path
        }
    }


{-| -}
decoder : DecoderConfig a b c leaf path -> Decoder (Tree4 a b c leaf)
decoder config =
    decoder4
        ( config.level4.decoder, config.level4.childrenField )
        ( config.level3.decoder, config.level3.childrenField )
        ( config.level2.decoder, config.level2.childrenField )
        config.leaf.decoder


encode : DecoderConfig a b c leaf path -> Tree4 a b c leaf -> Value
encode config tree =
    encode4
        ( config.level4.encoders, config.level4.childrenField )
        ( config.level3.encoders, config.level3.childrenField )
        ( config.level2.encoders, config.level2.childrenField )
        config.leaf.encode
        tree


{-| -}
pathDecoder : DecoderConfig a b c leaf path -> Decoder path
pathDecoder config =
    JD.field "tree" (decoder config)
        |> JD.andThen
            (\tree ->
                JD.field "path" (JD.list JD.int)
                    |> JD.andThen
                        (\path ->
                            case path of
                                [] ->
                                    Just (TreePath4 tree () ())
                                        |> Maybe.map config.level4.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1 ] ->
                                    Just (TreePath4 tree () ())
                                        |> Maybe.andThen (down4 b1)
                                        |> Maybe.map config.level3.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1, b2 ] ->
                                    Just (TreePath4 tree () ())
                                        |> Maybe.andThen (down4 b1)
                                        |> Maybe.andThen (down3 b2)
                                        |> Maybe.map config.level2.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1, b2, b3 ] ->
                                    Just (TreePath4 tree () ())
                                        |> Maybe.andThen (down4 b1)
                                        |> Maybe.andThen (down3 b2)
                                        |> Maybe.andThen (down2 b3)
                                        |> Maybe.map config.leaf.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                otherwise ->
                                    JD.fail <| "Illegal path length " ++ String.fromInt (List.length path)
                        )
            )


{-| -}
toRootPath : Tree4 a b c leaf -> TreePath4 a b c leaf
toRootPath tree =
    TreePath4 tree () ()


decoder1 : Decoder leaf -> Decoder (Tree1 leaf)
decoder1 leafDecoder =
    leafDecoder
        |> JD.map (\data -> Tree1 { data = data })


encode1 : (leaf -> Value) -> Tree1 leaf -> Value
encode1 leafEncode (Tree1 { data }) =
    leafEncode data


{-| -}
pathEncode1 : DecoderConfig a b c leaf path -> TreePath1 a b c leaf -> Value
pathEncode1 config ((TreePath1 tree1 idx1 ((TreePath2 tree2 idx2 ((TreePath3 tree3 idx3 ((TreePath4 tree4 _ _) as treePath4)) as treePath3)) as treePath2)) as treePath1) =
    JE.object
        [ ( "tree", tree4 |> encode config )
        , ( "path", [ idx3, idx2, idx1 ] |> JE.list JE.int )
        ]


{-| -}
data1 : TreePath1 a b c leaf -> leaf
data1 (TreePath1 (Tree1 { data }) _ _) =
    data


{-| -}
top1 : TreePath1 a b c leaf -> TreePath4 a b c leaf
top1 ((TreePath1 tree1 idx1 ((TreePath2 tree2 idx2 ((TreePath3 tree3 idx3 ((TreePath4 tree4 _ _) as treePath4)) as treePath3)) as treePath2)) as treePath1) =
    treePath4


{-| -}
offset1 : Int -> TreePath1 a b c leaf -> Maybe (TreePath1 a b c leaf)
offset1 dx (TreePath1 _ idx parentPath) =
    parentPath
        |> down2 (idx + dx)


{-| -}
down1 : Int -> TreePath1 a b c leaf -> Maybe Never
down1 _ _ =
    Nothing


{-| -}
downs1 : TreePath1 a b c leaf -> List Never
downs1 _ =
    []


{-| -}
up1 : TreePath1 a b c leaf -> Maybe (TreePath2 a b c leaf)
up1 ((TreePath1 tree1 idx1 ((TreePath2 tree2 idx2 ((TreePath3 tree3 idx3 ((TreePath4 tree4 _ _) as treePath4)) as treePath3)) as treePath2)) as treePath1) =
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
pathEncode2 : DecoderConfig a b c leaf path -> TreePath2 a b c leaf -> Value
pathEncode2 config ((TreePath2 tree2 idx2 ((TreePath3 tree3 idx3 ((TreePath4 tree4 _ _) as treePath4)) as treePath3)) as treePath2) =
    JE.object
        [ ( "tree", tree4 |> encode config )
        , ( "path", [ idx3, idx2 ] |> JE.list JE.int )
        ]


{-| -}
data2 : TreePath2 a b c leaf -> Data c leaf
data2 (TreePath2 (Tree2 { data }) _ _) =
    data


{-| -}
top2 : TreePath2 a b c leaf -> TreePath4 a b c leaf
top2 ((TreePath2 tree2 idx2 ((TreePath3 tree3 idx3 ((TreePath4 tree4 _ _) as treePath4)) as treePath3)) as treePath2) =
    treePath4


{-| -}
offset2 : Int -> TreePath2 a b c leaf -> Maybe (TreePath2 a b c leaf)
offset2 dx (TreePath2 _ idx parentPath) =
    parentPath
        |> down3 (idx + dx)


{-| -}
down2 : Int -> TreePath2 a b c leaf -> Maybe (TreePath1 a b c leaf)
down2 idx ((TreePath2 ((Tree2 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath1 childTree idx treePath)


{-| -}
downs2 : TreePath2 a b c leaf -> List (TreePath1 a b c leaf)
downs2 ((TreePath2 ((Tree2 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down2 idx treePath)


{-| -}
up2 : TreePath2 a b c leaf -> Maybe (TreePath3 a b c leaf)
up2 ((TreePath2 tree2 idx2 ((TreePath3 tree3 idx3 ((TreePath4 tree4 _ _) as treePath4)) as treePath3)) as treePath2) =
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
pathEncode3 : DecoderConfig a b c leaf path -> TreePath3 a b c leaf -> Value
pathEncode3 config ((TreePath3 tree3 idx3 ((TreePath4 tree4 _ _) as treePath4)) as treePath3) =
    JE.object
        [ ( "tree", tree4 |> encode config )
        , ( "path", [ idx3 ] |> JE.list JE.int )
        ]


{-| -}
data3 : TreePath3 a b c leaf -> Data b leaf
data3 (TreePath3 (Tree3 { data }) _ _) =
    data


{-| -}
top3 : TreePath3 a b c leaf -> TreePath4 a b c leaf
top3 ((TreePath3 tree3 idx3 ((TreePath4 tree4 _ _) as treePath4)) as treePath3) =
    treePath4


{-| -}
offset3 : Int -> TreePath3 a b c leaf -> Maybe (TreePath3 a b c leaf)
offset3 dx (TreePath3 _ idx parentPath) =
    parentPath
        |> down4 (idx + dx)


{-| -}
down3 : Int -> TreePath3 a b c leaf -> Maybe (TreePath2 a b c leaf)
down3 idx ((TreePath3 ((Tree3 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath2 childTree idx treePath)


{-| -}
downs3 : TreePath3 a b c leaf -> List (TreePath2 a b c leaf)
downs3 ((TreePath3 ((Tree3 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down3 idx treePath)


{-| -}
up3 : TreePath3 a b c leaf -> Maybe (TreePath4 a b c leaf)
up3 ((TreePath3 tree3 idx3 ((TreePath4 tree4 _ _) as treePath4)) as treePath3) =
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
pathEncode4 : DecoderConfig a b c leaf path -> TreePath4 a b c leaf -> Value
pathEncode4 config ((TreePath4 tree4 _ _) as treePath4) =
    JE.object
        [ ( "tree", tree4 |> encode config )
        , ( "path", [] |> JE.list JE.int )
        ]


{-| -}
data4 : TreePath4 a b c leaf -> Data a leaf
data4 (TreePath4 (Tree4 { data }) _ _) =
    data


{-| -}
top4 : TreePath4 a b c leaf -> TreePath4 a b c leaf
top4 ((TreePath4 tree4 _ _) as treePath4) =
    treePath4


{-| -}
offset4 : Int -> TreePath4 a b c leaf -> Maybe (TreePath4 a b c leaf)
offset4 dx (TreePath4 _ idx parentPath) =
    Nothing


{-| -}
down4 : Int -> TreePath4 a b c leaf -> Maybe (TreePath3 a b c leaf)
down4 idx ((TreePath4 ((Tree4 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath3 childTree idx treePath)


{-| -}
downs4 : TreePath4 a b c leaf -> List (TreePath3 a b c leaf)
downs4 ((TreePath4 ((Tree4 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down4 idx treePath)


{-| -}
up4 : TreePath4 a b c leaf -> Maybe Never
up4 ((TreePath4 tree4 _ _) as treePath4) =
    Nothing
