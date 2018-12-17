module TreePath.Tree3 exposing
    ( Tree, TreePath1, TreePath2, TreePath3
    , DecoderConfig, decoder, pathDecoder, pathEncode1, pathEncode2, pathEncode3
    , toRootPath
    , data1, data2, data3
    , top1, up1, offset1, down1, downs1, top2, up2, offset2, down2, downs2, top3, up3, offset3, down3, downs3
    )

{-| This module provides types and functions for managing a strongly typed tree
of depth 3. Each level of the tree can have its own type, and each level can
contain Data either of that type, or the leaf type.


# Definition

@docs Tree, TreePath1, TreePath2, TreePath3


# Encoders and decoders

@docs DecoderConfig, decoder, pathDecoder, pathEncode1, pathEncode2, pathEncode3


# Path constructor

@docs toRootPath


# Data

@docs data1, data2, data3


# Navigation

@docs top1, up1, offset1, down1, downs1, top2, up2, offset2, down2, downs2, top3, up3, offset3, down3, downs3

-}

import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TreePath.Data as Data exposing (Data)


{-| -}
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


{-| -}
type TreePath1 a b leaf
    = TreePath1 (Tree1 leaf) Int (TreePath2 a b leaf)


{-| -}
type TreePath2 a b leaf
    = TreePath2 (Tree2 b leaf) Int (TreePath3 a b leaf)


{-| -}
type TreePath3 a b leaf
    = TreePath3 (Tree3 a b leaf) () ()


{-| -}
type alias DecoderConfig a b leaf path =
    { level3 :
        { decoder : Decoder a
        , encoders : a -> List ( String, Value )
        , pathType : TreePath3 a b leaf -> path
        , childrenField : String
        }
    , level2 :
        { decoder : Decoder b
        , encoders : b -> List ( String, Value )
        , pathType : TreePath2 a b leaf -> path
        , childrenField : String
        }
    , leaf :
        { decoder : Decoder leaf
        , encode : leaf -> Value
        , pathType : TreePath1 a b leaf -> path
        }
    }


{-| -}
decoder : DecoderConfig a b leaf path -> Decoder (Tree3 a b leaf)
decoder config =
    decoder3
        ( config.level3.decoder, config.level3.childrenField )
        ( config.level2.decoder, config.level2.childrenField )
        config.leaf.decoder


encode : DecoderConfig a b leaf path -> Tree3 a b leaf -> Value
encode config tree =
    encode3
        ( config.level3.encoders, config.level3.childrenField )
        ( config.level2.encoders, config.level2.childrenField )
        config.leaf.encode
        tree


{-| -}
pathDecoder : DecoderConfig a b leaf path -> Decoder path
pathDecoder config =
    JD.field "tree" (decoder config)
        |> JD.andThen
            (\tree ->
                JD.field "path" (JD.list JD.int)
                    |> JD.andThen
                        (\path ->
                            case path of
                                [] ->
                                    Just (TreePath3 tree () ())
                                        |> Maybe.map config.level3.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1 ] ->
                                    Just (TreePath3 tree () ())
                                        |> Maybe.andThen (down3 b1)
                                        |> Maybe.map config.level2.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1, b2 ] ->
                                    Just (TreePath3 tree () ())
                                        |> Maybe.andThen (down3 b1)
                                        |> Maybe.andThen (down2 b2)
                                        |> Maybe.map config.leaf.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                otherwise ->
                                    JD.fail <| "Illegal path length " ++ String.fromInt (List.length path)
                        )
            )


{-| -}
toRootPath : Tree3 a b leaf -> TreePath3 a b leaf
toRootPath tree =
    TreePath3 tree () ()


decoder1 : Decoder leaf -> Decoder (Tree1 leaf)
decoder1 leafDecoder =
    leafDecoder
        |> JD.map (\data -> Tree1 { data = data })


encode1 : (leaf -> Value) -> Tree1 leaf -> Value
encode1 leafEncode (Tree1 { data }) =
    leafEncode data


{-| -}
pathEncode1 : DecoderConfig a b leaf path -> TreePath1 a b leaf -> Value
pathEncode1 config ((TreePath1 tree1 idx1 ((TreePath2 tree2 idx2 ((TreePath3 tree3 _ _) as treePath3)) as treePath2)) as treePath1) =
    JE.object
        [ ( "tree", tree3 |> encode config )
        , ( "path", [ idx2, idx1 ] |> JE.list JE.int )
        ]


{-| -}
data1 : TreePath1 a b leaf -> leaf
data1 (TreePath1 (Tree1 { data }) _ _) =
    data


{-| -}
top1 : TreePath1 a b leaf -> TreePath3 a b leaf
top1 ((TreePath1 tree1 idx1 ((TreePath2 tree2 idx2 ((TreePath3 tree3 _ _) as treePath3)) as treePath2)) as treePath1) =
    treePath3


{-| -}
offset1 : Int -> TreePath1 a b leaf -> Maybe (TreePath1 a b leaf)
offset1 dx (TreePath1 _ idx parentPath) =
    parentPath
        |> down2 (idx + dx)


{-| -}
down1 : Int -> TreePath1 a b leaf -> Maybe Never
down1 _ _ =
    Nothing


{-| -}
downs1 : TreePath1 a b leaf -> List Never
downs1 _ =
    []


{-| -}
up1 : TreePath1 a b leaf -> Maybe (TreePath2 a b leaf)
up1 ((TreePath1 tree1 idx1 ((TreePath2 tree2 idx2 ((TreePath3 tree3 _ _) as treePath3)) as treePath2)) as treePath1) =
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
pathEncode2 : DecoderConfig a b leaf path -> TreePath2 a b leaf -> Value
pathEncode2 config ((TreePath2 tree2 idx2 ((TreePath3 tree3 _ _) as treePath3)) as treePath2) =
    JE.object
        [ ( "tree", tree3 |> encode config )
        , ( "path", [ idx2 ] |> JE.list JE.int )
        ]


{-| -}
data2 : TreePath2 a b leaf -> Data b leaf
data2 (TreePath2 (Tree2 { data }) _ _) =
    data


{-| -}
top2 : TreePath2 a b leaf -> TreePath3 a b leaf
top2 ((TreePath2 tree2 idx2 ((TreePath3 tree3 _ _) as treePath3)) as treePath2) =
    treePath3


{-| -}
offset2 : Int -> TreePath2 a b leaf -> Maybe (TreePath2 a b leaf)
offset2 dx (TreePath2 _ idx parentPath) =
    parentPath
        |> down3 (idx + dx)


{-| -}
down2 : Int -> TreePath2 a b leaf -> Maybe (TreePath1 a b leaf)
down2 idx ((TreePath2 ((Tree2 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath1 childTree idx treePath)


{-| -}
downs2 : TreePath2 a b leaf -> List (TreePath1 a b leaf)
downs2 ((TreePath2 ((Tree2 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down2 idx treePath)


{-| -}
up2 : TreePath2 a b leaf -> Maybe (TreePath3 a b leaf)
up2 ((TreePath2 tree2 idx2 ((TreePath3 tree3 _ _) as treePath3)) as treePath2) =
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
pathEncode3 : DecoderConfig a b leaf path -> TreePath3 a b leaf -> Value
pathEncode3 config ((TreePath3 tree3 _ _) as treePath3) =
    JE.object
        [ ( "tree", tree3 |> encode config )
        , ( "path", [] |> JE.list JE.int )
        ]


{-| -}
data3 : TreePath3 a b leaf -> Data a leaf
data3 (TreePath3 (Tree3 { data }) _ _) =
    data


{-| -}
top3 : TreePath3 a b leaf -> TreePath3 a b leaf
top3 ((TreePath3 tree3 _ _) as treePath3) =
    treePath3


{-| -}
offset3 : Int -> TreePath3 a b leaf -> Maybe (TreePath3 a b leaf)
offset3 dx (TreePath3 _ idx parentPath) =
    Nothing


{-| -}
down3 : Int -> TreePath3 a b leaf -> Maybe (TreePath2 a b leaf)
down3 idx ((TreePath3 ((Tree3 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath2 childTree idx treePath)


{-| -}
downs3 : TreePath3 a b leaf -> List (TreePath2 a b leaf)
downs3 ((TreePath3 ((Tree3 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down3 idx treePath)


{-| -}
up3 : TreePath3 a b leaf -> Maybe Never
up3 ((TreePath3 tree3 _ _) as treePath3) =
    Nothing
