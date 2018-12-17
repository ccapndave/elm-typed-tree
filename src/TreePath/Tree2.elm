module TreePath.Tree2 exposing
    ( Tree, TreePath1, TreePath2
    , DecoderConfig, decoder, pathDecoder, pathEncode1, pathEncode2
    , toRootPath
    , data1, data2
    , top1, up1, offset1, down1, downs1, top2, up2, offset2, down2, downs2
    )

{-| This module provides types and functions for managing a strongly typed tree
of depth 2. Each level of the tree can have its own type, and each level can
contain Data either of that type, or the leaf type.


# Definition

@docs Tree, TreePath1, TreePath2


# Encoders and decoders

@docs DecoderConfig, decoder, pathDecoder, pathEncode1, pathEncode2


# Path constructor

@docs toRootPath


# Data

@docs data1, data2


# Navigation

@docs top1, up1, offset1, down1, downs1, top2, up2, offset2, down2, downs2

-}

import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TreePath.Data as Data exposing (Data)


{-| -}
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


{-| -}
type TreePath1 a leaf
    = TreePath1 (Tree1 leaf) Int (TreePath2 a leaf)


{-| -}
type TreePath2 a leaf
    = TreePath2 (Tree2 a leaf) () ()


{-| -}
type alias DecoderConfig a leaf path =
    { level2 :
        { decoder : Decoder a
        , encoders : a -> List ( String, Value )
        , pathType : TreePath2 a leaf -> path
        , childrenField : String
        }
    , leaf :
        { decoder : Decoder leaf
        , encode : leaf -> Value
        , pathType : TreePath1 a leaf -> path
        }
    }


{-| -}
decoder : DecoderConfig a leaf path -> Decoder (Tree2 a leaf)
decoder config =
    decoder2
        ( config.level2.decoder, config.level2.childrenField )
        config.leaf.decoder


encode : DecoderConfig a leaf path -> Tree2 a leaf -> Value
encode config tree =
    encode2
        ( config.level2.encoders, config.level2.childrenField )
        config.leaf.encode
        tree


{-| -}
pathDecoder : DecoderConfig a leaf path -> Decoder path
pathDecoder config =
    JD.field "tree" (decoder config)
        |> JD.andThen
            (\tree ->
                JD.field "path" (JD.list JD.int)
                    |> JD.andThen
                        (\path ->
                            case path of
                                [] ->
                                    Just (TreePath2 tree () ())
                                        |> Maybe.map config.level2.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                [ b1 ] ->
                                    Just (TreePath2 tree () ())
                                        |> Maybe.andThen (down2 b1)
                                        |> Maybe.map config.leaf.pathType
                                        |> Maybe.map JD.succeed
                                        |> Maybe.withDefault (JD.fail "Illegal path branch index")

                                otherwise ->
                                    JD.fail <| "Illegal path length " ++ String.fromInt (List.length path)
                        )
            )


{-| -}
toRootPath : Tree2 a leaf -> TreePath2 a leaf
toRootPath tree =
    TreePath2 tree () ()


decoder1 : Decoder leaf -> Decoder (Tree1 leaf)
decoder1 leafDecoder =
    leafDecoder
        |> JD.map (\data -> Tree1 { data = data })


encode1 : (leaf -> Value) -> Tree1 leaf -> Value
encode1 leafEncode (Tree1 { data }) =
    leafEncode data


{-| -}
pathEncode1 : DecoderConfig a leaf path -> TreePath1 a leaf -> Value
pathEncode1 config ((TreePath1 tree1 idx1 ((TreePath2 tree2 _ _) as treePath2)) as treePath1) =
    JE.object
        [ ( "tree", tree2 |> encode config )
        , ( "path", [ idx1 ] |> JE.list JE.int )
        ]


{-| -}
data1 : TreePath1 a leaf -> leaf
data1 (TreePath1 (Tree1 { data }) _ _) =
    data


{-| -}
top1 : TreePath1 a leaf -> TreePath2 a leaf
top1 ((TreePath1 tree1 idx1 ((TreePath2 tree2 _ _) as treePath2)) as treePath1) =
    treePath2


{-| -}
offset1 : Int -> TreePath1 a leaf -> Maybe (TreePath1 a leaf)
offset1 dx (TreePath1 _ idx parentPath) =
    parentPath
        |> down2 (idx + dx)


{-| -}
down1 : Int -> TreePath1 a leaf -> Maybe Never
down1 _ _ =
    Nothing


{-| -}
downs1 : TreePath1 a leaf -> List Never
downs1 _ =
    []


{-| -}
up1 : TreePath1 a leaf -> Maybe (TreePath2 a leaf)
up1 ((TreePath1 tree1 idx1 ((TreePath2 tree2 _ _) as treePath2)) as treePath1) =
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
pathEncode2 : DecoderConfig a leaf path -> TreePath2 a leaf -> Value
pathEncode2 config ((TreePath2 tree2 _ _) as treePath2) =
    JE.object
        [ ( "tree", tree2 |> encode config )
        , ( "path", [] |> JE.list JE.int )
        ]


{-| -}
data2 : TreePath2 a leaf -> Data a leaf
data2 (TreePath2 (Tree2 { data }) _ _) =
    data


{-| -}
top2 : TreePath2 a leaf -> TreePath2 a leaf
top2 ((TreePath2 tree2 _ _) as treePath2) =
    treePath2


{-| -}
offset2 : Int -> TreePath2 a leaf -> Maybe (TreePath2 a leaf)
offset2 dx (TreePath2 _ idx parentPath) =
    Nothing


{-| -}
down2 : Int -> TreePath2 a leaf -> Maybe (TreePath1 a leaf)
down2 idx ((TreePath2 ((Tree2 { children }) as tree) _ _) as treePath) =
    children
        |> Array.get idx
        |> Maybe.map (\childTree -> TreePath1 childTree idx treePath)


{-| -}
downs2 : TreePath2 a leaf -> List (TreePath1 a leaf)
downs2 ((TreePath2 ((Tree2 { children }) as tree) _ _) as treePath) =
    List.range 0 (Array.length children - 1)
        |> List.filterMap (\idx -> down2 idx treePath)


{-| -}
up2 : TreePath2 a leaf -> Maybe Never
up2 ((TreePath2 tree2 _ _) as treePath2) =
    Nothing
