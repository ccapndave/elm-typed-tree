module TreePath.Tree2
    exposing
        ( DecoderConfig
        , decoder
        , pathDecoder
        , toRootPath
        , Tree
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

import TreePath.Data as Data exposing (Data)
import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


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
    = TreePath1
        { tree : Tree2 a leaf
        , path : Array Int
        }


{-| -}
type TreePath2 a leaf
    = TreePath2
        { tree : Tree2 a leaf
        , path : Array Int
        }


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
    (JD.field "path" <| JD.array JD.int)
        |> JD.andThen
            (\path ->
                case Array.length path of
                    0 ->
                        JD.succeed (config.level2.pathType << TreePath2)

                    1 ->
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
toRootPath : Tree2 a leaf -> TreePath2 a leaf
toRootPath tree =
    TreePath2
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
pathEncode1 : DecoderConfig a leaf path -> TreePath1 a leaf -> Value
pathEncode1 config (TreePath1 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


getFocusedTree1 : TreePath1 a leaf -> Tree1 leaf
getFocusedTree1 (TreePath1 { tree, path }) =
    getFocusedTree2 (TreePath2 { tree = tree, path = path })
        |> treeChildren2
        |> Array.get (Array.get 0 path |> unsafe "getFocusedTree1")
        |> unsafe "getFocusedTree1"


treeData1 : Tree1 leaf -> leaf
treeData1 (Tree1 { data }) =
    data


{-| -}
data1 : TreePath1 a leaf -> leaf
data1 =
    getFocusedTree1 >> treeData1


{-| -}
top1 : TreePath1 a leaf -> TreePath2 a leaf
top1 (TreePath1 { tree, path }) =
    TreePath2 { tree = tree, path = Array.empty }


{-| -}
offset1 : Int -> TreePath1 a leaf -> Maybe (TreePath1 a leaf)
offset1 dx ((TreePath1 { tree, path }) as treePath) =
    treePath
        |> up1
        |> Maybe.andThen (down2 <| (Array.get 0 path |> unsafe "offset1") + dx)


{-| -}
down1 : Int -> TreePath1 a leaf -> Maybe Never
down1 idx ((TreePath1 { tree, path }) as treePath) =
    Nothing


{-| -}
downs1 : TreePath1 a leaf -> List Never
downs1 ((TreePath1 { tree, path }) as treePath) =
    []


{-| -}
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
pathEncode2 : DecoderConfig a leaf path -> TreePath2 a leaf -> Value
pathEncode2 config (TreePath2 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
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


{-| -}
data2 : TreePath2 a leaf -> Data a leaf
data2 =
    getFocusedTree2 >> treeData2


{-| -}
top2 : TreePath2 a leaf -> TreePath2 a leaf
top2 (TreePath2 { tree, path }) =
    TreePath2 { tree = tree, path = Array.empty }


{-| -}
offset2 : Int -> TreePath2 a leaf -> Maybe (TreePath2 a leaf)
offset2 dx ((TreePath2 { tree, path }) as treePath) =
    Nothing


{-| -}
down2 : Int -> TreePath2 a leaf -> Maybe (TreePath1 a leaf)
down2 idx ((TreePath2 { tree, path }) as treePath) =
    getFocusedTree2 treePath
        |> treeChildren2
        |> Array.get idx
        |> Maybe.map (\_ -> TreePath1 { tree = tree, path = Array.push idx path })


{-| -}
downs2 : TreePath2 a leaf -> List (TreePath1 a leaf)
downs2 ((TreePath2 { tree, path }) as treePath) =
    getFocusedTree2 treePath
        |> treeChildren2
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath1 { tree = tree, path = Array.push idx path })


{-| -}
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
