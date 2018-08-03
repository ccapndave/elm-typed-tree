module TreePath.Tree3
    exposing
        ( DecoderConfig
        , decoder
        , pathDecoder
        , toRootPath
        , Tree
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

import TreePath.Data as Data exposing (Data)
import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


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


pathDecoder : DecoderConfig a b leaf path -> Decoder path
pathDecoder config =
    (JD.field "path" <| JD.array JD.int)
        |> JD.andThen
            (\path ->
                case Array.length path of
                    0 ->
                        JD.succeed (config.level3.pathType << TreePath3)

                    1 ->
                        JD.succeed (config.level2.pathType << TreePath2)

                    2 ->
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


toRootPath : Tree3 a b leaf -> TreePath3 a b leaf
toRootPath tree =
    TreePath3
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


pathEncode1 : DecoderConfig a b leaf path -> TreePath1 a b leaf -> Value
pathEncode1 config (TreePath1 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
        ]


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


downs1 : TreePath1 a b leaf -> List Never
downs1 ((TreePath1 { tree, path }) as treePath) =
    []


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


encode2 : ( a -> List ( String, Value ), String ) -> (leaf -> Value) -> Tree2 a leaf -> Value
encode2 ( aEncoders, aChildrenField ) leafEncode (Tree2 { data, children }) =
    case data of
        Data.BranchData b ->
            JE.object <|
                ( aChildrenField, JE.array <| Array.map (encode1 leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


pathEncode2 : DecoderConfig a b leaf path -> TreePath2 a b leaf -> Value
pathEncode2 config (TreePath2 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
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


downs2 : TreePath2 a b leaf -> List (TreePath1 a b leaf)
downs2 ((TreePath2 { tree, path }) as treePath) =
    getFocusedTree2 treePath
        |> treeChildren2
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath1 { tree = tree, path = Array.push idx path })


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


encode3 : ( a -> List ( String, Value ), String ) -> ( b -> List ( String, Value ), String ) -> (leaf -> Value) -> Tree3 a b leaf -> Value
encode3 ( aEncoders, aChildrenField ) ( bEncoders, bChildrenField ) leafEncode (Tree3 { data, children }) =
    case data of
        Data.BranchData b ->
            JE.object <|
                ( aChildrenField, JE.array <| Array.map (encode2 ( bEncoders, bChildrenField ) leafEncode) children )
                    :: aEncoders b

        Data.LeafData l ->
            leafEncode l


pathEncode3 : DecoderConfig a b leaf path -> TreePath3 a b leaf -> Value
pathEncode3 config (TreePath3 { tree, path }) =
    JE.object
        [ ( "tree", encode config tree )
        , ( "path", (JE.array << Array.map JE.int) path )
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


downs3 : TreePath3 a b leaf -> List (TreePath2 a b leaf)
downs3 ((TreePath3 { tree, path }) as treePath) =
    getFocusedTree3 treePath
        |> treeChildren3
        |> (\children -> Array.length children - 1)
        |> List.range 0
        |> List.map (\idx -> TreePath2 { tree = tree, path = Array.push idx path })


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
