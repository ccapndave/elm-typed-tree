module TreePath.Data exposing
    ( Data(..)
    , getBranchData, getLeafData
    )

{-| This module represents the data held at the node of a tree. Each node can
have either branch or leafdata, so this data can be either of those.


# Definition

@docs Data


# Unwrappers

@docs getBranchData, getLeafData

-}


{-| Represent a value that may either be `Branch` of `Leaf` data (where each has
its own type, parameterised by `b` and `l`.
-}
type Data b l
    = BranchData b
    | LeafData l


{-| If the given data is branch data then unwrap it.
-}
getBranchData : Data b l -> Maybe b
getBranchData data =
    case data of
        BranchData b ->
            Just b

        LeafData l ->
            Nothing


{-| If the given data is leaf data then unwrap it.
-}
getLeafData : Data b l -> Maybe l
getLeafData data =
    case data of
        BranchData b ->
            Nothing

        LeafData l ->
            Just l
