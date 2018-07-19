module TreePath.Data
    exposing
        ( Data(..)
        , getBranchData
        , getLeafData
        )

{-| -}


type Data b l
    = BranchData b
    | LeafData l


getBranchData : Data b l -> Maybe b
getBranchData data =
    case data of
        BranchData b ->
            Just b

        LeafData l ->
            Nothing


getLeafData : Data b l -> Maybe l
getLeafData data =
    case data of
        BranchData b ->
            Nothing

        LeafData l ->
            Just l
