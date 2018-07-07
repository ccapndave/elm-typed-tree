module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TreePath exposing (..)
import Json.Decode as JD exposing (Decoder, field)


type alias StringIntTreePathBranchData = { caption : String }


type alias StringIntTreePathLeafData = { id : Int }


type alias StringIntTreePath = TreePath StringIntTreePathBranchData StringIntTreePathLeafData


decodeTreeString : Decoder b -> Decoder l -> String -> TreePath b l
decodeTreeString branchDecoder leafDecoder jsonStr =
  case JD.decodeString (treeDecoder branchDecoder leafDecoder) jsonStr of
    Ok treePath ->
      treePath

    Err err ->
      Debug.crash err


treePath : StringIntTreePath
treePath =
  decodeTreeString (field "caption" JD.string |> JD.map StringIntTreePathBranchData) (field "id" JD.int |> JD.map StringIntTreePathLeafData) """
{
  "caption": "root",
  "children": [
    {
      "caption": "Sub-tree 1",
      "children": [
        {
          "id": 1
        }
      ]
    },
    {
      "id": 2
    },
    {
      "id": 3
    }
  ]
}
  """


suite : Test
suite =
  describe "TreePath"
    [ test "Root data" <| \_ ->
      Expect.equal (BranchData { caption = "root" }) (data treePath)

    , test "Goto root" <| \_ ->
      treePath
        |> top
        |> data
        |> Expect.equal (BranchData { caption = "root" })

    , test "Goto root 2" <| \_ ->
      treePath
        |> down 0
        |> Maybe.andThen (down 0)
        |> Maybe.map top
        |> Maybe.map data
        |> Expect.equal (Just <| BranchData { caption = "root" })

    , test "Down" <| \_ ->
      treePath
        |> down 0
        |> Maybe.map data
        |> Expect.equal (Just <| BranchData { caption = "Sub-tree 1" })

    , test "Indexed down" <| \_ ->
      treePath
        |> down 1
        |> Maybe.map data
        |> Expect.equal (Just <| LeafData { id = 2 })

    , test "Indexed down 2" <| \_ ->
      treePath
        |> down 2
        |> Maybe.map data
        |> Expect.equal (Just <| LeafData { id = 3 })

    , test "Invalid down" <| \_ ->
      treePath
        |> down 100
        |> Maybe.map data
        |> Expect.equal Nothing

    , test "Double down" <| \_ ->
      treePath
        |> down 0
        |> Maybe.andThen (down 0)
        |> Maybe.map data
        |> Expect.equal (Just (LeafData { id = 1 }))

    , test "Up" <| \_ ->
      treePath
        |> down 0
        |> Maybe.andThen (down 0)
        |> Maybe.andThen up
        |> Maybe.map data
        |> Expect.equal (Just (BranchData { caption = "Sub-tree 1" }))

    , test "Invalid up" <| \_ ->
      treePath
        |> up
        |> Expect.equal Nothing

    , test "Right" <| \_ ->
      treePath
        |> down 0
        |> Maybe.andThen right
        |> Maybe.map data
        |> Expect.equal (Just <| LeafData { id = 2 })

    , test "Double right" <| \_ ->
      treePath
        |> down 0
        |> Maybe.andThen right
        |> Maybe.andThen right
        |> Maybe.map data
        |> Expect.equal (Just <| LeafData { id = 3 })

    ]
