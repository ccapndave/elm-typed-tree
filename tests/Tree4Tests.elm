module Tree4Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Array exposing (Array)
import Json.Decode as JD exposing (Decoder, field)

import TreePath.Tree4 as Tree4
import TreePath.Data exposing (..)
import Path.Types exposing (menuTreeDecoder, MenuTree, MenuPath)
import Path.Menu as Menu
import Path.Course as Course
import Path.Unit as Unit
import Path.Exercise as Exercise

decodeMenuTree : Decoder MenuTree -> String -> MenuTree
decodeMenuTree menuTreeDecoder jsonStr =
  case JD.decodeString menuTreeDecoder jsonStr of
    Ok treePath ->
      treePath

    Err err ->
      Debug.crash err


menuTree : MenuTree
menuTree =
  decodeMenuTree menuTreeDecoder """
{
  "menuId": 1,
  "courses": [
    {
      "courseId": 2,
      "units": [
        {
          "unitId": 3,
          "exercises": [
            {
              "exerciseId": 4
            },
            {
              "exerciseId": 5
            }
          ]
        }
      ]
    },
    {
      "exerciseId": 6
    }
  ]
}
"""


menuPath : MenuPath
menuPath =
  menuTree
    |> Tree4.toRootPath


suite : Test
suite =
  describe "Tree4"
    [ test "Root data" <| \_ ->
      menuPath
        |> Menu.data
        |> Expect.equal  (BranchData { menuId = 1 })

    , test "Goto root" <| \_ ->
      menuPath
        |> Menu.top
        |> Menu.data
        |> Expect.equal (BranchData { menuId = 1 })

    , test "Goto root 2" <| \_ ->
      menuPath
        |> Menu.down 0
        |> Maybe.andThen (Course.down 0)
        |> Maybe.map Unit.top
        |> Maybe.map Menu.data
        |> Expect.equal (Just <| BranchData { menuId = 1 })

    , test "Down" <| \_ ->
      menuPath
        |> Menu.down 0
        |> Maybe.map Course.data
        |> Expect.equal (Just <| BranchData { courseId = 2 })

    , test "Indexed down" <| \_ ->
      menuPath
        |> Menu.down 1
        |> Maybe.map Course.data
        |> Expect.equal (Just <| LeafData { exerciseId = 6 })

    , test "Invalid down" <| \_ ->
      menuPath
        |> Menu.down 2
        |> Maybe.map Course.data
        |> Expect.equal Nothing

    , test "Double down" <| \_ ->
      menuPath
        |> Menu.down 0
        |> Maybe.andThen (Course.down 0)
        |> Maybe.map Unit.data
        |> Expect.equal (Just <| BranchData { unitId = 3 })

    , test "Up" <| \_ ->
      menuPath
        |> Menu.down 0
        |> Maybe.andThen (Course.down 0)
        |> Maybe.andThen Unit.up
        |> Maybe.map Course.data
        |> Expect.equal (Just (BranchData { courseId = 2 }))

    , test "Invalid up" <| \_ ->
      menuPath
        |> Menu.up
        |> Expect.equal Nothing

    , test "Right" <| \_ ->
      menuPath
        |> Menu.down 0
        |> Maybe.andThen (Course.offset 1)
        |> Maybe.map Course.data
        |> Expect.equal (Just <| LeafData { exerciseId = 6 })

    , test "Invalid right" <| \_ ->
      menuPath
        |> Menu.down 0
        |> Maybe.andThen (Course.offset 1)
        |> Maybe.andThen (Course.offset 1)
        |> Maybe.map Course.data
        |> Expect.equal Nothing

    ]
