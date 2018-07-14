module Path.Types exposing (..)

import Json.Decode as JD exposing (Decoder, field, int)
import TreePath.Tree4 as Tree4 exposing (Tree, TreePath4, TreePath3, TreePath2, TreePath1)

type alias MenuTree = Tree Menu Course Unit Exercise

type alias MenuPath = TreePath4 Menu Course Unit Exercise

type alias CoursePath = TreePath3 Menu Course Unit Exercise

type alias UnitPath = TreePath2 Menu Course Unit Exercise

type alias ExercisePath = TreePath1 Menu Course Unit Exercise

type alias Menu = { menuId : Int }

type alias Course = { courseId : Int }

type alias Unit = { unitId : Int }

type alias Exercise = { exerciseId : Int }


menuTreeDecoder : Decoder MenuTree
menuTreeDecoder =
  let
    menuDecoder : Decoder Menu
    menuDecoder =
      JD.map Menu
        (field "menuId" int)

    courseDecoder : Decoder Course
    courseDecoder =
      JD.map Course
        (field "courseId" int)


    unitDecoder : Decoder Unit
    unitDecoder =
      JD.map Unit
        (field "unitId" int)

    exerciseDecoder : Decoder Exercise
    exerciseDecoder =
      JD.map Exercise
        (field "exerciseId" int)
  in
  Tree4.decoder
    { level4Decoder = menuDecoder
    , level4ChildrenField = "courses"
    , level3Decoder = courseDecoder
    , level3ChildrenField = "units"
    , level2Decoder = unitDecoder
    , level2ChildrenField = "exercises"
    , leafDecoder = exerciseDecoder
    }
