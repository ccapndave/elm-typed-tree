module Path.Types exposing (Course, CoursePath, Exercise, ExercisePath, Menu, MenuPath, MenuTree, Unit, UnitPath, menuTreeDecoder)

import Json.Decode as JD exposing (Decoder, field, int)
import Json.Encode as JE exposing (Value)
import TreePath.Tree4 as Tree4 exposing (Tree, TreePath1, TreePath2, TreePath3, TreePath4)


type alias MenuTree =
    Tree Menu Course Unit Exercise


type alias MenuPath =
    TreePath4 Menu Course Unit Exercise


type alias CoursePath =
    TreePath3 Menu Course Unit Exercise


type alias UnitPath =
    TreePath2 Menu Course Unit Exercise


type alias ExercisePath =
    TreePath1 Menu Course Unit Exercise


type alias Menu =
    { menuId : Int }


type alias Course =
    { courseId : Int }


type alias Unit =
    { unitId : Int }


type alias Exercise =
    { exerciseId : Int }


type Path
    = MenuPath MenuPath
    | CoursePath CoursePath
    | UnitPath UnitPath
    | ExercisePath ExercisePath


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
        { level4 =
            { decoder = menuDecoder
            , encoders = \_ -> []
            , pathType = MenuPath
            , childrenField = "courses"
            }
        , level3 =
            { decoder = courseDecoder
            , encoders = \_ -> []
            , pathType = CoursePath
            , childrenField = "units"
            }
        , level2 =
            { decoder = unitDecoder
            , encoders = \_ -> []
            , pathType = UnitPath
            , childrenField = "exercises"
            }
        , leaf =
            { decoder = exerciseDecoder
            , encode = \_ -> JE.null
            , pathType = ExercisePath
            }
        }
