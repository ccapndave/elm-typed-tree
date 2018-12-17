module Path.Exercise exposing (data, down, offset, top, up)

import Path.Types exposing (..)
import TreePath.Data exposing (Data)
import TreePath.Tree4 as Tree4


data : ExercisePath -> Exercise
data =
    Tree4.data1


top : ExercisePath -> MenuPath
top =
    Tree4.top1


offset : Int -> ExercisePath -> Maybe ExercisePath
offset =
    Tree4.offset1


down : Int -> ExercisePath -> Maybe Never
down =
    Tree4.down1


up : ExercisePath -> Maybe UnitPath
up =
    Tree4.up1
