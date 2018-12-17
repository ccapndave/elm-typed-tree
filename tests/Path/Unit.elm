module Path.Unit exposing (data, down, downs, offset, top, up)

import Path.Types exposing (..)
import TreePath.Data exposing (Data)
import TreePath.Tree4 as Tree4


data : UnitPath -> Data Unit Exercise
data =
    Tree4.data2


top : UnitPath -> MenuPath
top =
    Tree4.top2


offset : Int -> UnitPath -> Maybe UnitPath
offset =
    Tree4.offset2


down : Int -> UnitPath -> Maybe ExercisePath
down =
    Tree4.down2


downs : UnitPath -> List ExercisePath
downs =
    Tree4.downs2


up : UnitPath -> Maybe CoursePath
up =
    Tree4.up2
