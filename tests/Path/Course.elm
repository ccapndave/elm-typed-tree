module Path.Course exposing (data, down, downs, offset, top, up)

import Path.Types exposing (..)
import TreePath.Data exposing (Data)
import TreePath.Tree4 as Tree4


data : CoursePath -> Data Course Exercise
data =
    Tree4.data3


top : CoursePath -> MenuPath
top =
    Tree4.top3


offset : Int -> CoursePath -> Maybe CoursePath
offset =
    Tree4.offset3


down : Int -> CoursePath -> Maybe UnitPath
down =
    Tree4.down3


downs : CoursePath -> List UnitPath
downs =
    Tree4.downs3


up : CoursePath -> Maybe MenuPath
up =
    Tree4.up3
