module Path.Course exposing (..)

import TreePath.Data exposing (Data)
import TreePath.Tree4 as Tree4
import Path.Types exposing (..)

data : CoursePath -> Data Course Exercise
data = Tree4.data3

top : CoursePath -> MenuPath
top = Tree4.top3

offset : Int -> CoursePath -> Maybe CoursePath
offset = Tree4.offset3

down : Int -> CoursePath -> Maybe UnitPath
down = Tree4.down3

up : CoursePath -> Maybe MenuPath
up = Tree4.up3
