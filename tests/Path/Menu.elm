module Path.Menu exposing (data, down, downs, offset, top, up)

import Path.Types exposing (..)
import TreePath.Data exposing (Data)
import TreePath.Tree4 as Tree4


data : MenuPath -> Data Menu Exercise
data =
    Tree4.data4


top : MenuPath -> MenuPath
top =
    Tree4.top4


offset : Int -> MenuPath -> Maybe MenuPath
offset =
    Tree4.offset4


down : Int -> MenuPath -> Maybe CoursePath
down =
    Tree4.down4


downs : MenuPath -> List CoursePath
downs =
    Tree4.downs4


up : MenuPath -> Maybe Never
up =
    Tree4.up4
