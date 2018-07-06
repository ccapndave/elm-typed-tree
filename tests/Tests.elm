module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TreePath exposing (..)

suite : Test
suite =
  describe "Statechart"
    [ describe "Flat"
      [ test "Starting state" <| \_ ->
        Expect.equal True True
      ]
    ]
