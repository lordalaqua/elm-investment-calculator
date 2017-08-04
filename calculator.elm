module Main exposing (..)

import Html
import Model
import Update
import View

main : Program Never Model.Model Update.Msg
main =
    Html.beginnerProgram { model = Model.model, view = View.view, update = Update.update }

