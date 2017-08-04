module View exposing (view)

import Model
import Update
import Html
import Html.Attributes as Attr

import Form exposing (calculatorForm)

import Graph exposing (graph)


view : Model.Model -> Html.Html Update.Msg
view model =
    Html.main_ []
        [ Html.node "link" [ Attr.rel "stylesheet", Attr.href "calculator-nc.css" ] []
        , calculatorForm model
        , graph model
        ]