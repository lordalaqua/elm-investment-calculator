module Form exposing (calculatorForm)

import Model
import Update
import Html exposing (form, div, label, input, select, option, text)
import Html.Attributes exposing (class, for, type_, name, step, value, selected)
import Html.Events exposing (onInput)

calculatorForm :  Model.Model -> Html.Html Update.Msg
calculatorForm model =
  form [ class "calc-form"]
    [ div [ class "input-wrapper" ]
        [ label [ for "start_value" ] [ text "Valor Inicial" ]
        , input
            [ type_ "number"
            , name "start_value"
            , step "1000"
            , Html.Attributes.min "0"
            , onInput Update.StartValue
            , value (toString model.start_value)
            ]
            []
        ]
    , div [ class "input-wrapper" ]
        [ label [ for "deposit" ] [ text "Aporte mensal" ]
        , input
            [ type_ "number"
            , name "deposit"
            , step "100"
            , Html.Attributes.min "0"
            , onInput Update.Deposit
            , value (toString model.deposit)
            ]
            []
        ]
    , div [ class "input-wrapper" ]
        [ label [ for "rate" ] [ text "Taxa(%)" ]
        , input
            [ type_ "number"
            , name "rate"
            , step "0.50"
            , Html.Attributes.min "0"
            , onInput Update.Rate
            , value (toString model.rate)
            ]
            []
        ]
    , div [ class "input-wrapper" ]
        [ label [ for "rate_type" ] [ text "Por" ]
        , select
            [ name "rate_type"
            , onInput Update.RateKind
            , value (Model.rateTypetoString model.rate_type)
            ]
            (List.map
                (\val -> option
                    [ value val
                    , selected
                        (model.rate_type == Model.stringToRateType val)
                    ]
                    [ text val ])
                [ "Mês", "Ano" ]
            )
        ]
    , div [ class "input-wrapper" ]
        [ label [ for "time" ] [ text "Tempo aplicado" ]
        , input
            [ type_ "number"
            , name "time"
            , value "1"
            , Html.Attributes.min "1"
            , Html.Attributes.max "1000"
            , onInput Update.Time
            , value (toString model.time)
            ]
            []
        ]
    , div [ class "input-wrapper" ]
        [ label [ for "time_type" ] [ text "Em" ]
        , select
            [ name "time_type"
            , onInput Update.TimeKind
            , value (Model.timeTypetoString model.time_type)
            ]
            (List.map
                (\val -> option
                    [ value val
                    , selected
                        (model.time_type == Model.stringToTimeType val)
                    ]
                    [ text val ])
                [ "Meses", "Anos" ]
            )
        ]
    ]