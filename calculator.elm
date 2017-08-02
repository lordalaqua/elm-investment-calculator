module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg
import Svg.Attributes
import Window

main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODELInt


type RateType
    = Monthly
    | Yearly


type TimeType
    = Months
    | Years


rateTypetoString : RateType -> String
rateTypetoString t =
    if t == Monthly then
        "Mês"
    else
        "Ano"


stringToRateType : String -> RateType
stringToRateType t =
    if t == "Mês" then
        Monthly
    else
        Yearly


timeTypetoString : TimeType -> String
timeTypetoString t =
    if t == Months then
        "Meses"
    else
        "Anos"


stringToTimeType : String -> TimeType
stringToTimeType t =
    if t == "Meses" then
        Months
    else
        Years


type alias Model =
    { form :
        { start_value : Float
        , deposit : Float
        , rate : Float
        , rate_type : RateType
        , time : Int
        , time_type : TimeType
        }
    , results : {}
    }


model : Model
model =
    Model
        { start_value = 0.0
        , deposit = 0.0
        , rate = 0.0
        , rate_type = Yearly
        , time = 0
        , time_type = Years
        }
        {}



-- UPDATE


type Msg
    = StartValue String
    | Deposit String
    | Rate String
    | RateKind String
    | Time String
    | TimeKind String


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartValue start_value ->
            let
                old_form =
                    model.form

                new_form =
                    { old_form | start_value = Result.withDefault 0 (String.toFloat start_value) }
            in
                { model | form = new_form }

        Deposit deposit ->
            let
                old_form =
                    model.form

                new_form =
                    { old_form | deposit = Result.withDefault 0 (String.toFloat deposit) }
            in
                { model | form = new_form }

        Rate rate ->
            let
                old_form =
                    model.form

                new_form =
                    { old_form | rate = Result.withDefault 0 (String.toFloat rate) }
            in
                { model | form = new_form }

        RateKind rate_type ->
            let
                old_form =
                    model.form

                new_form =
                    { old_form | rate_type = stringToRateType rate_type }
            in
                { model | form = new_form }

        Time time ->
            let
                old_form =
                    model.form

                new_form =
                    { old_form | time = Result.withDefault 0 (String.toInt time) }
            in
                { model | form = new_form }

        TimeKind time_type ->
            let
                old_form =
                    model.form

                new_form =
                    { old_form | time_type = stringToTimeType time_type }
            in
                { model | form = new_form }



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ Html.node "link" [ rel "stylesheet", href "calculator.css" ] []
        , h1 [] [ text "Calculadora de juros compostos" ]
        , Html.form []
            [ div [ class "input-wrapper" ]
                [ label [ for "start_value" ] [ text "Valor Inicial" ]
                , input
                    [ type_ "number"
                    , name "start_value"
                    , onInput StartValue
                    , value (toString model.form.start_value)
                    ]
                    []
                ]
            , div [ class "input-wrapper" ]
                [ label [ for "deposit" ] [ text "Aporte mensal" ]
                , input
                    [ type_ "number"
                    , name "deposit"
                    , onInput Deposit
                    , value (toString model.form.deposit)
                    ]
                    []
                ]
            , div [ class "input-wrapper" ]
                [ label [ for "rate" ] [ text "Taxa(%)" ]
                , input
                    [ type_ "number"
                    , name "rate"
                    , onInput Rate
                    , value (toString model.form.rate)
                    ]
                    []
                ]
            , div [ class "input-wrapper" ]
                [ label [ for "rate_type" ] [ text "Por" ]
                , select
                    [ name "rate_type"
                    , onInput RateKind
                    , value (rateTypetoString model.form.rate_type)
                    ]
                    (List.map (\val -> option [ value val, selected (model.form.rate_type == stringToRateType val) ] [ text val ]) [ "Mês", "Ano" ])
                ]
            , div [ class "input-wrapper" ]
                [ label [ for "time" ] [ text "Tempo aplicado" ]
                , input
                    [ type_ "number"
                    , name "time"
                    , onInput Time
                    , value (toString model.form.time)
                    ]
                    []
                ]
            , div [ class "input-wrapper" ]
                [ label [ for "time_type" ] [ text "Em" ]
                , select
                    [ name "time_type"
                    , onInput TimeKind
                    , value (timeTypetoString model.form.time_type)
                    ]
                    (List.map (\val -> option [ value val, selected (model.form.time_type == stringToTimeType val) ] [ text val ]) [ "Meses", "Anos" ])
                ]
            , button [ type_ "button" ] [ text "Calcular" ]
            ]
        , div []
            [ text (toString (finalSum model.form.start_value model.form.deposit (model.form.rate / 1200.0) (toFloat model.form.time)))
            ]
        , graph model
        ]

convertToRange : Float -> Float -> Float -> Float
convertToRange value r1 r2 = (value/r1) * r2

graph : Model -> Html msg
graph model =
  let
    graphWidth =
      (Basics.max (toFloat model.form.time) 1)
    finalValue = 
      (finalSum model.form.start_value model.form.deposit (model.form.rate / 1200.0) (toFloat model.form.time))
    graphHeight = 
      1.5 * finalValue
    svgWidth =
      900
    svgHeight = 
      600
    range =
      model.form.time
    start_value = 
      model.form.start_value
    function n =
      (iteration n model.form.deposit (model.form.rate/1200.0) )
  in
    Svg.svg
      [ Svg.Attributes.width (toString svgWidth)
      , Svg.Attributes.height (toString svgHeight)
      , Svg.Attributes.viewBox ("0 0 " ++ toString svgWidth ++ " " ++ toString svgHeight) 
      ]
      [ Svg.polyline
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "#f80"
        , Svg.Attributes.points
          (String.join " "
            (List.map 
              (\n -> 
                (toString (convertToRange (toFloat (Tuple.first n)) graphWidth svgWidth))
                ++ "," ++ 
                (toString ((toFloat svgHeight) - (convertToRange (Tuple.second n) graphHeight svgHeight)))
              )
              (List.scanl 
                (\el -> \res -> ( el, function (Tuple.second res))) 
                (0, start_value) 
                (List.range 1 range)
              )
            )
          )
        ]
        []
      ]


finalSum : Float -> Float -> Float -> Float -> Float
finalSum p d r t =
  if r == 0 then p + d * t else (p + d/r) * (1.0 + r) ^ t - (d/r)

iteration : Float -> Float -> Float -> Float
iteration amount deposit rate =
  amount * (rate + 1.0) + deposit

