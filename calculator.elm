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
        , time = 1
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
                    , step "100"
                    , Html.Attributes.min "0"
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
                    , step "100"
                    , Html.Attributes.min "0"
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
                    , step "0.50"
                    , Html.Attributes.min "0"
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
                    , Html.Attributes.value "1"
                    , Html.Attributes.min "1"
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
            ]
        , graph model
        ]

convertToRange : Float -> Float -> Float -> Float -> Float -> Float
convertToRange value r1 s1 r2 s2 = (value-s1)/(r1-s1) * (r2-s2) + s2

graph : Model -> Html msg
graph model =
  let
    rate =
      if model.form.rate_type == Yearly then
        (1 + model.form.rate/100) ^ (1 / 12) - 1
      else
        model.form.rate / 100
    time =
      toFloat (if model.form.time_type == Years then
        model.form.time * 12
      else
        model.form.time)
    svgWidth =
      900
    svgHeight =
      svgWidth // 3
    finalValue =
      (total model.form.start_value model.form.deposit rate time)
    appliedValue =
      (applied model.form.start_value model.form.deposit time)
    valuesWidth =
      time + 1
    valuesHeight =
      1.2 * finalValue
    valuesX =
      0
    valuesY =
      0
    graphPadX =
      20
    graphPadY =
      10
    graphWidth =
      svgWidth - graphPadX
    rectWidth =
      svgWidth - (2 * graphPadX)
    graphHeight =
      toFloat svgHeight - graphPadY
    rectHeight =
      svgHeight - (2 * graphPadY)
    graphX =
      graphPadX
    graphY =
      graphPadY
    range =
      floor valuesWidth
    start_value =
      model.form.start_value
    plot1 n =
       ((toFloat n), (total model.form.start_value model.form.deposit rate (toFloat n)))
    plot2 n =
       ((toFloat n), (applied model.form.start_value model.form.deposit (toFloat n)))
    plot3 n =
       ((toFloat n), (interest model.form.start_value model.form.deposit rate (toFloat n)))
    graphPlotPoint p =
      ( (convertToRange
          (Tuple.first p)
          valuesWidth
          (2*valuesX)
          graphWidth
          graphX
        )
      , (toFloat svgHeight)
        -
        (Basics.max graphY (Basics.min graphHeight
          (convertToRange
            (Tuple.second p)
            valuesHeight
            valuesY
            graphHeight
            graphY
          )
        ))
      )
  in
    div [ class "results-wrapper"]
    [ div [ class "results" ]
      [ div [ class "results-item final" ]
        [ div [ class "results-title" ] [text "Valor Final: "]
        , div [ class "results-value" ] [text (formatFloat finalValue)]
        ]
      , div [ class "results-item applied" ]
        [ div [ class "results-title" ] [text "Montante aplicado: "]
        , div [ class "results-value" ] [ text (formatFloat appliedValue) ]
        ]
      , div [ class "results-item interest" ]
        [ div [ class "results-title" ] [text "Rendimento (Juros): "]
        , div [ class "results-value" ] [ text (formatFloat (finalValue - appliedValue)) ]
        ]
      , div [ class "results-item" ]
        [ div [ class "results-title" ] [text "Rendimento mensal médio: "]
        , div [ class "results-value" ] [ text (if time > 0 then (formatFloat ((finalValue - appliedValue)/time)) else "0,00") ]
        ]

      ]
    , div [ class "graph-wrapper"]
      [ Svg.svg
          [ Svg.Attributes.class "graph"
          , Svg.Attributes.width (toString svgWidth)
          , Svg.Attributes.height (toString svgHeight)
          , Svg.Attributes.viewBox ("0 0 " ++ toString svgWidth ++ " " ++ toString svgHeight)
          ]
          [ Svg.text_
            [ Svg.Attributes.x (toString (graphX+graphWidth))
            , Svg.Attributes.y (toString 50)
            ]
            [Svg.text "0"]
          , Svg.rect
            [ Svg.Attributes.fill "#fefefe"
            , Svg.Attributes.stroke "#ccc"
            , Svg.Attributes.x (toString graphX)
            , Svg.Attributes.y (toString graphY)
            , Svg.Attributes.width (toString rectWidth)
            , Svg.Attributes.height (toString rectHeight)
            ]
            []
          , (graphLine plot1 range graphPlotPoint "final")
          , (graphLine plot2 range graphPlotPoint "applied")
          , (graphLine plot3 range graphPlotPoint "interest")
          ]
      ]
    ]

formatFloat: Float -> String
formatFloat f =
  if f == 0 then "0,00" else
  let original = (toString (floor (f*100))) in
    (Tuple.second (String.foldr
      (\c -> \res ->
        let
          count =
            (Tuple.first res)
          str =
            (Tuple.second res)
          char =
            (String.fromChar c)
        in
          (count + 1
          , if (count /= 0 && count % 3 == 0)
            then char ++ "." ++ str
            else char ++ str
          )
      )
      (0, "")
      (String.dropRight 2 original)
    ))
    ++ "," ++ (String.right 2 original)


graphLine : (Int -> (Float, Float)) -> Int -> (( Float, Float ) -> ( Float, Float )) -> String -> Svg.Svg msg
graphLine function range plotPoint color =
  Svg.polyline
          [ Svg.Attributes.fill "none"
          , Svg.Attributes.class color
          , Svg.Attributes.strokeWidth "2"
          , Svg.Attributes.points
            (String.join " "
              (List.map
                (\p -> (toString (Tuple.first p)) ++ "," ++ (toString (Tuple.second p)))
                (List.map
                  plotPoint
                  (List.map function (List.range 0 range))
                )
              )
            )
          ]
          []

applied : Float -> Float -> Float -> Float
applied p d t =
  p + d * t

total : Float -> Float -> Float -> Float -> Float
total p d r t =
  if r == 0 then (applied p d t) else (p + d/r) * (1.0 + r) ^ t - (d/r)

interest : Float -> Float -> Float -> Float -> Float
interest p d r t =
  (total p d r t) - (applied p d t)

iteration : Float -> Float -> Float -> Float
iteration amount deposit rate =
  amount * (rate + 1.0) + deposit

