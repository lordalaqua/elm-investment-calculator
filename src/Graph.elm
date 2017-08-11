module Graph exposing (graph)

import Model
import Html exposing (..)
import Html.Attributes exposing (..)
import Svg
import Svg.Attributes


formatFloat: Float -> String
formatFloat f =
  if f > 1000000000000000 then "Mais de 1.000.000.000.000.000" else
  if f == 0 then "0,00" else
  if f < 1 then
    "0," ++ (toString (floor (f*100)))
  else
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

maxGraphValue : Float -> Float
maxGraphValue value =
  List.foldr
    (\n -> \res -> if value < n then n else res)--if res == 0 then if value < n then n else 0 else res)
    (10^15)
    (List.concat
      [ (List.map (\n -> toFloat (10^n)) (List.range 1 4))
      , (List.map (\n -> toFloat (n*10000)) (List.range 2 10))
      , (List.map (\n -> toFloat (n*100000)) (List.range 2 10))
      , (List.map (\n -> toFloat (n*5000000)) (List.range 1 20))
      , (List.map (\n -> toFloat (10^n)) (List.range 9 15))
      ])

graph : Model.Model -> Html.Html msg
graph model =
  let
    rate =
      if model.rate_type == Model.Yearly then
        (1 + model.rate/100) ^ (1 / 12) - 1
      else
        model.rate / 100
    time =
      (Basics.max 1 (Basics.min (1000*12) (toFloat (if model.time_type == Model.Years then
        model.time * 12
      else
        model.time))))
    svgWidth =
      900
    svgHeight =
      400
    finalValue =
      (Model.total model.start_value model.deposit rate time)
    appliedValue =
      (Model.applied model.start_value model.deposit time)
    valuesWidth =
      time
    valuesHeight =
      maxGraphValue finalValue
    valuesX =
      0
    valuesY =
      0
    graphPadX =
      100
    graphPadY =
      50
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
      model.start_value
    plot1 n =
       ((toFloat n), (Model.total model.start_value model.deposit rate (toFloat n)))
    plot2 n =
       ((toFloat n), (Model.applied model.start_value model.deposit (toFloat n)))
    plot3 n =
       ((toFloat n), (Model.interest model.start_value model.deposit rate (toFloat n)))
    graphPlotPoint p =
      ( (convertToRange
          (Tuple.first p)
          valuesWidth
          valuesX
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
    formatResults v =
      if time < 12000 then "R$ " ++ (formatFloat v) else "---"
    formatGraphValue value max =
      if max < 1000 then
        toString value
      else if max < 1000000 then
        toString (value/1000)
      else if max < 1000000000 then
        toString (value/1000000)
      else
        toString (value/1000000000)
    graphValuesBase max =
      if max < 1000 then
        ""
      else if max < 1000000 then
        "(x1000)"
      else if max < 1000000000 then
        "(x1 Milhão)"
      else
        "(x1 Bilhão)"
  in
    div [ class "results-wrapper"]
    [ div [ class "results" ]
      [ div [ class "results-item final" ]
        [ div [ class "results-title" ] [text "Valor Final: "]
        , div [ class "results-value" ] [text (formatResults finalValue)]
        ]
      , div [ class "results-item applied" ]
        [ div [ class "results-title" ] [text "Montante aplicado: "]
        , div [ class "results-value" ] [ text (formatResults appliedValue) ]
        ]
      , div [ class "results-item interest" ]
        [ div [ class "results-title" ] [text "Rendimento (Juros): "]
        , div [ class "results-value" ] [ text (formatResults (finalValue - appliedValue)) ]
        ]
      , div [ class "results-item monthly" ]
        [ div [ class "results-title" ] [text "Rendimento mensal médio: "]
        , div [ class "results-value" ] [ text (formatResults ((finalValue - appliedValue)/time)) ]
        ]

      ]
    , div [ class "graph-wrapper"]
      [ Svg.svg
          [ Svg.Attributes.class "graph"
          , Svg.Attributes.width (toString svgWidth)
          , Svg.Attributes.height (toString svgHeight)
          , Svg.Attributes.viewBox ("0 0 " ++ toString svgWidth ++ " " ++ toString svgHeight)
          ]
          [ Svg.rect
              [ Svg.Attributes.fill "#fefefe"
              , Svg.Attributes.stroke "#ccc"
              , Svg.Attributes.x (toString graphX)
              , Svg.Attributes.y (toString graphY)
              , Svg.Attributes.width (toString rectWidth)
              , Svg.Attributes.height (toString rectHeight)
              ]
              []
          , Svg.g []
            (List.map
              (\n ->
                let
                  value =
                    (toFloat n) * valuesHeight/5
                  y =
                    (toFloat svgHeight) -
                    (convertToRange
                      value
                      valuesHeight
                      valuesY
                      graphHeight
                      graphY)
                in
                Svg.g []
                [ Svg.text_
                    [ Svg.Attributes.textAnchor "end"
                    , Svg.Attributes.x (toString (graphX-5))
                    , Svg.Attributes.y (toString (y+6))
                    ]
                    [Svg.text (formatGraphValue value valuesHeight)]
                , Svg.line
                  [ Svg.Attributes.stroke "#ccc"
                  , Svg.Attributes.x1 (toString graphX)
                  , Svg.Attributes.x2 (toString graphWidth)
                  , Svg.Attributes.y1 (toString y)
                  , Svg.Attributes.y2 (toString y)
                  ]
                  []
                ]
              )
              (List.range 0 5)
            )
          , Svg.text_
            [ Svg.Attributes.textAnchor "start"
            , Svg.Attributes.x "0"
            , Svg.Attributes.y "200"
            ]
            [Svg.text "Valor (R$)"]
          , Svg.text_
            [ Svg.Attributes.textAnchor "start"
            , Svg.Attributes.x "0"
            , Svg.Attributes.y "218"
            ]
            [Svg.text (graphValuesBase valuesHeight)]
          , Svg.g []
            (List.map
              (\n ->
                let
                  value =
                    (toFloat n) * valuesWidth/12
                  x =
                    (convertToRange
                      value
                      valuesWidth
                      valuesX
                      graphWidth
                      graphX)
                in
                Svg.g []
                [ Svg.text_
                    [ Svg.Attributes.textAnchor "middle"
                    , Svg.Attributes.x (toString x)
                    , Svg.Attributes.y (toString (graphHeight + 15))
                    ]
                    [Svg.text (toString (toFloat (floor (value*10)) / 10))]
                ]
              )
              (List.range 1 12)
            )
          , Svg.text_
            [ Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.x (toString (graphWidth / 2))
            , Svg.Attributes.y (toString (graphHeight + 35))
            ]
            [Svg.text "Meses"]
          , (graphLine plot1 range graphPlotPoint "final")
          , (graphLine plot2 range graphPlotPoint "applied")
          , (graphLine plot3 range graphPlotPoint "interest")
          ]
      ]
    ]

convertToRange : Float -> Float -> Float -> Float -> Float -> Float
convertToRange value r1 s1 r2 s2 = (value-s1)/(r1-s1) * (r2-s2) + s2


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