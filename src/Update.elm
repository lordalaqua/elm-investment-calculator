module Update exposing (..)

import Model

type Msg
    = StartValue String
    | Deposit String
    | Rate String
    | RateKind String
    | Time String
    | TimeKind String

stringToFloat : String -> Float
stringToFloat string =
  abs (Result.withDefault 0 (String.toFloat
    (String.map (\c -> if c == ',' then '.' else c) string)))
update : Msg -> Model.Model -> Model.Model
update msg model =
    case msg of
        StartValue start_value ->
          { model | start_value = stringToFloat start_value }

        Deposit deposit ->
          { model | deposit = stringToFloat deposit }

        Rate rate ->
          { model | rate = stringToFloat rate }

        RateKind rate_type ->
          { model | rate_type = Model.stringToRateType rate_type }

        Time time ->
          { model | time = Result.withDefault 0 (String.toInt time) }

        TimeKind time_type ->
          { model | time_type = Model.stringToTimeType time_type }

setRate : Float -> Model.RateType -> Float
setRate rate rate_type =
    if rate_type == Model.Yearly then
      (1 + rate/100) ^ (1 / 12) - 1
    else
      rate / 100

setTime : Int -> Model.TimeType -> Int
setTime time time_type =
    if time_type == Model.Years then
      time * 12
    else
      time