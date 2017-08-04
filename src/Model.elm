module Model exposing (..)

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
    { start_value : Float
    , deposit : Float
    , rate : Float
    , rate_type : RateType
    , time : Int
    , time_type : TimeType
    }

model : Model
model =
    { start_value = 0.0
    , deposit = 0.0
    , rate = 0.0
    , rate_type = Yearly
    , time = 1
    , time_type = Years
    }

applied : Float -> Float -> Float -> Float
applied p d t =
  p + d * t

total : Float -> Float -> Float -> Float -> Float
total p d r t =
  if r == 0 then (applied p d t) else (p + d/r) * (1.0 + r) ^ t - (d/r)

interest : Float -> Float -> Float -> Float -> Float
interest p d r t =
  (total p d r t) - (applied p d t)