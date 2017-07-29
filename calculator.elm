import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODELInt
type RateType = Monthly | Yearly
type TimeType = Months | Years

rateTypetoString : RateType -> String
rateTypetoString t =
  if t == Monthly then "Mensal" else "Anual"
stringToRateType : String -> RateType
stringToRateType t =
  if t == "Mensal" then Monthly else Yearly

timeTypetoString : TimeType -> String
timeTypetoString t =
  if t == Months then "Meses" else "Anos"
stringToTimeType : String -> TimeType
stringToTimeType t =
  if t == "Meses" then Months else Years

type alias Model =
  { form :
    { start_value : Float
    , deposit : Float
    , rate : Float
    , rate_type : RateType
    , time : Int
    , time_type : TimeType
    }
  , results: {}
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
        old_form = model.form
        new_form = { old_form | start_value = Result.withDefault 0 (String.toFloat start_value) }
      in
        { model | form = new_form }
    Deposit deposit ->
      let
        old_form = model.form
        new_form = { old_form | deposit = Result.withDefault 0 (String.toFloat deposit) }
      in
        { model | form = new_form }
    Rate rate ->
      let
        old_form = model.form
        new_form = { old_form | rate = Result.withDefault 0 (String.toFloat rate) }
      in
        { model | form = new_form }
    RateKind rate_type ->
      let
        old_form = model.form
        new_form = { old_form | rate_type = stringToRateType rate_type }
      in
        { model | form = new_form }
    Time time ->
      let
        old_form = model.form
        new_form = { old_form | time = Result.withDefault 0 (String.toInt time) }
      in
        { model | form = new_form }
    TimeKind time_type ->
      let
        old_form = model.form
        new_form = { old_form | time_type = stringToTimeType time_type }
      in
        { model | form = new_form }


-- VIEW

view : Model -> Html Msg
view model =
  Html.form []
    [ h1 [] [ text "Calculadora de juros compostos" ]
    , div []
      [ input [ type_ "number"
              , placeholder "Valor Inicial"
              , onInput StartValue
              , value (toString model.form.start_value)] []
      , input [ type_ "number"
              , placeholder "Aporte mensal"
              , onInput Deposit
              , value (toString model.form.deposit)] []
      ]
    , div []
      [ input [ type_ "number"
              , placeholder "Taxa"
              , onInput Rate
              , value (toString model.form.rate)] []
      , select [ onInput RateKind
               , value (rateTypetoString model.form.rate_type)]
               (List.map (\val -> option [value val] [text val]) ["Mensal","Anual"])
      ]
    ]
