module Calculator where

import Html exposing (Html, Attribute, text, toElement, div, input, h1, h3)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String exposing (toFloat)
import List exposing ( map, foldl, foldr )
import Debug exposing ( log )

-- MODEL

type alias Model = String

-- UPDATE

update newPropertyValue previousPropertyValue =
  newPropertyValue

-- an Address that you can send messages to (our Mailbox/communication hub)
view : Address String -> String -> Html
view address propertyValue =
  div []
    [ h1 [myStyle] [text "Stamp duty calculator"]
    ,  input
        [ placeholder "Value of property"
        , value propertyValue
        , on "input" targetValue (Signal.message address)
        , myStyle
        ]
        []
    , h1 [myStyle] [text "Stamp duty ="]
    --(toString (calculateStampDuty (stringToFloatOrZero propertyValue)))
    , div [ myStyle ] [ text (propertyValue |> stringToFloatOrZero |> calculateStampDuty |> toString) ]
    ]

myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]


-- type Maybe a = Just a | Nothing
stringToFloatOrZero: String -> Float
stringToFloatOrZero string = 
    String.toFloat string 
    |> Result.toMaybe 
    |> Maybe.withDefault 0

--type alias StampDutyBandRate = {band: StampDutyBand, rate: Float}
--appendStampDutyForBand StampDutyBandRate ->  (Float -> Float) =


-- http://elm-lang.org/guide/model-the-problem
type StampDutyBand = Threshold Float | Remainder 

-- https://www.gov.uk/stamp-duty-land-tax/residential-property-rates
calculateStampDuty: Float -> Float
calculateStampDuty propertyValue =
    let rates = [{band = Threshold(125000.0), rate = 0}
                ,{band = Threshold(125000.0), rate = 0.02}
                ,{band = Threshold(675000.0), rate = 0.05}
                ,{band = Threshold(575000.0), rate = 0.10}
                ,{band = Remainder,           rate = 0.12}]
    in
        .stampDuty (foldl appendStampDutyForBand { remainingPropertyValue = propertyValue, stampDuty = 0 } rates)

appendStampDutyForBand { band, rate } { remainingPropertyValue, stampDuty } =
    log ("band: " ++ (toString band) ++ ", rate: " ++ (toString rate) ++ ", remainingPropertyValue: " ++ (toString remainingPropertyValue)) <|
    let amountEligableForStampDuty = (calculateAmountEligableForStampDuty band remainingPropertyValue)
        stampDutyForBand = (amountEligableForStampDuty * rate)
        propertyValueForNextBand = (calculateRemainingPropertyValue band remainingPropertyValue)
    in
        -- Return a new record
        { remainingPropertyValue = propertyValueForNextBand, stampDuty = stampDuty + stampDutyForBand }

calculateAmountEligableForStampDuty: StampDutyBand -> Float -> Float
calculateAmountEligableForStampDuty band remainingPropertyValue =
    case band of
        Threshold threshold ->
            if (threshold < remainingPropertyValue) then threshold else remainingPropertyValue
        Remainder ->
            remainingPropertyValue

calculateRemainingPropertyValue: StampDutyBand -> Float -> Float
calculateRemainingPropertyValue band remainingPropertyValue =
    case band of
        Threshold threshold ->
            if (remainingPropertyValue - threshold) < 0 then 0 else (remainingPropertyValue - threshold)
        Remainder ->
            0
