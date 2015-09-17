module Calculator where

import Html exposing (Html, Attribute, text, toElement, div, input, h1)
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

update newStr oldStr =
  newStr


view : Address String -> String -> Html
view address string =
  div []
    [ h1 [myStyle] [text "Stamp duty calculator"]
    ,  input
        [ placeholder "Value of property"
        , value string
        , on "input" targetValue (Signal.message address)
        , myStyle
        ]
        []
    , div [ myStyle ] [ text (stringToFloatOrZero string |> calculateStampDuty |> toString) ]
    ]

stringToFloatOrZero: String -> Float
stringToFloatOrZero string = 
    String.toFloat string 
    |> Result.toMaybe 
    |> Maybe.withDefault 0


type StampDutyBand = Remainder | Threshold Float

calculateStampDuty: Float -> Float
calculateStampDuty propertyValue =
    let rates = [{band = Threshold(125000.0), rate = 0}
                ,{band = Threshold(125000.0), rate = 0.02}
                ,{band = Threshold(675000.0), rate = 0.05}
                ,{band = Threshold(575000.0), rate = 0.10}
                ,{band = Remainder, rate = 0.12}]
    in
        .stampDuty (foldl appendRate { remainingPropertyValue = propertyValue, stampDuty = 0 } rates)

appendRate {band, rate} { remainingPropertyValue, stampDuty } =
    log ("band: " ++ (toString band) ++ ", rate: " ++ (toString rate) ++ ", remainingPropertyValue: " ++ (toString remainingPropertyValue)) <|
    let newRemainingPropertyValue = calculateRemainingPropertyValue band remainingPropertyValue
        amountEligableForStampDuty = calculateAmountEligableForStampDuty band remainingPropertyValue
        stampDutyForBand = (amountEligableForStampDuty * rate)
    in
        {remainingPropertyValue = newRemainingPropertyValue, stampDuty = stampDuty + stampDutyForBand}

--calculateRemainingPropertyValue: StampDutyBand Float -> Float
calculateRemainingPropertyValue band remainingPropertyValue =
    case band of
        Remainder ->
            0
        Threshold threshold ->
            if (remainingPropertyValue - threshold) < 0 then 0 else (remainingPropertyValue - threshold)

--calculateAmountEligableForStampDuty: StampDutyBand Float -> Float
calculateAmountEligableForStampDuty band remainingPropertyValue =
    case band of
        Remainder ->
            remainingPropertyValue
        Threshold threshold ->
            if (threshold < remainingPropertyValue) then threshold else remainingPropertyValue


myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
