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

calculateStampDuty: Float -> Float
calculateStampDuty propertyValue =
    let rates = [{threshold = 125000, rate = 0}
                ,{threshold = 125000, rate = 0.02}
                ,{threshold = 675000, rate = 0.05}
                ,{threshold = 575000, rate = 0.10}
                ,{threshold = 0, rate = 0.12}]
    in
        .stampDuty (foldl appendRate { remainingPropertyValue = propertyValue, stampDuty = 0 } rates)

appendRate {threshold, rate} { remainingPropertyValue, stampDuty } =
    log ("threshold: " ++ (toString threshold) ++ ", rate: " ++ (toString rate) ++ ", remainingPropertyValue: " ++ (toString remainingPropertyValue)) <|
    let newRemainingPropertyValue = if (remainingPropertyValue - threshold) < 0 then 0 else (remainingPropertyValue - threshold)
        amountEligableForStampDuty = if (threshold < remainingPropertyValue) then threshold else remainingPropertyValue
    in
        {remainingPropertyValue = newRemainingPropertyValue, stampDuty = stampDuty + (amountEligableForStampDuty * rate)}

myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
