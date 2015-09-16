module Calculator where

import Html exposing (Html, Attribute, text, toElement, div, input, h1)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String exposing (toFloat)

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
calculateStampDuty value =
    if | value > 125000 -> (value - 125000) * 0.2
       | otherwise -> 0


myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
