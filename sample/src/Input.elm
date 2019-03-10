module Input exposing
    ( Config
    , Config_
    , Input
    , config
    , decorate
    , fromString
    , init
    , toString
    , view
    )

import Html exposing (Attribute, Html, div, input)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode



-- Model


type Input
    = Input (Maybe String)


init : Input
init =
    Input Nothing


toString : Input -> Maybe String
toString (Input mv) =
    mv


fromString : String -> Input
fromString =
    Input << Just



-- Config


type Config msg
    = Config (Config_ msg)


type alias Config_ msg =
    { placeholder : String
    , type_ : String
    , onChange : String -> msg
    }


config : Config_ msg -> Config msg
config =
    Config



-- View


view : Config msg -> Input -> Html msg
view (Config conf) (Input mv) =
    input
        [ Attributes.type_ conf.type_
        , Attributes.placeholder conf.placeholder
        , Attributes.value <|
            Maybe.withDefault "" mv
        , onChange conf.onChange
        , class "input"
        ]
        []


{-| Overwrite input view.
Append new style in `styles/input.scss` and take its class name as first argument.
-}
decorate : String -> Attribute msg
decorate key =
    classList
        [ ( "decorate", True )
        , ( key, True )
        ]



-- Helper functions


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    Events.stopPropagationOn "change" (Decode.map alwaysStop (Decode.map tagger Events.targetValue))


alwaysStop : a -> ( a, Bool )
alwaysStop x =
    ( x, True )


{-| A specialized version of `class` for this module.
It handles generated class name by CSS modules.
-}
class : String -> Attribute msg
class name =
    Attributes.class <| "input__" ++ name


classList : List ( String, Bool ) -> Attribute msg
classList ps =
    Attributes.classList <|
        List.map (\( name, b ) -> ( "input__" ++ name, b )) ps
