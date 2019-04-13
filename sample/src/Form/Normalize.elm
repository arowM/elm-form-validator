module Form.Normalize exposing
    ( age
    , email
    , horns
    , name
    , phone
    )

import Form.Error as Error exposing (Error)


name : String -> Result Error String
name =
    Ok


age : String -> Result Error Int
age =
    int Error.AgeInvalid


horns : String -> Result Error Int
horns =
    int Error.HornInvalid


email : String -> Result Error String
email =
    Ok


phone : String -> Result Error String
phone =
    Ok << String.map zenDigitToHan



-- Helper functions


int : Error -> String -> Result Error Int
int err val =
    String.toInt val
        |> Result.fromMaybe err


{-|

    zenDigitToHan '0'
    --> '0'

    zenDigitToHan 'a'
    --> 'a'

    zenDigitToHan '０'
    --> '0'

    zenDigitToHan '１'
    --> '1'

    zenDigitToHan '９'
    --> '9'

-}
zenDigitToHan : Char -> Char
zenDigitToHan c =
    let
        code =
            Char.toCode c

        zero =
            Char.toCode '０'

        nine =
            Char.toCode '９'
    in
    if zero <= code && code <= nine then
        Char.fromCode <|
            code
                - zero
                + Char.toCode '0'

    else
        c
