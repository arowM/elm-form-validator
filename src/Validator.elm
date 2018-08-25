module Validator
    exposing
        ( Validator
        , concat
        , combine
        , map
        , lift
        , liftMap
        , required
        , optional
        , when
        , unless
        , with
        , errors
        , isValid
        , succeed
        , fail
        , custom
        , pattern
        , minBound
        , maxBound
        , maxLength
        , minLength
        )

{-| This module provides a scalable way to validate a form by combining primitive validators.

For example, let's assume a form having two inputs as follows.

    type alias Form =
        { sampleInput : Maybe Int
        , anotherInput : Maybe String
        }

The first step is to define a validator for each input.

    import Regex

    type SampleError
        = SampleBoundError
        | SampleRequiredError

    sampleValidator : Validator (Maybe Int) SampleError
    sampleValidator =
        required SampleRequiredError <|
            concat
                [ minBound SampleBoundError 10
                , maxBound SampleBoundError 20
                ]

    errors sampleValidator (Just 15)
    --> []

    errors sampleValidator (Just 30)
    --> [ SampleBoundError ]

    errors sampleValidator Nothing
    --> [ SampleRequiredError ]

    isValid sampleValidator (Just 15)
    --> True

    isValid sampleValidator (Just 30)
    --> False


    type AnotherError
        = AnotherLengthError
        | AnotherPatternError

    anotherValidator : Validator (Maybe String) AnotherError
    anotherValidator =
        optional <|
            concat
                [ maxLength AnotherLengthError 20
                , pattern AnotherPatternError <| Maybe.withDefault Regex.never <| Regex.fromString "^(http://|https://)"
                ]

    errors anotherValidator Nothing
    --> []

    errors anotherValidator (Just "foo")
    --> [ AnotherPatternError ]

    errors anotherValidator (Just "https://foo")
    --> []

    errors anotherValidator (Just "https://tooooooooooolong")
    --> [ AnotherLengthError ]

    errors anotherValidator (Just "ftp://tooooooooooolong")
    --> [ AnotherLengthError, AnotherPatternError ]

    isValid anotherValidator Nothing
    --> True

    isValid anotherValidator (Just "foo")
    --> False

The next step is combining these validators to create a validator for the entire form.

    type FormError
        = SampleError SampleError
        | AnotherError AnotherError

    formValidator : Validator Form FormError
    formValidator =
        concat
            [ liftMap SampleError .sampleInput sampleValidator
            , liftMap AnotherError .anotherInput anotherValidator
            ]

    errors formValidator
        { sampleInput = Just 15
        , anotherInput = Just "https://foo"
        }
    --> []

    errors formValidator
        { sampleInput = Nothing
        , anotherInput = Nothing
        }
    --> [ SampleError SampleRequiredError ]

    errors formValidator
        { sampleInput = Nothing
        , anotherInput = Just "foo"
        }
    --> [ SampleError SampleRequiredError
    --> , AnotherError AnotherPatternError
    --> ]

    displayFormError : FormError -> String
    displayFormError err =
        case err of
            SampleError SampleRequiredError ->
                "Sample Input cannot be empty"
            SampleError SampleBoundError ->
                "Sample Input is out of bounds"
            AnotherError AnotherLengthError ->
                "Length of Another Input is toooo long"
            AnotherError AnotherPatternError ->
                "Another Input must begin with `http://` or `https://`"

    map displayFormError <|
        errors formValidator
            { sampleInput = Nothing
            , anotherInput = Nothing
            }
    --> [ "Sample Input cannot be empty" ]


# Types

@docs Validator


# Functions to run Validator

@docs errors
@docs isValid


# Primitive Validators

@docs succeed
@docs fail
@docs minBound
@docs maxBound
@docs maxLength
@docs minLength
@docs pattern
@docs custom


# Combinators

@docs concat
@docs (||.)


# Helper functions

@docs required
@docs optional
@docs when
@docs unless
@docs with


# Operators

@docs map
@docs lift
@docs liftMap

-}

import Regex exposing (Regex)


{-| An opaque type representing validator for value of type `a`.
-}
type Validator a err
    = Validator (a -> Validity err)


type Validity err
    = Valid
    | Invalid (List err)


{-| A convenient wrapper for validating required values.

    errors (required "Cannot be empty" <| minBound "Too small" 10) Nothing
    --> [ "Cannot be empty" ]

    errors (required "Cannot be empty" <| minBound "Too small" 10) <| Just 100
    --> []

    errors (required "Cannot be empty" <| minBound "Too small" 10) <| Just 2
    --> [ "Too small" ]

-}
required : err -> Validator a err -> Validator (Maybe a) err
required err (Validator f) =
    Validator <|
        \ma ->
            case ma of
                Nothing ->
                    Invalid [ err ]

                Just a ->
                    f a


{-| A convenient wrapper for validating optional values.

    errors (optional <| minLength "Too small" 10) Nothing
    --> []

    errors (optional <| minLength "Too small" 10) <| Just "enough long"
    --> []

    errors (optional <| minLength "Too small" 10) <| Just "short"
    --> [ "Too small" ]

-}
optional : Validator a err -> Validator (Maybe a) err
optional (Validator f) =
    Validator <|
        \ma ->
            case ma of
                Nothing ->
                    Valid

                Just a ->
                    f a


{-| Only checks validity if a condition is `True`.

    import Regex

    checkPrefix : Validator String String
    checkPrefix = pattern "Incorrect format" (Regex.fromString "^foo" |> Maybe.withDefault Regex.never)

    errors (when (\str -> String.length str > 2) checkPrefix) "ba"
    --> []

    errors (when (\str -> String.length str > 2) checkPrefix) "bar"
    --> [ "Incorrect format" ]

-}
when : (a -> Bool) -> Validator a err -> Validator a err
when g (Validator f) =
    Validator <|
        \a ->
            if g a then
                f a
            else
                Valid


{-| Only checks validity unless a condition is `True`.

    import Regex

    checkPrefix : Validator String String
    checkPrefix = pattern "Incorrect format" (Regex.fromString "^foo" |> Maybe.withDefault Regex.never)

    errors (unless (\str -> String.length str < 3) checkPrefix) "ba"
    --> []

    errors (unless (\str -> String.length str < 3) checkPrefix) "bar"
    --> [ "Incorrect format" ]

-}
unless : (a -> Bool) -> Validator a err -> Validator a err
unless g =
    when (not << g)


{-|

    import Regex

    checkPrefix : Validator String String
    checkPrefix = pattern "Incorrect format" (Regex.fromString "^foo" |> Maybe.withDefault Regex.never)

    type alias Form =
        { foo : Maybe String
        , isRequired : Bool
        }

    form : Validator Form String
    form =
        with <| \{ isRequired } ->
            lift .foo <|
                (if isRequired then required "Required" else optional)
                    checkPrefix

    errors form { foo = Nothing, isRequired = False }
    --> []

    errors form { foo = Nothing, isRequired = True }
    --> [ "Required" ]

    errors form { foo = Just "bar" , isRequired = True }
    --> [ "Incorrect format" ]
-}
with : (a -> Validator a err) -> Validator a err
with f =
    Validator <|
        \a ->
            case f a of
                Validator g ->
                    g a


{-| Concatnate list of validators.

    import Regex

    errors (concat [ minBound "Too small" 10, maxBound "Too large" 100 ]) 8
    --> [ "Too small" ]

    errors (concat [ minBound "Too small" 10, maxBound "Too large" 100 ]) 20
    --> []

    errors (concat [ minLength "Too short" 10, pattern "Does not match pattern" (Regex.fromString "^foo" |> Maybe.withDefault Regex.never) ]) "bar"
    --> [ "Too short", "Does not match pattern" ]

-}
concat : List (Validator a err) -> Validator a err
concat fs =
    Validator <|
        \a ->
            case List.concatMap (\v -> errors v a) fs of
                [] ->
                    Valid

                ls ->
                    Invalid ls


{-| Combine two validators on OR condition.

    import Regex

    errors
        (combine
            (minLength "Too short" 10)
            (pattern "Does not match pattern"
                (Regex.fromString "^foo"
                    |> Maybe.withDefault Regex.never
                )
            )
        )
        "foobar"
    --> []

    errors
        (combine
            (minLength "Too short" 10)
            (pattern "Does not match pattern"
                (Regex.fromString "^foo"
                    |> Maybe.withDefault Regex.never
                )
            )
        )
        "enough long"
    --> []

    errors
        (combine (minLength "Too short" 10)
            (pattern "Does not match pattern"
                (Regex.fromString "^foo"
                    |> Maybe.withDefault Regex.never
                )
            )
        )
        "short"
    --> [ "Does not match pattern" ]

-}
combine : Validator a err -> Validator a err -> Validator a err
combine (Validator f) (Validator g) =
    Validator <|
        \a ->
            if f a == Valid then
                Valid
            else
                g a


{-| Convert `err` type.
-}
map : (suberr -> err) -> Validator a suberr -> Validator a err
map g (Validator f) =
    Validator <|
        \a ->
            case f a of
                Valid ->
                    Valid

                Invalid errs ->
                    Invalid <| List.map g errs


{-| `lift` is mainly used for accessing sub model of target value.

    errors (lift .str <| minLength "Too short" 10) { str = "foo", int = 5 }
    --> [ "Too short" ]

-}
lift : (a -> b) -> Validator b err -> Validator a err
lift g (Validator f) =
    Validator <| f << g


{-| `liftMap` can convert a validator by `lift` and `map` at one time for convenience.
-}
liftMap : (suberr -> err) -> (a -> b) -> Validator b suberr -> Validator a err
liftMap h g v =
    map h <| lift g v


{-| Run validator to a target value and returns all validation errors.
-}
errors : Validator a err -> a -> List err
errors (Validator f) a =
    case f a of
        Valid ->
            []

        Invalid errs ->
            errs


{-| The `isValid` only checks if a target value is valid or not.
-}
isValid : Validator a err -> a -> Bool
isValid (Validator f) a =
    f a == Valid


{-| A constructor for `Validator` which always results to valid.

    isValid succeed "foo"
    --> True

    isValid succeed <| Just 34
    --> True

    errors (required "Required error" succeed) <| Nothing
    --> [ "Required error" ]

-}
succeed : Validator a err
succeed =
    Validator <| always Valid


{-| A constructor for `Validator` which always results to invalid.

    errors (fail "error") "foo"
    --> [ "error" ]

    errors (fail "error") <| Just 34
    --> [ "error" ]

    errors (when (\n -> n < 0) <| fail "error") -1
    --> [ "error" ]

-}
fail : err -> Validator a err
fail err =
    custom (always <| Just err)


{-| A constructor for `Validator` from a function.

    errors (custom (\n -> if n < 10 then Just "Too small" else Nothing)) 8
    --> [ "Too small" ]

-}
custom : (a -> Maybe err) -> Validator a err
custom f =
    Validator <|
        \a ->
            case f a of
                Nothing ->
                    Valid

                Just err ->
                    Invalid [ err ]


{-| A constructor for `Validator` from a regular expression.

    import Regex

    errors (pattern "Pattern error" (Regex.fromString "^foo" |> Maybe.withDefault Regex.never)) "foobar"
    --> []

    errors (pattern "Pattern error" (Regex.fromString "^foo" |> Maybe.withDefault Regex.never)) "barfoo"
    --> [ "Pattern error" ]

-}
pattern : err -> Regex -> Validator String err
pattern err exp =
    Validator <|
        \str ->
            if Regex.contains exp str then
                Valid
            else
                Invalid [ err ]


{-| A constructor for `Validator` providing minimum bound.

    errors (minBound "Too small" 10) 2
    --> [ "Too small" ]

-}
minBound : err -> comparable -> Validator comparable err
minBound err bound =
    Validator <|
        \n ->
            if n >= bound then
                Valid
            else
                Invalid [ err ]


{-| A constructor for `Validator` providing maximum bound.

    errors (maxBound "Too large" 100) 200
    --> [ "Too large" ]

-}
maxBound : err -> comparable -> Validator comparable err
maxBound err bound =
    Validator <|
        \n ->
            if n <= bound then
                Valid
            else
                Invalid [ err ]


{-| A constructor for `Validator` providing minimum length.

    errors (minLength "Too short" 10) "short"
    --> [ "Too short" ]

-}
minLength : err -> Int -> Validator String err
minLength err bound =
    Validator <|
        \str ->
            if String.length str >= bound then
                Valid
            else
                Invalid [ err ]


{-| A constructor for `Validator` providing maximum length.

    errors (maxLength "Too long" 10) "tooooooooo long"
    --> [ "Too long" ]

-}
maxLength : err -> Int -> Validator String err
maxLength err bound =
    Validator <|
        \str ->
            if String.length str <= bound then
                Valid
            else
                Invalid [ err ]
