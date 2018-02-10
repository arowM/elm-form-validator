# elm-form-validator

[![Build Status](https://travis-ci.org/arowM/elm-form-validator.svg?branch=master)](https://travis-ci.org/arowM/elm-form-validator)

## Summary

This module provides a scalable way to validate a form by combining primitive validators.

## Example

For example, let's assume a form having two inputs as follows.

```elm
type alias Form =
    { sampleInput : Maybe Int
    , anotherInput : Maybe String
    }
```

The first step is to define a validator for each input.

```elm
import Regex exposing (regex)

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
            , pattern AnotherPatternError <| regex "^(http://|https://)"
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
```

The next step is combining these validators to create a validator for the entire form.

```elm
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
```

## Details

See the [documentation](http://package.elm-lang.org/packages/arowM/elm-form-validator/latest/Validator).
