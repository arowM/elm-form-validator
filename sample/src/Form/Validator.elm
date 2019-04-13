module Form.Validator exposing (..)

import Form.Error as Error exposing (Error)
import Validator exposing (..)


name : Validator String Error
name =
    minLength NameEmpty 1

age : Validator String Error
age =
    succeed

horn : Validator String Error
horn =
    maxBound HornTooMany 2

form
