module Form.Error exposing (..)

type Error
    = NameRequired
    | NameEmpty -- Arised when inputs are deleted.
    | AgeRequired
    | AgeEmpty
    | AgeInvalid
    | HornRequired
    | HornEmpty
    | HornInvalid
    | HornTooMany
    | EmailInvalid
    | PhoneInvalid
    | NoEmailOrPhone
