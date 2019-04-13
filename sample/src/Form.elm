module Form exposing
    ( Form
    , init
    , view
    )

import Input exposing (Input)
import Validator exposing (Validator)

type alias Form =
    { name : Input
    , age : Input
    , horns : Input
    , email : Input
    , phone : Input
    , message : Input
    }




init : Form
init =
    { name = Input.init
    , age = Input.init
    , horns = Input.init
    , email = Input.init
    , phone = Input.init
    , message = Input.init
    }





displayNameError : NameError -> String
displayNameError err =
    case err of
        NameRequired ->
            "必須項目です"

        NameEmpty ->
            "1文字以上入力してください"


-- Age




nameValidator : Validator String Error
nameValidator =
    minLength NameEmpty 1


displayNameError : NameError -> String
displayNameError err =
    case err of
        NameRequired ->
            "必須項目です"

        NameEmpty ->
            "1文字以上入力してください"


    , age = Input.init
    , horns = Input.init
    , email = Input.init
    , phone = Input.init



