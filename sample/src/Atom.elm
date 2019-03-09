module Atom exposing
    ( row
    , wrap
    )

import Html exposing (Html, div)
import Layout


{-| Wrap children with half padding.
See `StyleGuide.elm` for actual usage.
-}
wrap : List (Html msg) -> Html msg
wrap children =
    div
        [ Layout.wrap ]
        children


{-| Align child elements horizontally.
-}
row : List (Html msg) -> Html msg
row children =
    div
        [ Layout.row ]
        children
