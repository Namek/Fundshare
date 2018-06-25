module Views.Spinner exposing (spinner)

import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (style)
import Material.Progress as Loading


spinner : Html msg
spinner =
    div [ style [ ( "transform", "scale(5)" ) ] ]
        [ Loading.indeterminate
        ]
