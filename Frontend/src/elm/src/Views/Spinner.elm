module Views.Spinner exposing (spinner)

import Element exposing (Element, paragraph, row, text)


spinner : Element msg
spinner =
    row []
        [ paragraph [] [ text "Loading..." ] ]
