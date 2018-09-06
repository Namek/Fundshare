module Page.NotFound exposing (view)

import Data.Session exposing (SessionState)
import Html exposing (Html, div, h1, img, main_, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)



-- VIEW --


view : SessionState -> Html msg
view session =
    main_ [ id "content", class "container", tabindex -1 ]
        [ h1 [] [ text "Not Found" ]
        , div [ class "row" ]
            [ text "404" ]
        ]
