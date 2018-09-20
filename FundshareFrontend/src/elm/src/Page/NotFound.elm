module Page.NotFound exposing (view)

import Data.Session exposing (SessionState)
import Element exposing (Element, paragraph, row, text)



-- VIEW --


view : SessionState -> Element msg
view session =
    row [] [ paragraph [] [ text "404" ] ]
