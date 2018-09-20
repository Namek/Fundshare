module Page.Errored exposing (PageLoadError, pageLoadError, view)

import Data.Session exposing (SessionState)
import Element exposing (Element, paragraph, row, text)



-- MODEL --


type PageLoadError
    = PageLoadError Model


type alias Model =
    { errorMessage : String
    }


pageLoadError : String -> PageLoadError
pageLoadError errorMessage =
    PageLoadError { errorMessage = errorMessage }



-- VIEW --


view : SessionState -> PageLoadError -> Element msg
view session (PageLoadError model) =
    row []
        [ row [] [ paragraph [] [ text "Error Loading Page " ] ]
        , row [] [ paragraph [] [ text model.errorMessage ] ]
        ]
