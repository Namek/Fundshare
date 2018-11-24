module Misc.SuccessAnimation exposing (successAnim)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy



--fill "#D8D8D8"
-- stroke "#979797"


successAnim { diameter, strokeColor, fillColor } =
    let
        diameterStr =
            String.fromInt diameter

        draw _ =
            svg
                [ class "successAnimation animated"
                , width diameterStr
                , height diameterStr
                , viewBox "0 0 70 70"
                ]
                [ Svg.path
                    [ class "successAnimationResult"
                    , fill fillColor
                    , d "M35,60 C21.1928813,60 10,48.8071187 10,35 C10,21.1928813 21.1928813,10 35,10 C48.8071187,10 60,21.1928813 60,35 C60,48.8071187 48.8071187,60 35,60 Z M23.6332378,33.2260427 L22.3667622,34.7739573 L34.1433655,44.40936 L47.776114,27.6305926 L46.223886,26.3694074 L33.8566345,41.59064 L23.6332378,33.2260427 Z"
                    ]
                    []
                , circle
                    [ class "successAnimationCircle"
                    , cx "35"
                    , cy "35"
                    , r "25"
                    , stroke strokeColor
                    , strokeWidth "2"
                    , strokeLinecap "round"
                    , fill "transparent"
                    ]
                    []
                , polyline
                    [ class "successAnimationCheck"
                    , stroke strokeColor
                    , strokeWidth "2"
                    , points "23 34 34 43 47 27"
                    , fill "transparent"
                    ]
                    []
                ]
    in
    Svg.Lazy.lazy draw diameter
