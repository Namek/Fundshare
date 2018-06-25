module Styles.Common
    exposing
        ( commonGlobalStylesheet
        , csg
        )

import Styles.Internal exposing (..)


csg : String -> String
csg =
    p2k


commonGlobalStylesheet : String
commonGlobalStylesheet =
    """
    .interactive {
        cursor: pointer;
        transition: background 0.3s ease-out;
    }
    .interactive:hover {
        background: rgba(0,0,0,0.06);
    }

    @keyframes custom_ripple {
      0% {
        transform: scale(0, 0);
        opacity: 1;
      }
      20% {
        transform: scale(25, 25);
        opacity: 1;
      }
      100% {
        opacity: 0;
        transform: scale(40, 40);
      }
    }
    """
