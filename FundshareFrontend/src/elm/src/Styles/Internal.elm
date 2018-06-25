module Styles.Internal exposing (c, cc, p2k, t2k)

import Regex exposing (Regex)


{-| PascalCase to class name in kebab-case.
-}
p2k : String -> String
p2k =
    pascalToKebabCase
        >> String.split "_"
        >> String.join "--"


{-| Any type to class name in kebab-case.
-}
t2k : a -> String
t2k =
    toString >> p2k


( c, cc ) =
    ( \str -> "." ++ t2k str
    , \cls cls2 -> c cls ++ c cls2
    )


pascalToKebabCase : String -> String
pascalToKebabCase =
    Regex.replace Regex.All
        (Regex.regex "([a-z])([A-Z])")
        (\{ submatches } ->
            case submatches of
                [ Just a, Just b ] ->
                    a ++ "-" ++ b

                _ ->
                    Debug.crash "other case with this regex should never happen"
        )
        >> String.toLower
