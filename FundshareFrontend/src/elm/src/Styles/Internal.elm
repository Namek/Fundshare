module Styles.Internal exposing (c, cc, p2k)

import Regex exposing (Regex)


{-| PascalCase to class name in kebab-case.
-}
p2k : String -> String
p2k =
    pascalToKebabCase
        >> String.split "_"
        >> String.join "--"


c str =
    "." ++ p2k str


cc =
    \cls cls2 -> c cls ++ c cls2


maybePascalRegex : Maybe Regex
maybePascalRegex =
    Regex.fromString "([a-z])([A-Z])"


pascalToKebabCase : String -> String
pascalToKebabCase str =
    maybePascalRegex
        |> Maybe.andThen
            (\pascalRegex ->
                Regex.replace pascalRegex
                    (\{ submatches } ->
                        case submatches of
                            [ Just a, Just b ] ->
                                a ++ "-" ++ String.toLower b

                            _ ->
                                Debug.todo "other case with this regex should never happen"
                    )
                    str
                    |> Just
            )
        |> Maybe.withDefault ""
