module Misc.Time exposing (DistanceLocale, calculateTimeDistance, timeDistanceInWords)

import Time


{-| Data type used by localization functions
to produce a string.
-}
type DistanceLocale
    = LessThanXSeconds Int
    | HalfAMinute
    | LessThanXMinutes Int
    | XMinutes Int
    | AboutXHours Int
    | XDays Int
    | AboutXMonths Int
    | XMonths Int
    | AboutXYears Int
    | OverXYears Int
    | AlmostXYears Int


timeDistanceInWords :
    Bool
    -> Time.Posix
    -> Time.Posix
    -> String
timeDistanceInWords =
    calculateTimeDistance >> locale_


calculateTimeDistance :
    Bool
    -> Time.Posix
    -> Time.Posix
    -> DistanceLocale
calculateTimeDistance includeSeconds d1 d2 =
    let
        seconds =
            Date.diff Second d1 d2

        offset =
            Date.offsetFromUtc d1 - Date.offsetFromUtc d2

        minutes =
            (round <| toFloat seconds / 60) - offset
    in
    if includeSeconds && minutes < 2 then
        upToOneMinute seconds

    else if minutes == 0 then
        LessThanXMinutes 1

    else if minutes < 2 then
        XMinutes minutes

    else if minutes < 45 then
        -- 2 mins up to 0.75 hrs
        XMinutes minutes

    else if minutes < 90 then
        -- 0.75 hrs up to 1.5 hrs
        AboutXHours 1

    else if minutes < minutes_in_day then
        -- 1.5 hrs up to 24 hrs
        upToOneDay minutes

    else if minutes < minutes_in_almost_two_days then
        -- 1 day up to 1.75 days
        XDays 1

    else if minutes < minutes_in_month then
        -- 1.75 days up to 30 days
        upToOneMonth minutes

    else if minutes < minutes_in_two_months then
        -- 1 month up to 2 months
        upToTwoMonths minutes

    else
        moreThanTwoMonths minutes d1 d2


locale_ : DistanceLocale -> String
locale_ distance =
    case distance of
        LessThanXSeconds i ->
            circa "less than" Second i

        HalfAMinute ->
            "half a minute"

        LessThanXMinutes i ->
            circa "less than" Minute i

        XMinutes i ->
            exact Minute i

        AboutXHours i ->
            circa "about" Hour i

        XDays i ->
            exact Day i

        AboutXMonths i ->
            circa "about" Month i

        XMonths i ->
            exact Month i

        AboutXYears i ->
            circa "about" Year i

        OverXYears i ->
            circa "over" Year i

        AlmostXYears i ->
            circa "almost" Year i


formatInterval : Interval -> String
formatInterval =
    String.toLower << toString


singular : Interval -> String
singular interval =
    case interval of
        Minute ->
            "a " ++ formatInterval interval

        _ ->
            "1 " ++ formatInterval interval


circa : String -> Interval -> Int -> String
circa prefix interval i =
    case i of
        1 ->
            prefix ++ " " ++ singular interval

        _ ->
            prefix ++ " " ++ toString i ++ " " ++ formatInterval interval ++ "s"


exact : Interval -> Int -> String
exact interval i =
    case i of
        1 ->
            "1 " ++ formatInterval interval

        _ ->
            toString i ++ " " ++ formatInterval interval ++ "s"
