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


type Interval
    = Second
    | Minute
    | Hour
    | Day
    | Month
    | Year


timeDistanceInWords :
    Bool
    -> Time.Posix
    -> Time.Posix
    -> String
timeDistanceInWords includeSeconds t1 t2 =
    calculateTimeDistance includeSeconds t1 t2 |> locale_


calculateTimeDistance :
    Bool
    -> Time.Posix
    -> Time.Posix
    -> DistanceLocale
calculateTimeDistance includeSeconds d1 d2 =
    let
        seconds =
            Time.posixToMillis d1 - Time.posixToMillis d2

        minutes =
            round <| toFloat seconds / 60
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
formatInterval interval =
    case interval of
        Second ->
            "second"

        Minute ->
            "minute"

        Hour ->
            "hour"

        Day ->
            "day"

        Month ->
            "month"

        Year ->
            "year"


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
            prefix ++ " " ++ String.fromInt i ++ " " ++ formatInterval interval ++ "s"


exact : Interval -> Int -> String
exact interval i =
    case i of
        1 ->
            "1 " ++ formatInterval interval

        _ ->
            String.fromInt i ++ " " ++ formatInterval interval ++ "s"


upToOneMinute : Int -> DistanceLocale
upToOneMinute seconds =
    if seconds < 5 then
        LessThanXSeconds 5

    else if seconds < 10 then
        LessThanXSeconds 10

    else if seconds < 20 then
        LessThanXSeconds 20

    else if seconds < 40 then
        HalfAMinute

    else if seconds < 60 then
        LessThanXMinutes 1

    else
        XMinutes 1


upToOneDay : Int -> DistanceLocale
upToOneDay minutes =
    let
        hours =
            round <| toFloat minutes / 60
    in
    AboutXHours hours


upToOneMonth : Int -> DistanceLocale
upToOneMonth minutes =
    let
        days =
            round <| toFloat minutes / minutes_in_day
    in
    XDays days


upToTwoMonths : Int -> DistanceLocale
upToTwoMonths minutes =
    let
        months =
            round <| toFloat minutes / minutes_in_month
    in
    AboutXMonths months


upToOneYear : Int -> DistanceLocale
upToOneYear minutes =
    let
        nearestMonth =
            round <| toFloat minutes / minutes_in_month
    in
    XMonths nearestMonth


moreThanTwoMonths :
    Int
    -> Time.Posix
    -> Time.Posix
    -> DistanceLocale
moreThanTwoMonths months d1 d2 =
    if months < 12 then
        let
            minutes =
                (Time.posixToMillis d1 - Time.posixToMillis d2) // (1000 * 60 * minutes_in_month)
        in
        -- 2 months up to 12 months
        upToOneYear minutes

    else
        -- 1 year up to max Date
        let
            monthsSinceStartOfYear =
                months |> modBy 12

            years =
                floor <| toFloat months / 12
        in
        if monthsSinceStartOfYear < 3 then
            -- N years up to 1 years 3 months
            AboutXYears years

        else if monthsSinceStartOfYear < 9 then
            -- N years 3 months up to N years 9 months
            OverXYears years

        else
            -- N years 9 months up to N year 12 months
            AlmostXYears <| years + 1


minutes_in_day : number
minutes_in_day =
    1440


minutes_in_almost_two_days : number
minutes_in_almost_two_days =
    2520


minutes_in_month : number
minutes_in_month =
    43200


minutes_in_two_months : number
minutes_in_two_months =
    86400
