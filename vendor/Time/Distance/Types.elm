module Time.Distance.Types exposing (Config, DistanceId(..), Locale, Tense(..))


type DistanceId
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


type Tense
    = Past
    | Future


type alias Config =
    { withAffix : Bool }


type alias Locale =
    Config -> Tense -> DistanceId -> String
