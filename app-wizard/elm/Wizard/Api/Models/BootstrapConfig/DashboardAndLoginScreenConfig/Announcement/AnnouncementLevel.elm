module Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig.Announcement.AnnouncementLevel exposing
    ( AnnouncementLevel(..)
    , decoder
    , encode
    , field
    , toString
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Maybe.Extra as Maybe


type AnnouncementLevel
    = Info
    | Warning
    | Critical


decoder : Decoder AnnouncementLevel
decoder =
    D.string
        |> D.andThen
            (\level ->
                Maybe.unwrap
                    (D.fail <| "Unexpected announcement level " ++ level)
                    D.succeed
                    (fromString level)
            )


encode : AnnouncementLevel -> E.Value
encode =
    E.string << toString


validation : Validation e AnnouncementLevel
validation =
    V.string
        |> V.andThen
            (\level ->
                Maybe.unwrap
                    (V.fail (Error.value InvalidString))
                    V.succeed
                    (fromString level)
            )


field : AnnouncementLevel -> Field
field =
    Field.string << toString


fromString : String -> Maybe AnnouncementLevel
fromString str =
    case str of
        "InfoAnnouncementLevelType" ->
            Just Info

        "WarningAnnouncementLevelType" ->
            Just Warning

        "CriticalAnnouncementLevelType" ->
            Just Critical

        _ ->
            Nothing


toString : AnnouncementLevel -> String
toString level =
    case level of
        Info ->
            "InfoAnnouncementLevelType"

        Warning ->
            "WarningAnnouncementLevelType"

        Critical ->
            "CriticalAnnouncementLevelType"
