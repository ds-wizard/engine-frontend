module Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig.Announcement exposing
    ( Announcement
    , decoder
    , encode
    , toFormInitials
    , validation
    )

import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig.Announcement.AnnouncementLevel as AnnouncementLevel exposing (AnnouncementLevel)


type alias Announcement =
    { content : String
    , level : AnnouncementLevel
    , dashboard : Bool
    , loginScreen : Bool
    }


decoder : Decoder Announcement
decoder =
    D.succeed Announcement
        |> D.required "content" D.string
        |> D.required "level" AnnouncementLevel.decoder
        |> D.required "dashboard" D.bool
        |> D.required "loginScreen" D.bool


encode : Announcement -> E.Value
encode announcement =
    E.object
        [ ( "content", E.string announcement.content )
        , ( "level", AnnouncementLevel.encode announcement.level )
        , ( "dashboard", E.bool announcement.dashboard )
        , ( "loginScreen", E.bool announcement.loginScreen )
        ]


validation : Validation FormError Announcement
validation =
    V.succeed Announcement
        |> V.andMap (V.field "content" V.string)
        |> V.andMap (V.field "level" AnnouncementLevel.validation)
        |> V.andMap (V.field "dashboard" V.bool)
        |> V.andMap (V.field "loginScreen" V.bool)


toFormInitials : Announcement -> List ( String, Field )
toFormInitials announcement =
    [ ( "content", Field.string announcement.content )
    , ( "level", AnnouncementLevel.field announcement.level )
    , ( "dashboard", Field.bool announcement.dashboard )
    , ( "loginScreen", Field.bool announcement.loginScreen )
    ]
