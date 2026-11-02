module Common.Api.Models.Announcement exposing
    ( Announcement
    , decoder
    , encode
    , toFormInitials
    , validation
    )

import Common.Api.Models.Announcement.AnnouncementLevel as AnnouncementLevel exposing (AnnouncementLevel)
import Common.Utils.Form.FormError exposing (FormError)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias Announcement =
    { content : String
    , level : AnnouncementLevel
    }


decoder : Decoder Announcement
decoder =
    D.succeed Announcement
        |> D.required "content" D.string
        |> D.required "level" AnnouncementLevel.decoder


encode : Announcement -> E.Value
encode announcement =
    E.object
        [ ( "content", E.string announcement.content )
        , ( "level", AnnouncementLevel.encode announcement.level )
        ]


validation : Validation FormError Announcement
validation =
    V.succeed Announcement
        |> V.andMap (V.field "content" V.string)
        |> V.andMap (V.field "level" AnnouncementLevel.validation)


toFormInitials : Announcement -> List ( String, Field )
toFormInitials announcement =
    [ ( "content", Field.string announcement.content )
    , ( "level", AnnouncementLevel.field announcement.level )
    ]
