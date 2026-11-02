module Common.Api.Models.Settings.DashboardAndMenuSettings exposing
    ( DashboardAndMenuSettings
    , customMenuLinkFields
    , decoder
    , default
    , encode
    , initEmptyForm
    , initForm
    , section
    , validation
    )

import Common.Api.Models.Announcement as Announcement exposing (Announcement)
import Common.Api.Models.CustomMenuLink as CustomMenuLink exposing (CustomMenuLink)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias DashboardAndMenuSettings =
    { customMenuLinks : List CustomMenuLink
    , announcements : List Announcement
    }


default : DashboardAndMenuSettings
default =
    { customMenuLinks = []
    , announcements = []
    }


section : { name : String, decoder : Decoder DashboardAndMenuSettings, encode : DashboardAndMenuSettings -> E.Value }
section =
    { name = "dashboard-and-menu", decoder = decoder, encode = encode }


decoder : Decoder DashboardAndMenuSettings
decoder =
    D.succeed DashboardAndMenuSettings
        |> D.required "customMenuLinks" (D.list CustomMenuLink.decoder)
        |> D.required "announcements" (D.list Announcement.decoder)


encode : DashboardAndMenuSettings -> E.Value
encode settings =
    E.object
        [ ( "customMenuLinks", E.list CustomMenuLink.encode settings.customMenuLinks )
        , ( "announcements", E.list Announcement.encode settings.announcements )
        ]



-- Form


validation : Validation FormError DashboardAndMenuSettings
validation =
    V.succeed DashboardAndMenuSettings
        |> V.andMap (V.field "customMenuLinks" (V.list CustomMenuLink.validation))
        |> V.andMap (V.field "announcements" (V.list Announcement.validation))


initEmptyForm : Form FormError DashboardAndMenuSettings
initEmptyForm =
    Form.initial [] validation


initForm : DashboardAndMenuSettings -> Form FormError DashboardAndMenuSettings
initForm settings =
    let
        customMenuLinks =
            List.map
                (\l ->
                    Field.group
                        [ ( "icon", Field.string l.icon )
                        , ( "title", Field.string l.title )
                        , ( "url", Field.string l.url )
                        , ( "newWindow", Field.bool l.newWindow )
                        ]
                )
                settings.customMenuLinks
    in
    Form.initial
        [ ( "customMenuLinks", Field.list customMenuLinks )
        , ( "announcements", Field.list (List.map (Field.group << Announcement.toFormInitials) settings.announcements) )
        ]
        validation


{-| The fields of one link, for moving an item of the list.
-}
customMenuLinkFields : List String
customMenuLinkFields =
    [ "icon", "title", "url", "newWindow" ]
