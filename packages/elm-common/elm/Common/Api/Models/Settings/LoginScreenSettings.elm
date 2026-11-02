module Common.Api.Models.Settings.LoginScreenSettings exposing
    ( LoginScreenSettings
    , decoder
    , default
    , encode
    , initEmptyForm
    , initForm
    , section
    , validation
    )

import Common.Api.Models.Announcement as Announcement exposing (Announcement)
import Common.Utils.Form.Field as Field
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias LoginScreenSettings =
    { loginInfo : Maybe String
    , loginInfoSidebar : Maybe String
    , announcements : List Announcement
    }


default : LoginScreenSettings
default =
    { loginInfo = Nothing
    , loginInfoSidebar = Nothing
    , announcements = []
    }


section : { name : String, decoder : Decoder LoginScreenSettings, encode : LoginScreenSettings -> E.Value }
section =
    { name = "login-screen", decoder = decoder, encode = encode }


decoder : Decoder LoginScreenSettings
decoder =
    D.succeed LoginScreenSettings
        |> D.required "loginInfo" (D.maybe D.string)
        |> D.required "loginInfoSidebar" (D.maybe D.string)
        |> D.required "announcements" (D.list Announcement.decoder)


encode : LoginScreenSettings -> E.Value
encode settings =
    E.object
        [ ( "loginInfo", E.maybe E.string settings.loginInfo )
        , ( "loginInfoSidebar", E.maybe E.string settings.loginInfoSidebar )
        , ( "announcements", E.list Announcement.encode settings.announcements )
        ]



-- Form


validation : Validation FormError LoginScreenSettings
validation =
    V.succeed LoginScreenSettings
        |> V.andMap (V.field "loginInfo" V.maybeString)
        |> V.andMap (V.field "loginInfoSidebar" V.maybeString)
        |> V.andMap (V.field "announcements" (V.list Announcement.validation))


initEmptyForm : Form FormError LoginScreenSettings
initEmptyForm =
    Form.initial [] validation


initForm : LoginScreenSettings -> Form FormError LoginScreenSettings
initForm settings =
    Form.initial
        [ ( "loginInfo", Field.maybeString settings.loginInfo )
        , ( "loginInfoSidebar", Field.maybeString settings.loginInfoSidebar )
        , ( "announcements", Field.list (List.map (Field.group << Announcement.toFormInitials) settings.announcements) )
        ]
        validation
