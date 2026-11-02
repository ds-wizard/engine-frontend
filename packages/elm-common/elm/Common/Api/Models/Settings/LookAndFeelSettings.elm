module Common.Api.Models.Settings.LookAndFeelSettings exposing
    ( LookAndFeelSettings
    , decoder
    , encode
    , initEmptyForm
    , initForm
    , section
    , validation
    )

{-| The look of an instance: its title and its color. The logo is a file, sent
and removed on its own (`Common.Api.Settings.putLogo`); `logoUrl` is where the
one in use is, and is only read.
-}

import Common.Utils.Form.Field as Field
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias LookAndFeelSettings =
    { appTitle : Maybe String
    , primaryColor : Maybe String
    , logoUrl : Maybe String
    }


section : { name : String, decoder : Decoder LookAndFeelSettings, encode : LookAndFeelSettings -> E.Value }
section =
    { name = "look-and-feel", decoder = decoder, encode = encode }



-- JSON


decoder : Decoder LookAndFeelSettings
decoder =
    D.succeed LookAndFeelSettings
        |> D.required "appTitle" (D.maybe D.string)
        |> D.required "primaryColor" (D.maybe D.string)
        |> D.required "logoUrl" (D.nullable D.string)


encode : LookAndFeelSettings -> E.Value
encode settings =
    E.object
        [ ( "appTitle", E.maybe E.string settings.appTitle )
        , ( "primaryColor", E.maybe E.string settings.primaryColor )
        , ( "logoUrl", E.maybe E.string settings.logoUrl )
        ]



-- Form


validation : Validation FormError LookAndFeelSettings
validation =
    V.succeed LookAndFeelSettings
        |> V.andMap (V.field "appTitle" V.maybeString)
        |> V.andMap (V.field "stylePrimaryColor" V.maybeString)
        |> V.andMap (V.field "logoUrl" V.maybeString)


initEmptyForm : Form FormError LookAndFeelSettings
initEmptyForm =
    Form.initial [] validation


initForm : LookAndFeelSettings -> Form FormError LookAndFeelSettings
initForm settings =
    Form.initial
        [ ( "appTitle", Field.maybeString settings.appTitle )
        , ( "stylePrimaryColor", Field.maybeString settings.primaryColor )
        , ( "logoUrl", Field.maybeString settings.logoUrl )
        ]
        validation
