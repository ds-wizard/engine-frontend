module Common.Api.Models.Settings.RegistrySettings exposing
    ( RegistrySettings
    , decoder
    , encode
    , initEmptyForm
    , initForm
    , section
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias RegistrySettings =
    { enabled : Bool
    , apiKey : String
    }


section : { name : String, decoder : Decoder RegistrySettings, encode : RegistrySettings -> E.Value }
section =
    { name = "registry", decoder = decoder, encode = encode }



-- JSON


decoder : Decoder RegistrySettings
decoder =
    D.succeed RegistrySettings
        |> D.required "enabled" D.bool
        |> D.required "apiKey" D.string


encode : RegistrySettings -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "apiKey", E.string config.apiKey )
        ]



-- Form


validation : Validation FormError RegistrySettings
validation =
    V.succeed RegistrySettings
        |> V.andMap (V.field "enabled" V.bool)
        |> V.andMap (V.field "enabled" V.bool |> V.ifElse "apiKey" V.string V.optionalString)


initEmptyForm : Form FormError RegistrySettings
initEmptyForm =
    Form.initial [] validation


initForm : RegistrySettings -> Form FormError RegistrySettings
initForm config =
    let
        fields =
            [ ( "enabled", Field.bool config.enabled )
            , ( "apiKey", Field.string config.apiKey )
            ]
    in
    Form.initial fields validation
