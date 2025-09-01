module Wizard.Api.Models.EditableConfig.EditableRegistryConfig exposing
    ( EditableRegistryConfig
    , decoder
    , encode
    , initEmptyForm
    , initForm
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.Form.Validate as V


type alias EditableRegistryConfig =
    { enabled : Bool
    , token : String
    }



-- JSON


decoder : Decoder EditableRegistryConfig
decoder =
    D.succeed EditableRegistryConfig
        |> D.required "enabled" D.bool
        |> D.required "token" D.string


encode : EditableRegistryConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "token", E.string config.token )
        ]



-- Form


validation : Validation FormError EditableRegistryConfig
validation =
    V.succeed EditableRegistryConfig
        |> V.andMap (V.field "enabled" V.bool)
        |> V.andMap (V.field "enabled" V.bool |> V.ifElse "token" V.string V.optionalString)


initEmptyForm : Form FormError EditableRegistryConfig
initEmptyForm =
    Form.initial [] validation


initForm : EditableRegistryConfig -> Form FormError EditableRegistryConfig
initForm config =
    let
        fields =
            [ ( "enabled", Field.bool config.enabled )
            , ( "token", Field.string config.token )
            ]
    in
    Form.initial fields validation
