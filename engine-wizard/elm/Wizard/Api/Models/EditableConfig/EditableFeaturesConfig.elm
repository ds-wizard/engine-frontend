module Wizard.Api.Models.EditableConfig.EditableFeaturesConfig exposing
    ( EditableFeaturesConfig
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
import Shared.Form.FormError exposing (FormError)


type alias EditableFeaturesConfig =
    { toursEnabled : Bool
    }



-- JSON


decoder : Decoder EditableFeaturesConfig
decoder =
    D.succeed EditableFeaturesConfig
        |> D.required "toursEnabled" D.bool


encode : EditableFeaturesConfig -> D.Value
encode config =
    E.object
        [ ( "toursEnabled", E.bool config.toursEnabled )
        ]



-- Form


validation : Validation FormError EditableFeaturesConfig
validation =
    V.succeed EditableFeaturesConfig
        |> V.andMap (V.field "toursEnabled" V.bool)


initEmptyForm : Form FormError EditableFeaturesConfig
initEmptyForm =
    Form.initial [] validation


initForm : EditableFeaturesConfig -> Form FormError EditableFeaturesConfig
initForm config =
    let
        initials =
            [ ( "toursEnabled", Field.bool config.toursEnabled ) ]
    in
    Form.initial initials validation
