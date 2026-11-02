module Common.Api.Models.Settings.FeaturesSettings exposing
    ( FeaturesSettings
    , decoder
    , encode
    , initEmptyForm
    , initForm
    , section
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias FeaturesSettings =
    { aiAssistantEnabled : Bool
    , toursEnabled : Bool
    }


section : { name : String, decoder : Decoder FeaturesSettings, encode : FeaturesSettings -> E.Value }
section =
    { name = "features", decoder = decoder, encode = encode }



-- JSON


decoder : Decoder FeaturesSettings
decoder =
    D.succeed FeaturesSettings
        |> D.optional "aiAssistantEnabled" D.bool False
        |> D.required "toursEnabled" D.bool


encode : FeaturesSettings -> D.Value
encode settings =
    E.object
        [ ( "aiAssistantEnabled", E.bool settings.aiAssistantEnabled )
        , ( "toursEnabled", E.bool settings.toursEnabled )
        ]



-- Form


validation : Validation FormError FeaturesSettings
validation =
    V.succeed FeaturesSettings
        |> V.andMap (V.field "aiAssistantEnabled" V.bool)
        |> V.andMap (V.field "toursEnabled" V.bool)


initEmptyForm : Form FormError FeaturesSettings
initEmptyForm =
    Form.initial [] validation


initForm : FeaturesSettings -> Form FormError FeaturesSettings
initForm settings =
    Form.initial
        [ ( "aiAssistantEnabled", Field.bool settings.aiAssistantEnabled )
        , ( "toursEnabled", Field.bool settings.toursEnabled )
        ]
        validation
