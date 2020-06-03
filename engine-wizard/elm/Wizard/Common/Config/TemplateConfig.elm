module Wizard.Common.Config.TemplateConfig exposing
    ( TemplateConfig
    , decoder
    , default
    , encode
    , initEmptyForm
    , initForm
    , validation
    )

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Field as Field
import Wizard.Common.Form.Validate as V


type alias TemplateConfig =
    { recommendedTemplateUuid : Maybe String
    }


default : TemplateConfig
default =
    { recommendedTemplateUuid = Nothing }



-- JSON


decoder : Decoder TemplateConfig
decoder =
    D.succeed TemplateConfig
        |> D.required "recommendedTemplateUuid" (D.maybe D.string)


encode : TemplateConfig -> E.Value
encode config =
    E.object
        [ ( "recommendedTemplateUuid", E.maybe E.string config.recommendedTemplateUuid ) ]



-- Form


validation : Validation CustomFormError TemplateConfig
validation =
    V.succeed TemplateConfig
        |> V.andMap (V.field "recommendedTemplateUuid" V.maybeString)


initEmptyForm : Form CustomFormError TemplateConfig
initEmptyForm =
    Form.initial [] validation


initForm : TemplateConfig -> Form CustomFormError TemplateConfig
initForm config =
    let
        fields =
            [ ( "recommendedTemplateUuid", Field.maybeString config.recommendedTemplateUuid ) ]
    in
    Form.initial fields validation
