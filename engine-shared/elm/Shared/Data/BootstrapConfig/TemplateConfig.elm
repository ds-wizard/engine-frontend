module Shared.Data.BootstrapConfig.TemplateConfig exposing
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
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Shared.Utils exposing (getOrganizationAndItemId)


type alias TemplateConfig =
    { recommendedTemplateId : Maybe String
    }


default : TemplateConfig
default =
    { recommendedTemplateId = Nothing }



-- JSON


decoder : Decoder TemplateConfig
decoder =
    D.succeed TemplateConfig
        |> D.required "recommendedTemplateId" (D.maybe D.string)


encode : TemplateConfig -> E.Value
encode config =
    E.object
        [ ( "recommendedTemplateId", E.maybe E.string config.recommendedTemplateId ) ]



-- Form


validation : Validation FormError TemplateConfig
validation =
    V.succeed TemplateConfig
        |> V.andMap (V.field "recommendedTemplateId" V.maybeString)


initEmptyForm : Form FormError TemplateConfig
initEmptyForm =
    Form.initial [] validation


initForm : TemplateConfig -> Form FormError TemplateConfig
initForm config =
    let
        fields =
            [ ( "recommendedTemplate", Field.maybeString (Maybe.map getOrganizationAndItemId config.recommendedTemplateId) )
            , ( "recommendedTemplateId", Field.maybeString config.recommendedTemplateId )
            ]
    in
    Form.initial fields validation
