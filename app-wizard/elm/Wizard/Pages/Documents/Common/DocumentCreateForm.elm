module Wizard.Pages.Documents.Common.DocumentCreateForm exposing
    ( DocumentCreateForm
    , encode
    , init
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)


type alias DocumentCreateForm =
    { name : String
    , documentTemplateId : String
    , formatUuid : String
    , questionnaireEventUuid : Maybe String
    }


init :
    { q | name : String, documentTemplate : Maybe DocumentTemplateSuggestion, formatUuid : Maybe Uuid }
    -> Maybe Uuid
    -> Form FormError DocumentCreateForm
init questionnaire mbEventUuid =
    Form.initial
        [ ( "name", Field.string questionnaire.name )
        , ( "documentTemplateId", Field.string (Maybe.unwrap "" .id questionnaire.documentTemplate) )
        , ( "formatUuid", Field.string (Maybe.unwrap "" Uuid.toString questionnaire.formatUuid) )
        , ( "questionnaireEventUuid", Field.string (Maybe.unwrap "" Uuid.toString mbEventUuid) )
        ]
        validation


validation : Validation FormError DocumentCreateForm
validation =
    Validate.map4 DocumentCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "documentTemplateId" Validate.string)
        (Validate.field "formatUuid" Validate.string)
        (Validate.field "questionnaireEventUuid" (Validate.maybe Validate.string))


encode : Uuid -> DocumentCreateForm -> E.Value
encode questionnaireUuid form =
    E.object
        [ ( "name", E.string form.name )
        , ( "questionnaireUuid", E.string (Uuid.toString questionnaireUuid) )
        , ( "documentTemplateId", E.string form.documentTemplateId )
        , ( "formatUuid", E.string form.formatUuid )
        , ( "questionnaireEventUuid", E.maybe E.string form.questionnaireEventUuid )
        ]
