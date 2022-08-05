module Wizard.Documents.Common.DocumentCreateForm exposing
    ( DocumentCreateForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import Shared.Data.TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)


type alias DocumentCreateForm =
    { name : String
    , templateId : String
    , formatUuid : String
    , questionnaireEventUuid : Maybe String
    }


init :
    { q | name : String, template : Maybe TemplateSuggestion, formatUuid : Maybe Uuid }
    -> Maybe Uuid
    -> Form FormError DocumentCreateForm
init questionnaire mbEventUuid =
    Form.initial
        [ ( "name", Field.string questionnaire.name )
        , ( "templateId", Field.string (Maybe.unwrap "" .id questionnaire.template) )
        , ( "formatUuid", Field.string (Maybe.unwrap "" Uuid.toString questionnaire.formatUuid) )
        , ( "questionnaireEventUuid", Field.string (Maybe.unwrap "" Uuid.toString mbEventUuid) )
        ]
        validation


validation : Validation FormError DocumentCreateForm
validation =
    Validate.map4 DocumentCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "templateId" Validate.string)
        (Validate.field "formatUuid" Validate.string)
        (Validate.field "questionnaireEventUuid" (Validate.maybe Validate.string))


encode : Uuid -> DocumentCreateForm -> E.Value
encode questionnaireUuid form =
    E.object
        [ ( "name", E.string form.name )
        , ( "questionnaireUuid", E.string (Uuid.toString questionnaireUuid) )
        , ( "templateId", E.string form.templateId )
        , ( "formatUuid", E.string form.formatUuid )
        , ( "questionnaireEventUuid", E.maybe E.string form.questionnaireEventUuid )
        ]
