module Wizard.Documents.Common.DocumentCreateForm exposing
    ( DocumentCreateForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)


type alias DocumentCreateForm =
    { name : String
    , templateId : String
    , formatUuid : String
    }


init : Form FormError DocumentCreateForm
init =
    Form.initial [] validation


validation : Validation FormError DocumentCreateForm
validation =
    Validate.map3 DocumentCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "templateId" Validate.string)
        (Validate.field "formatUuid" Validate.string)


encode : Uuid -> DocumentCreateForm -> E.Value
encode questionnaireUuid form =
    E.object
        [ ( "name", E.string form.name )
        , ( "questionnaireUuid", E.string (Uuid.toString questionnaireUuid) )
        , ( "templateId", E.string form.templateId )
        , ( "formatUuid", E.string form.formatUuid )
        ]
