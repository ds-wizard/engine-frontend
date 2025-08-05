module Wizard.Common.Components.Questionnaire.VersionForm exposing
    ( VersionForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Form
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireVersion exposing (QuestionnaireVersion)


type alias VersionForm =
    { name : String
    , description : Maybe String
    }


initEmpty : Form.Form FormError VersionForm
initEmpty =
    Form.initial [] validation


init : QuestionnaireVersion -> Form.Form FormError VersionForm
init version =
    let
        initials =
            [ ( "name", Field.string version.name )
            , ( "description", Field.string (Maybe.withDefault "" version.description) )
            ]
    in
    Form.initial initials validation


validation : Validation FormError VersionForm
validation =
    Validate.map2 VersionForm
        (Validate.field "name" Validate.string)
        (Validate.field "description" (Validate.maybe Validate.string))


encode : Uuid -> VersionForm -> E.Value
encode eventUuid form =
    E.object
        [ ( "eventUuid", Uuid.encode eventUuid )
        , ( "name", E.string form.name )
        , ( "description", E.maybe E.string form.description )
        ]
