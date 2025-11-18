module Wizard.Pages.KMEditor.Common.KnowledgeModelEditorPublishForm exposing
    ( KnowledgeModelEditorPublishForm
    , encode
    , init
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as Validate
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import String exposing (fromInt)
import Uuid exposing (Uuid)


type alias KnowledgeModelEditorPublishForm =
    { major : Int
    , minor : Int
    , patch : Int
    , description : String
    , readme : String
    }


init : Form FormError KnowledgeModelEditorPublishForm
init =
    Form.initial [] validation


validation : Validation FormError KnowledgeModelEditorPublishForm
validation =
    Validate.map5 KnowledgeModelEditorPublishForm
        (Validate.field "major" Validate.versionNumber)
        (Validate.field "minor" Validate.versionNumber)
        (Validate.field "patch" Validate.versionNumber)
        (Validate.field "description" Validate.string)
        (Validate.field "readme" Validate.string)


encode : Uuid -> KnowledgeModelEditorPublishForm -> E.Value
encode kmEditorUuid form =
    let
        version =
            String.join "." <| List.map fromInt [ form.major, form.minor, form.patch ]
    in
    E.object
        [ ( "editorUuid", Uuid.encode kmEditorUuid )
        , ( "version", E.string version )
        , ( "description", E.string form.description )
        , ( "readme", E.string form.readme )
        ]
