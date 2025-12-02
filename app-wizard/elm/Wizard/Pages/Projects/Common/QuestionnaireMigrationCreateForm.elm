module Wizard.Pages.Projects.Common.QuestionnaireMigrationCreateForm exposing
    ( QuestionnaireMigrationCreateForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E


type alias QuestionnaireMigrationCreateForm =
    { knowledgeModelPackageId : String
    }


initEmpty : Form FormError QuestionnaireMigrationCreateForm
initEmpty =
    Form.initial [] validation


init : String -> Form FormError QuestionnaireMigrationCreateForm
init kmPackageId =
    let
        initials =
            [ ( "knowledgeModelPackageId", Field.string kmPackageId ) ]
    in
    Form.initial initials validation


validation : Validation FormError QuestionnaireMigrationCreateForm
validation =
    Validate.map QuestionnaireMigrationCreateForm
        (Validate.field "knowledgeModelPackageId" Validate.string)


encode : List String -> QuestionnaireMigrationCreateForm -> E.Value
encode tagUuids form =
    E.object
        [ ( "targetKnowledgeModelPackageId", E.string form.knowledgeModelPackageId )
        , ( "targetTagUuids", E.list E.string tagUuids )
        ]
