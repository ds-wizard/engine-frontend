module Wizard.Questionnaires.Common.QuestionnaireMigrationCreateForm exposing
    ( QuestionnaireMigrationCreateForm
    , encode
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Shared.Form.FormError exposing (FormError)


type alias QuestionnaireMigrationCreateForm =
    { packageId : String
    }


initEmpty : Form FormError QuestionnaireMigrationCreateForm
initEmpty =
    Form.initial [] validation


validation : Validation FormError QuestionnaireMigrationCreateForm
validation =
    Validate.map QuestionnaireMigrationCreateForm
        (Validate.field "packageId" Validate.string)


encode : List String -> QuestionnaireMigrationCreateForm -> E.Value
encode tagUuids form =
    E.object
        [ ( "targetPackageId", E.string form.packageId )
        , ( "targetTagUuids", E.list E.string tagUuids )
        ]
