module Questionnaires.Common.QuestionnaireMigrationCreateForm exposing
    ( QuestionnaireMigrationCreateForm
    , encode
    , initEmpty
    , validation
    )

import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E


type alias QuestionnaireMigrationCreateForm =
    { packageId : String
    }


initEmpty : Form CustomFormError QuestionnaireMigrationCreateForm
initEmpty =
    Form.initial [] validation


validation : Validation CustomFormError QuestionnaireMigrationCreateForm
validation =
    Validate.map QuestionnaireMigrationCreateForm
        (Validate.field "packageId" Validate.string)


encode : List String -> QuestionnaireMigrationCreateForm -> E.Value
encode tagUuids form =
    E.object
        [ ( "targetPackageId", E.string form.packageId )
        , ( "targetTagUuids", E.list E.string tagUuids )
        ]
