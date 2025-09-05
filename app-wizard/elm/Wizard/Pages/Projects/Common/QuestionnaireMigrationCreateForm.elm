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
    { packageId : String
    }


initEmpty : Form FormError QuestionnaireMigrationCreateForm
initEmpty =
    Form.initial [] validation


init : String -> Form FormError QuestionnaireMigrationCreateForm
init packageId =
    let
        initials =
            [ ( "packageId", Field.string packageId ) ]
    in
    Form.initial initials validation


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
