module Wizard.Pages.Projects.Common.ProjectMigrationCreateForm exposing
    ( ProjectMigrationCreateForm
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


type alias ProjectMigrationCreateForm =
    { knowledgeModelPackageUuid : String
    }


initEmpty : Form FormError ProjectMigrationCreateForm
initEmpty =
    Form.initial [] validation


init : String -> Form FormError ProjectMigrationCreateForm
init kmPackageId =
    let
        initials =
            [ ( "knowledgeModelPackageUuid", Field.string kmPackageId ) ]
    in
    Form.initial initials validation


validation : Validation FormError ProjectMigrationCreateForm
validation =
    Validate.map ProjectMigrationCreateForm
        (Validate.field "knowledgeModelPackageUuid" Validate.string)


encode : List String -> ProjectMigrationCreateForm -> E.Value
encode tagUuids form =
    E.object
        [ ( "knowledgeModelPackageUuid", E.string form.knowledgeModelPackageUuid )
        , ( "targetTagUuids", E.list E.string tagUuids )
        ]
