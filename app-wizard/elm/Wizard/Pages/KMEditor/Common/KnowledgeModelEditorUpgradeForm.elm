module Wizard.Pages.KMEditor.Common.KnowledgeModelEditorUpgradeForm exposing
    ( KnowledgeModelEditorUpgradeForm
    , encode
    , init
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E


type alias KnowledgeModelEditorUpgradeForm =
    { targetPackageId : String }


init : Form FormError KnowledgeModelEditorUpgradeForm
init =
    Form.initial [] validation


validation : Validation FormError KnowledgeModelEditorUpgradeForm
validation =
    Validate.map KnowledgeModelEditorUpgradeForm
        (Validate.field "targetPackageId" Validate.string)


encode : KnowledgeModelEditorUpgradeForm -> E.Value
encode form =
    E.object
        [ ( "targetPackageId", E.string form.targetPackageId ) ]
