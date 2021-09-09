module Wizard.KMEditor.Common.BranchUpgradeForm exposing
    ( BranchUpgradeForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Shared.Form.FormError exposing (FormError)


type alias BranchUpgradeForm =
    { targetPackageId : String }


init : Form FormError BranchUpgradeForm
init =
    Form.initial [] validation


validation : Validation FormError BranchUpgradeForm
validation =
    Validate.map BranchUpgradeForm
        (Validate.field "targetPackageId" Validate.string)


encode : BranchUpgradeForm -> E.Value
encode form =
    E.object
        [ ( "targetPackageId", E.string form.targetPackageId ) ]
