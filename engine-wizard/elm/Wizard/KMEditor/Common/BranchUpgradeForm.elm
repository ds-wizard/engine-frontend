module Wizard.KMEditor.Common.BranchUpgradeForm exposing
    ( BranchUpgradeForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Json.Encode as E exposing (..)
import Wizard.Common.Form exposing (CustomFormError)


type alias BranchUpgradeForm =
    { targetPackageId : String }


init : Form CustomFormError BranchUpgradeForm
init =
    Form.initial [] validation


validation : Validation CustomFormError BranchUpgradeForm
validation =
    Validate.map BranchUpgradeForm
        (Validate.field "targetPackageId" Validate.string)


encode : BranchUpgradeForm -> E.Value
encode form =
    E.object
        [ ( "targetPackageId", E.string form.targetPackageId ) ]
