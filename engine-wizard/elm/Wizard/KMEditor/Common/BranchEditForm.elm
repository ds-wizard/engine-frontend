module Wizard.KMEditor.Common.BranchEditForm exposing
    ( BranchEditForm
    , init
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Shared.Data.BranchDetail exposing (BranchDetail)
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias BranchEditForm =
    { name : String
    , kmId : String
    }


init : BranchDetail -> Form FormError BranchEditForm
init branch =
    let
        initials =
            [ ( "name", Field.string branch.name )
            , ( "kmId", Field.string branch.kmId )
            ]
    in
    Form.initial initials validation


initEmpty : Form FormError BranchEditForm
initEmpty =
    Form.initial [] validation


validation : Validation FormError BranchEditForm
validation =
    V.map2 BranchEditForm
        (V.field "name" V.string)
        (V.field "kmId" V.kmId)
