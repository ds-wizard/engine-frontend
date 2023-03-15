module Wizard.KMEditor.Common.BranchPublishForm exposing
    ( BranchPublishForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as Validate
import String exposing (fromInt)
import Uuid exposing (Uuid)


type alias BranchPublishForm =
    { major : Int
    , minor : Int
    , patch : Int
    , description : String
    , readme : String
    }


init : Form FormError BranchPublishForm
init =
    Form.initial [] validation


validation : Validation FormError BranchPublishForm
validation =
    Validate.map5 BranchPublishForm
        (Validate.field "major" Validate.versionNumber)
        (Validate.field "minor" Validate.versionNumber)
        (Validate.field "patch" Validate.versionNumber)
        (Validate.field "description" Validate.string)
        (Validate.field "readme" Validate.string)


encode : Uuid -> BranchPublishForm -> E.Value
encode branchUuid form =
    let
        version =
            String.join "." <| List.map fromInt [ form.major, form.minor, form.patch ]
    in
    E.object
        [ ( "branchUuid", Uuid.encode branchUuid )
        , ( "version", E.string version )
        , ( "description", E.string form.description )
        , ( "readme", E.string form.readme )
        ]
