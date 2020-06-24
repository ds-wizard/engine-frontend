module Wizard.KMEditor.Common.BranchPublishForm exposing
    ( BranchPublishForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Json.Encode as E exposing (..)
import Shared.Form.FormError exposing (FormError)
import String exposing (fromInt)


type alias BranchPublishForm =
    { major : Int
    , minor : Int
    , patch : Int
    , description : String
    , readme : String
    , license : String
    }


init : Form FormError BranchPublishForm
init =
    Form.initial [] validation


validation : Validation FormError BranchPublishForm
validation =
    Validate.map6 BranchPublishForm
        (Validate.field "major" (Validate.int |> Validate.andThen (Validate.minInt 0)))
        (Validate.field "minor" (Validate.int |> Validate.andThen (Validate.minInt 0)))
        (Validate.field "patch" (Validate.int |> Validate.andThen (Validate.minInt 0)))
        (Validate.field "description" Validate.string)
        (Validate.field "readme" Validate.string)
        (Validate.field "license" Validate.string)


encode : BranchPublishForm -> ( String, E.Value )
encode form =
    let
        version =
            String.join "." <| List.map fromInt [ form.major, form.minor, form.patch ]

        object =
            E.object
                [ ( "description", E.string form.description )
                , ( "readme", E.string form.readme )
                , ( "license", E.string form.license )
                ]
    in
    ( version, object )
