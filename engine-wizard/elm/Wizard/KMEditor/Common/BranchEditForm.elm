module Wizard.KMEditor.Common.BranchEditForm exposing
    ( BranchEditForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.Form.Validate as V
import Version
import Wizard.Api.Models.BranchDetail exposing (BranchDetail)
import Wizard.Common.AppState exposing (AppState)


type alias BranchEditForm =
    { name : String
    , description : String
    , kmId : String
    , versionMajor : Int
    , versionMinor : Int
    , versionPatch : Int
    , license : String
    , readme : String
    }


init : AppState -> BranchDetail -> Form FormError BranchEditForm
init appState branch =
    let
        initials =
            [ ( "name", Field.string branch.name )
            , ( "description", Field.string branch.description )
            , ( "kmId", Field.string branch.kmId )
            , ( "versionMajor", Field.string (String.fromInt (Version.getMajor branch.version)) )
            , ( "versionMinor", Field.string (String.fromInt (Version.getMinor branch.version)) )
            , ( "versionPatch", Field.string (String.fromInt (Version.getPatch branch.version)) )
            , ( "license", Field.string branch.license )
            , ( "readme", Field.string branch.readme )
            ]
    in
    Form.initial initials (validation appState)


initEmpty : AppState -> Form FormError BranchEditForm
initEmpty appState =
    Form.initial [] (validation appState)


validation : AppState -> Validation FormError BranchEditForm
validation appState =
    V.succeed BranchEditForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" V.string)
        |> V.andMap (V.field "kmId" (V.kmId appState))
        |> V.andMap (V.field "versionMajor" V.versionNumber)
        |> V.andMap (V.field "versionMinor" V.versionNumber)
        |> V.andMap (V.field "versionPatch" V.versionNumber)
        |> V.andMap (V.field "license" V.string)
        |> V.andMap (V.field "readme" V.string)


encode : BranchEditForm -> E.Value
encode form =
    let
        version =
            String.join "." <| List.map String.fromInt [ form.versionMajor, form.versionMinor, form.versionPatch ]
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "description", E.string form.description )
        , ( "kmId", E.string form.kmId )
        , ( "version", E.string version )
        , ( "license", E.string form.license )
        , ( "readme", E.string form.readme )
        ]
