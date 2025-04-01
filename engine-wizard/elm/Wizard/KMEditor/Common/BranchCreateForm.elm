module Wizard.KMEditor.Common.BranchCreateForm exposing
    ( BranchCreateForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Maybe.Extra as Maybe
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Wizard.Common.AppState exposing (AppState)


type alias BranchCreateForm =
    { name : String
    , kmId : String
    , versionMajor : Int
    , versionMinor : Int
    , versionPatch : Int
    , previousPackageId : Maybe String
    }


init : AppState -> Maybe String -> Form FormError BranchCreateForm
init appState selectedPackage =
    let
        initials =
            case selectedPackage of
                Just packageId ->
                    [ ( "previousPackageId", Field.string packageId ) ]

                _ ->
                    []
    in
    Form.initial initials (validation appState)


validation : AppState -> Validation FormError BranchCreateForm
validation appState =
    V.succeed BranchCreateForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "kmId" (V.kmId appState))
        |> V.andMap (V.field "versionMajor" V.versionNumber)
        |> V.andMap (V.field "versionMinor" V.versionNumber)
        |> V.andMap (V.field "versionPatch" V.versionNumber)
        |> V.andMap (V.field "previousPackageId" V.maybeString)


encode : BranchCreateForm -> E.Value
encode form =
    let
        parentPackage =
            Maybe.unwrap E.null E.string form.previousPackageId

        version =
            String.join "." <| List.map String.fromInt [ form.versionMajor, form.versionMinor, form.versionPatch ]
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "kmId", E.string form.kmId )
        , ( "version", E.string version )
        , ( "previousPackageId", parentPackage )
        ]
