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
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias BranchCreateForm =
    { name : String
    , kmId : String
    , previousPackageId : Maybe String
    }


init : Maybe String -> Form FormError BranchCreateForm
init selectedPackage =
    let
        initials =
            case selectedPackage of
                Just packageId ->
                    [ ( "previousPackageId", Field.string packageId ) ]

                _ ->
                    []
    in
    Form.initial initials validation


validation : Validation FormError BranchCreateForm
validation =
    V.map3 BranchCreateForm
        (V.field "name" V.string)
        (V.field "kmId" V.kmId)
        (V.field "previousPackageId" (V.oneOf [ V.emptyString |> V.map (\_ -> Nothing), V.string |> V.map Just ]))


encode : BranchCreateForm -> E.Value
encode form =
    let
        parentPackage =
            case form.previousPackageId of
                Just previousPackageId ->
                    E.string previousPackageId

                Nothing ->
                    E.null
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "kmId", E.string form.kmId )
        , ( "previousPackageId", parentPackage )
        ]
