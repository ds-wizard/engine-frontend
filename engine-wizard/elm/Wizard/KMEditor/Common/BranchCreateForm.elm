module Wizard.KMEditor.Common.BranchCreateForm exposing
    ( BranchCreateForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Encode as E exposing (..)
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as Validate


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
    Validate.map3 BranchCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "kmId" (Validate.regex "^^(?![-])(?!.*[-]$)[a-zA-Z0-9-]+$"))
        (Validate.field "previousPackageId" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))


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
