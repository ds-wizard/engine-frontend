module KMEditor.Common.BranchCreateForm exposing
    ( BranchCreateForm
    , encode
    , init
    , validation
    )

import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Encode as E exposing (..)
import Utils exposing (validateRegex)


type alias BranchCreateForm =
    { name : String
    , kmId : String
    , previousPackageId : Maybe String
    }


init : Maybe String -> Form CustomFormError BranchCreateForm
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


validation : Validation CustomFormError BranchCreateForm
validation =
    Validate.map3 BranchCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "kmId" (validateRegex "^^(?![-])(?!.*[-]$)[a-zA-Z0-9-]+$"))
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
