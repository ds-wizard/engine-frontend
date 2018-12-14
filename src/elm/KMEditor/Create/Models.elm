module KMEditor.Create.Models exposing (KnowledgeModelCreateForm, Model, encodeKnowledgeCreateModelForm, initKnowledgeModelCreateForm, initialModel, knowledgeModelCreateFormValidation)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Encode as Encode exposing (..)
import KMPackages.Common.Models exposing (PackageDetail)
import Utils exposing (validateRegex)


type alias Model =
    { packages : ActionResult (List PackageDetail)
    , savingKnowledgeModel : ActionResult String
    , form : Form CustomFormError KnowledgeModelCreateForm
    , selectedPackage : Maybe String
    }


initialModel : Maybe String -> Model
initialModel selectedPackage =
    { packages = Loading
    , savingKnowledgeModel = Unset
    , form = initKnowledgeModelCreateForm Nothing
    , selectedPackage = selectedPackage
    }


type alias KnowledgeModelCreateForm =
    { name : String
    , kmId : String
    , parentPackageId : Maybe String
    }


initKnowledgeModelCreateForm : Maybe String -> Form CustomFormError KnowledgeModelCreateForm
initKnowledgeModelCreateForm selectedPackage =
    let
        initials =
            case selectedPackage of
                Just packageId ->
                    [ ( "parentPackageId", Field.string packageId ) ]

                _ ->
                    []
    in
    Form.initial initials knowledgeModelCreateFormValidation


knowledgeModelCreateFormValidation : Validation CustomFormError KnowledgeModelCreateForm
knowledgeModelCreateFormValidation =
    Validate.map3 KnowledgeModelCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "kmId" (validateRegex "^^(?![-])(?!.*[-]$)[a-zA-Z0-9-]+$"))
        (Validate.field "parentPackageId" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))


encodeKnowledgeCreateModelForm : KnowledgeModelCreateForm -> Encode.Value
encodeKnowledgeCreateModelForm form =
    let
        parentPackage =
            case form.parentPackageId of
                Just parentPackageId ->
                    Encode.string parentPackageId

                Nothing ->
                    Encode.null
    in
    Encode.object
        [ ( "name", Encode.string form.name )
        , ( "kmId", Encode.string form.kmId )
        , ( "parentPackageId", parentPackage )
        , ( "organizationId", Encode.string "" )
        ]
