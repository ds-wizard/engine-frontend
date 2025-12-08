module Wizard.Pages.Projects.Common.ProjectCreateForm exposing
    ( ProjectCreateForm
    , ValidationMode(..)
    , encodeFromPackage
    , encodeFromTemplate
    , init
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.Project.ProjectSharing as ProjectSharing
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility
import Wizard.Api.Models.ProjectPermission as ProjectPermission exposing (ProjectPermission)
import Wizard.Data.AppState exposing (AppState)


type alias ProjectCreateForm =
    { name : String
    , visibilityEnabled : Bool
    , visibilityPermission : ProjectPermission
    , sharingEnabled : Bool
    , sharingPermission : ProjectPermission
    , knowledgeModelPackageId : String
    , templateId : String
    }


init : AppState -> ValidationMode -> Maybe Uuid -> Maybe String -> Form FormError ProjectCreateForm
init appState validationMode selectedTemplateUuid selectedPackageId =
    let
        toField fieldName mbFieldValue =
            case mbFieldValue of
                Just fieldValue ->
                    [ ( fieldName, Field.string fieldValue ) ]

                _ ->
                    []

        ( visibilityEnabled, visibilityPermission ) =
            ProjectVisibility.toFormValues appState.config.project.projectVisibility.defaultValue

        ( sharingEnabled, sharingPermission ) =
            ProjectSharing.toFormValues appState.config.project.projectSharing.defaultValue

        initials =
            [ ( "visibilityEnabled", Field.bool visibilityEnabled )
            , ( "visibilityPermission", ProjectPermission.field visibilityPermission )
            , ( "sharingEnabled", Field.bool sharingEnabled )
            , ( "sharingPermission", ProjectPermission.field sharingPermission )
            ]
                ++ toField "templateId" (Maybe.map Uuid.toString selectedTemplateUuid)
                ++ toField "knowledgeModelPackageId" selectedPackageId
    in
    Form.initial initials (validation validationMode)


type ValidationMode
    = TemplateValidationMode
    | PackageValidationMode


validation : ValidationMode -> Validation FormError ProjectCreateForm
validation validationMode =
    let
        validationBase =
            V.succeed ProjectCreateForm
                |> V.andMap (V.field "name" V.string)
                |> V.andMap (V.field "visibilityEnabled" V.bool)
                |> V.andMap (V.field "visibilityPermission" ProjectPermission.validation)
                |> V.andMap (V.field "sharingEnabled" V.bool)
                |> V.andMap (V.field "sharingPermission" ProjectPermission.validation)
    in
    case validationMode of
        TemplateValidationMode ->
            validationBase
                |> V.andMap (V.succeed "")
                |> V.andMap (V.field "templateId" V.string)

        PackageValidationMode ->
            validationBase
                |> V.andMap (V.field "knowledgeModelPackageId" V.string)
                |> V.andMap (V.succeed "")


encodeFromPackage : List String -> ProjectCreateForm -> E.Value
encodeFromPackage questionTagUuids form =
    E.object
        [ ( "name", E.string form.name )
        , ( "knowledgeModelPackageId", E.string form.knowledgeModelPackageId )
        , ( "visibility", ProjectVisibility.encode (ProjectVisibility.fromFormValues form.visibilityEnabled form.visibilityPermission form.sharingEnabled form.sharingPermission) )
        , ( "sharing", ProjectSharing.encode (ProjectSharing.fromFormValues form.sharingEnabled form.sharingPermission) )
        , ( "questionTagUuids", E.list E.string questionTagUuids )
        , ( "templateId", E.maybe E.string Nothing )
        ]


encodeFromTemplate : ProjectCreateForm -> E.Value
encodeFromTemplate form =
    E.object
        [ ( "name", E.string form.name )
        , ( "projectUuid", E.string form.templateId )
        ]
