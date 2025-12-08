module Wizard.Pages.Projects.Common.ProjectShareForm exposing
    ( ProjectShareForm
    , encode
    , getMemberUuids
    , init
    , initEmpty
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Wizard.Api.Models.Member as Member
import Wizard.Api.Models.Project.ProjectSharing as ProjectSharing
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility
import Wizard.Api.Models.ProjectCommon exposing (ProjectCommon)
import Wizard.Api.Models.ProjectPermission as ProjectPermission exposing (ProjectPermission)
import Wizard.Pages.Projects.Common.ProjectShareFormPermission as ProjectShareFormPermission exposing (ProjectShareFormPermission)


type alias ProjectShareForm =
    { visibilityEnabled : Bool
    , visibilityPermission : ProjectPermission
    , sharingEnabled : Bool
    , sharingPermission : ProjectPermission
    , permissions : List ProjectShareFormPermission
    }


initEmpty : Form FormError ProjectShareForm
initEmpty =
    Form.initial [] validation


init : ProjectCommon -> Form FormError ProjectShareForm
init project =
    Form.initial (projectToFormInitials project) validation


getMemberUuids : Form FormError ProjectShareForm -> List String
getMemberUuids form =
    let
        indexes =
            Form.getListIndexes "permissions" form

        toMemberUuid index =
            Maybe.withDefault "" (Form.getFieldAsString ("permissions." ++ String.fromInt index ++ ".memberUuid") form).value
    in
    List.map toMemberUuid indexes


projectToFormInitials : ProjectCommon -> List ( String, Field.Field )
projectToFormInitials project =
    let
        ( visibilityEnabled, visibilityPermission ) =
            ProjectVisibility.toFormValues project.visibility

        ( sharingEnabled, sharingPermission ) =
            ProjectSharing.toFormValues project.sharing

        permissionFields =
            List.map ProjectShareFormPermission.initFromPermission <|
                List.sortWith (\p1 p2 -> Member.compare p1.member p2.member) project.permissions
    in
    [ ( "visibilityEnabled", Field.bool visibilityEnabled )
    , ( "visibilityPermission", ProjectPermission.field visibilityPermission )
    , ( "sharingEnabled", Field.bool sharingEnabled )
    , ( "sharingPermission", ProjectPermission.field sharingPermission )
    , ( "permissions", Field.list permissionFields )
    ]


validation : Validation FormError ProjectShareForm
validation =
    V.succeed ProjectShareForm
        |> V.andMap (V.field "visibilityEnabled" V.bool)
        |> V.andMap (V.field "visibilityPermission" ProjectPermission.validation)
        |> V.andMap (V.field "sharingEnabled" V.bool)
        |> V.andMap (V.field "sharingPermission" ProjectPermission.validation)
        |> V.andMap (V.field "permissions" (V.list ProjectShareFormPermission.validation))


encode : ProjectShareForm -> E.Value
encode form =
    E.object
        [ ( "visibility", ProjectVisibility.encode (ProjectVisibility.fromFormValues form.visibilityEnabled form.visibilityPermission form.sharingEnabled form.sharingPermission) )
        , ( "sharing", ProjectSharing.encode (ProjectSharing.fromFormValues form.sharingEnabled form.sharingPermission) )
        , ( "permissions", E.list ProjectShareFormPermission.encode form.permissions )
        ]
