module Wizard.Pages.Projects.Common.ProjectSettingsForm exposing
    ( ProjectSettingsForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import Uuid
import Wizard.Api.Models.ProjectSettings exposing (ProjectSettings)
import Wizard.Data.AppState exposing (AppState)


type alias ProjectSettingsForm =
    { name : String
    , description : Maybe String
    , projectTags : List String
    , isTemplate : Bool
    , documentTemplateId : Maybe String
    , formatUuid : Maybe String
    }


initEmpty : AppState -> Form FormError ProjectSettingsForm
initEmpty appState =
    Form.initial [] (validation appState)


init : AppState -> ProjectSettings -> Form FormError ProjectSettingsForm
init appState project =
    let
        initials =
            [ ( "name", Field.string project.name )
            , ( "description", Field.string (Maybe.withDefault "" project.description) )
            , ( "projectTags", Field.list (List.map Field.string project.projectTags ++ [ Field.string "" ]) )
            , ( "isTemplate", Field.bool project.isTemplate )
            , ( "documentTemplateId", Field.string (Maybe.unwrap "" .id project.documentTemplate) )
            , ( "formatUuid", Field.string (Maybe.unwrap "" Uuid.toString project.formatUuid) )
            ]
    in
    Form.initial initials (validation appState)


validation : AppState -> Validation FormError ProjectSettingsForm
validation appState =
    V.succeed ProjectSettingsForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" (V.maybe V.string))
        |> V.andMap (V.field "projectTags" (V.list (V.oneOf [ V.emptyString, V.projectTag appState ])))
        |> V.andMap (V.field "isTemplate" V.bool)
        |> V.andMap (V.field "documentTemplateId" (V.maybe V.string))
        |> V.andMap (V.field "formatUuid" (V.maybe V.string))


encode : ProjectSettingsForm -> E.Value
encode form =
    let
        formatUuid =
            Maybe.andThen (always form.formatUuid) form.documentTemplateId

        projectTags =
            form.projectTags
                |> List.filter (not << String.isEmpty)
                |> List.sortBy String.toUpper
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "description", E.maybe E.string form.description )
        , ( "projectTags", E.list E.string projectTags )
        , ( "isTemplate", E.bool form.isTemplate )
        , ( "documentTemplateId", E.maybe E.string form.documentTemplateId )
        , ( "formatUuid", E.maybe E.string formatUuid )
        ]
