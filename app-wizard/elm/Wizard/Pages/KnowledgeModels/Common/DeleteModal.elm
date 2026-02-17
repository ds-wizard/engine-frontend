module Wizard.Pages.KnowledgeModels.Common.DeleteModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , open
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.ActionResultBlock as ActionResultBlock
import Common.Components.Badge as Badge
import Common.Components.Flash as Flash
import Common.Components.Modal as Modal
import Common.Ports.Dom as Dom
import Gettext exposing (gettext)
import Html exposing (Html, div, input, label, li, p, strong, text, ul)
import Html.Attributes exposing (class, id, target)
import Html.Events exposing (onInput)
import Html.Extra as Html
import String.Format as String
import Version
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackages
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Api.Models.KnowledgeModelPackageDeletionImpact exposing (KnowledgeModelPackageDeletionImpact)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Routes as Routes


type alias Model =
    { kmPackageToBeDeleted : Maybe KnowledgeModelPackage
    , kmDependents : ActionResult (List KnowledgeModelPackageDeletionImpact)
    , allVersions : Bool
    , continueDelete : Bool
    , deletingKmPackage : ActionResult String
    , confirmInputValue : String
    }


initialModel : Bool -> Model
initialModel allVersions =
    { kmPackageToBeDeleted = Nothing
    , kmDependents = ActionResult.Unset
    , allVersions = allVersions
    , continueDelete = False
    , deletingKmPackage = ActionResult.Unset
    , confirmInputValue = ""
    }


type Msg
    = Open KnowledgeModelPackage
    | Close
    | GetDependentsCompleted (Result ApiError (List KnowledgeModelPackageDeletionImpact))
    | ContinueDelete
    | UpdateConfirmInputValue String
    | Delete
    | DeleteCompleted (Result ApiError ())


open : KnowledgeModelPackage -> Msg
open =
    Open


type alias UpdateConfig msg =
    { afterDeleteCmd : Cmd msg
    , wrapMsg : Msg -> msg
    }


update : AppState -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update appState cfg msg model =
    case msg of
        Open kmPackage ->
            ( { model | kmPackageToBeDeleted = Just kmPackage, kmDependents = ActionResult.Loading }
            , Cmd.map cfg.wrapMsg <| KnowledgeModelPackages.getKnowledgeModelPackageDependents appState kmPackage.uuid model.allVersions GetDependentsCompleted
            )

        Close ->
            ( initialModel model.allVersions, Cmd.none )

        GetDependentsCompleted result ->
            case result of
                Ok dependents ->
                    ( { model | kmDependents = ActionResult.Success dependents }, Cmd.none )

                Err error ->
                    ( { model | kmDependents = ApiError.toActionResult appState (gettext "Unable to get dependents of the Knowledge Model." appState.locale) error }
                    , Cmd.none
                    )

        ContinueDelete ->
            ( { model | continueDelete = True }
            , Dom.focus "#km-delete-confirm"
            )

        UpdateConfirmInputValue value ->
            ( { model | confirmInputValue = value }, Cmd.none )

        Delete ->
            case model.kmPackageToBeDeleted of
                Just kmPackage ->
                    ( { model | deletingKmPackage = ActionResult.Loading }
                    , Cmd.map cfg.wrapMsg <| KnowledgeModelPackages.deleteKnowledgeModelPackage appState kmPackage.uuid model.allVersions DeleteCompleted
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteCompleted result ->
            case result of
                Ok _ ->
                    ( initialModel model.allVersions
                    , cfg.afterDeleteCmd
                    )

                Err error ->
                    ( { model | deletingKmPackage = ApiError.toActionResult appState (gettext "Knowledge Model could not be deleted." appState.locale) error }
                    , Cmd.none
                    )


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( visible, kmLabel ) =
            case model.kmPackageToBeDeleted of
                Just kmPackage ->
                    ( True
                    , if model.allVersions then
                        kmPackage.organizationId ++ ":" ++ kmPackage.kmId

                      else
                        kmPackage.organizationId ++ ":" ++ kmPackage.kmId ++ ":" ++ Version.toString kmPackage.version
                    )

                Nothing ->
                    ( False, "" )

        modalContent =
            if model.continueDelete then
                [ Flash.warning (gettext "This action cannot be undone." appState.locale)
                , div [ class "form-group" ]
                    [ label [] [ text (String.format (gettext "Type \"%s\" to confirm" appState.locale) [ kmLabel ]) ]
                    , input
                        [ class "form-control form-control-danger"
                        , id "km-delete-confirm"
                        , onInput UpdateConfirmInputValue
                        ]
                        []
                    ]
                ]

            else
                [ ActionResultBlock.view
                    { viewContent = viewDependents appState kmLabel model.allVersions
                    , actionResult = model.kmDependents
                    , locale = appState.locale
                    }
                ]

        configureModal =
            if model.continueDelete then
                Modal.confirmConfigActionResult model.deletingKmPackage
                    >> Modal.confirmConfigAction (gettext "Delete" appState.locale) Delete
                    >> Modal.confirmConfigActionEnabled (model.confirmInputValue == kmLabel)

            else
                Modal.confirmConfigActionResult ActionResult.Unset
                    >> Modal.confirmConfigAction (gettext "Continue" appState.locale) ContinueDelete
                    >> Modal.confirmConfigActionEnabled (ActionResult.isSuccess model.kmDependents)

        modalConfig =
            Modal.confirmConfig (String.format (gettext "Delete %s" appState.locale) [ kmLabel ])
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> configureModal
                |> Modal.confirmConfigCancelMsg Close
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "km-delete"
    in
    Modal.confirm appState modalConfig


viewDependents : AppState -> String -> Bool -> List KnowledgeModelPackageDeletionImpact -> Html msg
viewDependents appState kmLabel allVersions dependents =
    let
        explanationText =
            if allVersions then
                gettext "This action will permanently delete all versions of %s, including all dependent knowledge models, knowledge model editors, and projects." appState.locale

            else
                gettext "This action will permanently delete %s, including all dependent knowledge models, knowledge model editors, and projects." appState.locale
    in
    div []
        [ Flash.warning (gettext "Unexpected bad things will happen if you don't read this!" appState.locale)
        , p []
            (String.formatHtml explanationText
                [ strong [] [ text kmLabel ] ]
            )
        , p [] [ text (gettext "Carefully review the list of what will be deleted before continuing." appState.locale) ]
        , div [] (List.map (viewDependent appState) dependents)
        ]


viewDependent : AppState -> KnowledgeModelPackageDeletionImpact -> Html msg
viewDependent appState dependent =
    let
        anyContent =
            not (List.isEmpty dependent.projects)
                || not (List.isEmpty dependent.packages)
                || not (List.isEmpty dependent.editors)

        packages =
            Html.viewIf (not (List.isEmpty dependent.packages)) <|
                viewDependentGroup
                    { title = gettext "Knowledge Models:" appState.locale
                    , items = dependent.packages
                    , viewItem =
                        \package ->
                            linkTo (Routes.knowledgeModelsDetail package.uuid)
                                [ target "_blank" ]
                                [ text (package.name ++ " " ++ Version.toString package.version)
                                ]
                    }

        editors =
            Html.viewIf (not (List.isEmpty dependent.editors)) <|
                viewDependentGroup
                    { title = gettext "Knowledge Model Editors:" appState.locale
                    , items = dependent.editors
                    , viewItem =
                        \editor ->
                            linkTo (Routes.kmEditorEditor editor.uuid Nothing)
                                [ target "_blank" ]
                                [ text editor.name ]
                    }

        projects =
            Html.viewIf (not (List.isEmpty dependent.projects)) <|
                viewDependentGroup
                    { title = gettext "Projects:" appState.locale
                    , items = dependent.projects
                    , viewItem =
                        \project ->
                            linkTo (Routes.projectsDetail project.uuid)
                                [ target "_blank" ]
                                [ text project.name ]
                    }
    in
    div [ class "card bg-light mb-2" ]
        [ div [ class "card-header" ]
            [ strong [] [ text dependent.name ]
            , Badge.secondary [ class "ms-2" ] [ text (Version.toString dependent.version) ]
            ]
        , Html.viewIf anyContent <|
            div [ class "card-body py-2" ]
                [ packages
                , editors
                , projects
                ]
        ]


type alias ViewDependentGroupProps a msg =
    { title : String
    , items : List a
    , viewItem : a -> Html msg
    }


viewDependentGroup : ViewDependentGroupProps a msg -> Html msg
viewDependentGroup props =
    Html.viewIf (not (List.isEmpty props.items)) <|
        div [ class "py-1" ]
            [ strong []
                [ text props.title
                ]
            , ul [ class "mb-0 ps-3" ]
                (List.map
                    (\item ->
                        li []
                            [ props.viewItem item
                            ]
                    )
                    props.items
                )
            ]
