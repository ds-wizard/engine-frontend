module Wizard.Projects.Detail.Components.Settings exposing
    ( Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , init
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Debouncer.Extra as Debouncer exposing (Debouncer)
import Form exposing (Form)
import Form.Field as Field
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, br, button, div, h2, hr, label, li, p, strong, text, ul)
import Html.Attributes exposing (class, classList, disabled, id, name, style)
import Html.Events exposing (onClick, onMouseDown)
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Api.DocumentTemplates as DocumentTemplatesApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.DocumentTemplate.DocumentTemplateState as DocumentTemplateState exposing (DocumentTemplateState)
import Shared.Data.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Data.Package exposing (Package)
import Shared.Data.PackageSuggestion as PackageSuggestion
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Permission exposing (Permission)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Setters exposing (setSelected)
import Shared.Utils exposing (dispatch, listFilterJust)
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Tag as Tag
import Wizard.Ports as Ports
import Wizard.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)
import Wizard.Projects.Common.QuestionnaireEditForm as QuestionnaireEditForm exposing (QuestionnaireEditForm)
import Wizard.Projects.Detail.Components.Settings.DeleteModal as DeleteModal
import Wizard.Routes as Routes



-- MODEL


type alias Model =
    { form : Form FormError QuestionnaireEditForm
    , templateTypeHintInputModel : TypeHintInput.Model DocumentTemplateSuggestion
    , savingQuestionnaire : ActionResult String
    , deleteModalModel : DeleteModal.Model
    , projectTagsDebouncer : Debouncer Msg
    , projectTagsSuggestions : ActionResult (Pagination String)
    }


init : AppState -> Maybe QuestionnaireDetail -> Model
init appState mbQuestionnaire =
    let
        setSelectedTemplate =
            setSelected (Maybe.andThen .documentTemplate mbQuestionnaire)
    in
    { form = Maybe.unwrap (QuestionnaireEditForm.initEmpty appState) (QuestionnaireEditForm.init appState) mbQuestionnaire
    , templateTypeHintInputModel = setSelectedTemplate <| TypeHintInput.init "documentTemplateId"
    , savingQuestionnaire = Unset
    , deleteModalModel = DeleteModal.initialModel
    , projectTagsDebouncer = Debouncer.toDebouncer <| Debouncer.debounce 500
    , projectTagsSuggestions = ActionResult.Unset
    }



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | PutQuestionnaireComplete (Result ApiError ())
    | DeleteModalMsg DeleteModal.Msg
    | TemplateTypeHintInputMsg (TypeHintInput.Msg DocumentTemplateSuggestion)
    | ProjectTagsSearch String
    | ProjectTagsSearchComplete (Result ApiError (Pagination String))
    | DebouncerMsg (Debouncer.Msg Msg)


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , redirectCmd : Cmd msg
    , packageId : String
    , questionnaireUuid : Uuid
    , permissions : List Permission
    }


update : UpdateConfig msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update cfg msg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg cfg formMsg appState model

        PutQuestionnaireComplete result ->
            handlePutQuestionnaireComplete appState model result

        DeleteModalMsg deleteModalMsg ->
            handleDeleteModalMsg cfg deleteModalMsg appState model

        TemplateTypeHintInputMsg typeHintInputMsg ->
            handleTemplateTypeHintInputMsg cfg typeHintInputMsg appState model

        ProjectTagsSearch value ->
            handleProjectTagsSearch cfg appState model value

        ProjectTagsSearchComplete result ->
            handleProjectTagsSearchComplete appState model result

        DebouncerMsg debounceMsg ->
            handleDebouncerMsg cfg appState model debounceMsg


handleFormMsg : UpdateConfig msg -> Form.Msg -> AppState -> Model -> ( Model, Cmd msg )
handleFormMsg cfg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    QuestionnaireEditForm.encode form

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        QuestionnairesApi.putQuestionnaire cfg.questionnaireUuid body appState PutQuestionnaireComplete
            in
            ( { model | savingQuestionnaire = Loading }
            , cmd
            )

        _ ->
            let
                searchValue fieldName value =
                    if fieldName == lastProjectTagFieldName model.form then
                        dispatch (cfg.wrapMsg <| DebouncerMsg <| Debouncer.provideInput <| ProjectTagsSearch value)

                    else
                        Cmd.none

                cmd =
                    case formMsg of
                        Form.Focus fieldName ->
                            searchValue fieldName ""

                        Form.Input fieldName Form.Text (Field.String value) ->
                            searchValue fieldName value

                        _ ->
                            Cmd.none
            in
            ( { model | form = Form.update (QuestionnaireEditForm.validation appState) formMsg model.form }
            , cmd
            )


handlePutQuestionnaireComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd msg )
handlePutQuestionnaireComplete appState model result =
    case result of
        Ok _ ->
            ( { model | savingQuestionnaire = Unset }
            , Ports.refresh ()
            )

        Err error ->
            ( { model | savingQuestionnaire = ApiError.toActionResult appState (gettext "Questionnaire could not be saved." appState.locale) error }
            , Cmd.none
            )


handleDeleteModalMsg : UpdateConfig msg -> DeleteModal.Msg -> AppState -> Model -> ( Model, Cmd msg )
handleDeleteModalMsg cfg deleteModalMsg appState model =
    let
        updateConfig =
            { wrapMsg = cfg.wrapMsg << DeleteModalMsg
            , deleteCompleteCmd = cfg.redirectCmd
            }

        ( deleteModalModel, cmd ) =
            DeleteModal.update updateConfig deleteModalMsg appState model.deleteModalModel
    in
    ( { model | deleteModalModel = deleteModalModel }, cmd )


handleTemplateTypeHintInputMsg : UpdateConfig msg -> TypeHintInput.Msg DocumentTemplateSuggestion -> AppState -> Model -> ( Model, Cmd msg )
handleTemplateTypeHintInputMsg cfg typeHintInputMsg appState model =
    let
        formMsg =
            cfg.wrapMsg << FormMsg << Form.Input "documentTemplateId" Form.Select << Field.String

        typeHintInputCfg =
            { wrapMsg = cfg.wrapMsg << TemplateTypeHintInputMsg
            , getTypeHints = DocumentTemplatesApi.getTemplatesFor cfg.packageId
            , getError = gettext "Unable to get Knowledge Models." appState.locale
            , setReply = formMsg << .id
            , clearReply = Just <| formMsg ""
            , filterResults = Nothing
            }

        ( templateTypeHintInputModel, cmd ) =
            TypeHintInput.update typeHintInputCfg typeHintInputMsg appState model.templateTypeHintInputModel
    in
    ( { model | templateTypeHintInputModel = templateTypeHintInputModel }, cmd )


handleProjectTagsSearch : UpdateConfig msg -> AppState -> Model -> String -> ( Model, Cmd msg )
handleProjectTagsSearch cfg appState model value =
    let
        queryString =
            PaginationQueryString.fromQ value
                |> PaginationQueryString.withSize (Just 10)

        selectedTags =
            Form.getListIndexes "projectTags" model.form
                |> List.unconsLast
                |> Maybe.unwrap [] Tuple.second
                |> List.map (\i -> (Form.getFieldAsString ("projectTags." ++ String.fromInt i) model.form).value)
                |> listFilterJust
                |> List.filter (not << String.isEmpty)

        cmd =
            Cmd.map cfg.wrapMsg <|
                QuestionnairesApi.getProjectTagsSuggestions queryString selectedTags appState ProjectTagsSearchComplete
    in
    ( model, cmd )


handleProjectTagsSearchComplete : AppState -> Model -> Result ApiError (Pagination String) -> ( Model, Cmd msg )
handleProjectTagsSearchComplete appState model result =
    case result of
        Ok data ->
            ( { model | projectTagsSuggestions = Success data }
            , Cmd.none
            )

        Err error ->
            ( { model | projectTagsSuggestions = ApiError.toActionResult appState (gettext "Unable to get project tags." appState.locale) error }
            , Cmd.none
            )


handleDebouncerMsg : UpdateConfig msg -> AppState -> Model -> Debouncer.Msg Msg -> ( Model, Cmd msg )
handleDebouncerMsg cfg appState model debounceMsg =
    let
        updateConfig =
            { mapMsg = cfg.wrapMsg << DebouncerMsg
            , getDebouncer = .projectTagsDebouncer
            , setDebouncer = \d m -> { m | projectTagsDebouncer = d }
            }

        update_ updateMsg updateModel =
            update cfg updateMsg appState updateModel
    in
    Debouncer.update update_ updateConfig debounceMsg model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map TemplateTypeHintInputMsg <|
        TypeHintInput.subscriptions model.templateTypeHintInputModel



-- VIEW


type alias ViewConfig =
    { questionnaire : QuestionnaireDescriptor
    , package : Package
    , packageVersions : List Version
    , templateState : Maybe DocumentTemplateState
    , tags : List Tag
    }


view : AppState -> ViewConfig -> Model -> Html Msg
view appState cfg model =
    div [ class "Projects__Detail__Content Projects__Detail__Content--Settings" ]
        [ div [ detailClass "container" ]
            [ formView appState cfg model
            , hr [ class "separator" ] []
            , knowledgeModel appState cfg
            , hr [ class "separator" ] []
            , dangerZone appState cfg
            ]
        , Html.map DeleteModalMsg <| DeleteModal.view appState model.deleteModalModel
        ]


formView : AppState -> ViewConfig -> Model -> Html Msg
formView appState cfg model =
    let
        typeHintInputConfig =
            { viewItem = TypeHintItem.templateSuggestion
            , wrapMsg = TemplateTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = True
            }

        typeHintInput isInvalid =
            let
                unsupportedError =
                    case cfg.templateState of
                        Just DocumentTemplateState.UnsupportedMetamodelVersion ->
                            Flash.error appState (gettext "This document template is no longer supported." appState.locale)

                        _ ->
                            emptyNode
            in
            div []
                [ unsupportedError
                , TypeHintInput.view appState typeHintInputConfig model.templateTypeHintInputModel isInvalid
                ]

        formatInput =
            case model.templateTypeHintInputModel.selected of
                Just selectedTemplate ->
                    FormGroup.formatRadioGroup appState selectedTemplate.formats model.form "formatUuid" (gettext "Default document format" appState.locale)

                _ ->
                    emptyNode

        isTemplateInput =
            if Feature.projectTemplatesCreate appState then
                [ hr [] []
                , Html.map FormMsg <| FormGroup.toggle model.form "isTemplate" <| gettext "Project Template" appState.locale
                , FormExtra.mdAfter (gettext "Project templates can be used by other users so they don't have to start their new projects from scratch." appState.locale)
                ]

            else
                []

        projectTagsInput =
            if Feature.projectTagging appState then
                projectTagsFormGroup appState model

            else
                emptyNode
    in
    div []
        ([ h2 [] [ text (gettext "Settings" appState.locale) ]
         , FormResult.errorOnlyView appState model.savingQuestionnaire
         , Html.map FormMsg <| FormGroup.input appState model.form "name" <| gettext "Name" appState.locale
         , Html.map FormMsg <| FormGroup.input appState model.form "description" <| gettext "Description" appState.locale
         , Html.map FormMsg <| projectTagsInput
         , hr [] []
         , FormGroup.formGroupCustom typeHintInput appState model.form "documentTemplateId" <| gettext "Default document template" appState.locale
         , Html.map FormMsg <| formatInput
         ]
            ++ isTemplateInput
            ++ [ FormActions.viewActionOnly appState
                    (ActionButton.ButtonConfig (gettext "Save" appState.locale) model.savingQuestionnaire (FormMsg Form.Submit) False)
               ]
        )


projectTagsFormGroup : AppState -> Model -> Html Form.Msg
projectTagsFormGroup appState model =
    let
        tags =
            Form.getListIndexes "projectTags" model.form
                |> List.unconsLast
                |> Maybe.unwrap [] Tuple.second
    in
    div [ class "form-group form-group-project-tags" ]
        [ label [] [ text (gettext "Project Tags" appState.locale) ]
        , div []
            (List.map (projectTagView appState model.form) tags ++ projectTagInput appState model)
        ]


projectTagView : AppState -> Form FormError QuestionnaireEditForm -> Int -> Html Form.Msg
projectTagView appState form i =
    let
        value =
            Maybe.withDefault "" <| (Form.getFieldAsString ("projectTags." ++ String.fromInt i) form).value
    in
    div [ class "project-tag", dataCy "project_settings_tag" ]
        [ text value
        , a
            [ class "text-danger ms-2"
            , onClick (Form.RemoveItem "projectTags" i)
            , dataCy "project_settings_tag-remove"
            ]
            [ faSet "_global.remove" appState ]
        ]


projectTagInput : AppState -> Model -> List (Html Form.Msg)
projectTagInput appState model =
    let
        field =
            Form.getFieldAsString (lastProjectTagFieldName model.form) model.form

        isEmpty =
            Maybe.unwrap True String.isEmpty field.value

        ( hasError, errorView ) =
            case field.error of
                Just err ->
                    ( True
                    , p [ class "invalid-feedback", style "display" "block" ]
                        [ text (Form.errorToString appState "" err) ]
                    )

                Nothing ->
                    ( False, emptyNode )

        typehints =
            ActionResult.unwrap [] .items model.projectTagsSuggestions

        typehintMessage =
            Form.Input (lastProjectTagFieldName model.form) Form.Text << Field.String

        typehintView tagName =
            li [ onMouseDown <| typehintMessage tagName, dataCy "project_settings_tag-suggestion" ]
                [ text tagName ]

        typehintsView hasFocus =
            if List.isEmpty typehints || not hasFocus || hasError then
                emptyNode

            else
                ul [ class "typehints" ]
                    (List.map typehintView typehints)
    in
    [ div [ class "input-group" ]
        [ Input.textInput field
            [ class "form-control"
            , classList [ ( "is-invalid", hasError ) ]
            , id "projectTag"
            , name "projectTag"
            ]
        , button
            [ class "btn btn-secondary"
            , disabled (isEmpty || hasError)
            , onClick (Form.Append "projectTags")
            , dataCy "project_settings_add-tag-button"
            ]
            [ text (gettext "Add" appState.locale) ]
        ]
    , errorView
    , typehintsView field.hasFocus
    ]


knowledgeModel : AppState -> ViewConfig -> Html Msg
knowledgeModel appState cfg =
    let
        tagList =
            if List.isEmpty cfg.tags then
                emptyNode

            else
                Tag.viewList cfg.tags
    in
    div []
        [ h2 [] [ text (gettext "Knowledge Model" appState.locale) ]
        , linkTo appState
            (Routes.knowledgeModelsDetail cfg.package.id)
            [ class "package-link mb-2" ]
            [ TypeHintItem.packageSuggestionWithVersion (PackageSuggestion.fromPackage cfg.package cfg.packageVersions) ]
        , tagList
        , div [ class "text-end" ]
            [ linkTo appState
                (Routes.projectsCreateMigration cfg.questionnaire.uuid)
                [ class "btn btn-outline-secondary migration-link" ]
                [ text (gettext "Create migration" appState.locale) ]
            ]
        ]


dangerZone : AppState -> ViewConfig -> Html Msg
dangerZone appState cfg =
    div []
        [ h2 [] [ text (gettext "Danger Zone" appState.locale) ]
        , div [ class "card border-danger" ]
            [ div [ class "card-body" ]
                [ p [ class "card-text" ]
                    [ strong [] [ text (gettext "Delete this project" appState.locale) ]
                    , br [] []
                    , text (gettext "Deleted projects cannot be recovered." appState.locale)
                    ]
                , button
                    [ class "btn btn-outline-danger"
                    , onClick (DeleteModalMsg (DeleteModal.open cfg.questionnaire))
                    ]
                    [ text (gettext "Delete this project" appState.locale) ]
                ]
            ]
        ]



-- UTILS


lastProjectTagFieldName : Form FormError QuestionnaireEditForm -> String
lastProjectTagFieldName form =
    Form.getListIndexes "projectTags" form
        |> List.last
        |> Maybe.withDefault 0
        |> String.fromInt
        |> (++) "projectTags."
