module Wizard.Pages.Projects.Detail.Components.Settings exposing
    ( Model
    , Msg
    , UpdateConfig
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
import Html exposing (Html, a, br, button, div, form, h2, hr, label, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, classList, disabled, id, name, style, type_)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick, onMouseDown, onSubmit)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Set
import Shared.Components.Flash as Flash
import Shared.Components.FontAwesome exposing (faQuestionnaireSettingsKmAllQuestions, faQuestionnaireSettingsKmFiltered, faRemove)
import Shared.Components.FormExtra as FormExtra
import Shared.Components.FormGroup as FormGroup
import Shared.Components.FormResult as FormResult
import Shared.Components.Page as Page
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Utils.Form as Form
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.Setters exposing (setSelected)
import Task.Extra as Task
import Uuid exposing (Uuid)
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateState as DocumentTemplateState
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Api.Models.Package.PackagePhase as PackagePhase
import Wizard.Api.Models.PackageSuggestion as PackageSuggestion
import Wizard.Api.Models.Permission exposing (Permission)
import Wizard.Api.Models.QuestionnaireSettings exposing (QuestionnaireSettings)
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Components.FormActions as FormActions
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.Tag as Tag
import Wizard.Components.TypeHintInput as TypeHintInput
import Wizard.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Projects.Common.QuestionnaireDescriptor as QuestionnaireDescriptor
import Wizard.Pages.Projects.Common.QuestionnaireSettingsForm as QuestionnaireSettingsForm exposing (QuestionnaireSettingsForm)
import Wizard.Pages.Projects.Detail.Components.Settings.DeleteModal as DeleteModal
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature
import Wizard.Utils.HtmlAttributesUtils exposing (detailClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks



-- MODEL


type alias Model =
    { form : Form FormError QuestionnaireSettingsForm
    , templateTypeHintInputModel : TypeHintInput.Model DocumentTemplateSuggestion
    , savingQuestionnaire : ActionResult String
    , deleteModalModel : DeleteModal.Model
    , projectTagsDebouncer : Debouncer Msg
    , projectTagsSuggestions : ActionResult (Pagination String)
    }


init : AppState -> Maybe QuestionnaireSettings -> Model
init appState mbQuestionnaire =
    let
        setSelectedTemplate =
            setSelected (Maybe.andThen .documentTemplate mbQuestionnaire)
    in
    { form = Maybe.unwrap (QuestionnaireSettingsForm.initEmpty appState) (QuestionnaireSettingsForm.init appState) mbQuestionnaire
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
    | SetTemplateTypeHintInputReply String
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

        SetTemplateTypeHintInputReply value ->
            handleSetTemplateTypeHintInputReplyMsg appState model value

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
                    QuestionnaireSettingsForm.encode form

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        QuestionnairesApi.putQuestionnaireSettings appState cfg.questionnaireUuid body PutQuestionnaireComplete
            in
            ( { model | savingQuestionnaire = Loading }
            , cmd
            )

        _ ->
            let
                searchValue fieldName value =
                    if fieldName == lastProjectTagFieldName model.form then
                        Task.dispatch (cfg.wrapMsg <| DebouncerMsg <| Debouncer.provideInput <| ProjectTagsSearch value)

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
            ( { model | form = Form.update (QuestionnaireSettingsForm.validation appState) formMsg model.form }
            , cmd
            )


handlePutQuestionnaireComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd msg )
handlePutQuestionnaireComplete appState model result =
    case result of
        Ok _ ->
            ( { model | savingQuestionnaire = Success "" }
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


handleSetTemplateTypeHintInputReplyMsg : AppState -> Model -> String -> ( Model, Cmd msg )
handleSetTemplateTypeHintInputReplyMsg appState model value =
    let
        formMsg field =
            Form.Input field Form.Select << Field.String

        form =
            model.form
                |> Form.update (QuestionnaireSettingsForm.validation appState) (formMsg "documentTemplateId" value)
                |> Form.update (QuestionnaireSettingsForm.validation appState) (formMsg "formatUuid" "")
    in
    ( { model | form = form }, Cmd.none )


handleTemplateTypeHintInputMsg : UpdateConfig msg -> TypeHintInput.Msg DocumentTemplateSuggestion -> AppState -> Model -> ( Model, Cmd msg )
handleTemplateTypeHintInputMsg cfg typeHintInputMsg appState model =
    let
        typeHintInputCfg =
            { wrapMsg = cfg.wrapMsg << TemplateTypeHintInputMsg
            , getTypeHints = DocumentTemplatesApi.getTemplatesFor appState cfg.packageId
            , getError = gettext "Unable to get Knowledge Models." appState.locale
            , setReply = cfg.wrapMsg << SetTemplateTypeHintInputReply << .id
            , clearReply = Just <| cfg.wrapMsg <| SetTemplateTypeHintInputReply ""
            , filterResults = Nothing
            }

        ( templateTypeHintInputModel, cmd ) =
            TypeHintInput.update typeHintInputCfg typeHintInputMsg model.templateTypeHintInputModel
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
                |> List.filterMap (\i -> (Form.getFieldAsString ("projectTags." ++ String.fromInt i) model.form).value)
                |> List.filter (not << String.isEmpty)

        cmd =
            Cmd.map cfg.wrapMsg <|
                QuestionnairesApi.getProjectTagsSuggestions appState queryString selectedTags ProjectTagsSearchComplete
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


view : AppState -> QuestionnaireSettings -> Model -> Html Msg
view appState questionnaire model =
    div [ class "Projects__Detail__Content Projects__Detail__Content--Settings" ]
        [ div [ detailClass "" ]
            [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.projectsSettings) (gettext "Settings" appState.locale)
            , formView appState questionnaire model
            , hr [ class "separator" ] []
            , knowledgeModel appState questionnaire
            , hr [ class "separator" ] []
            , dangerZone appState questionnaire
            ]
        , Html.map DeleteModalMsg <| DeleteModal.view appState model.deleteModalModel
        ]


formView : AppState -> QuestionnaireSettings -> Model -> Html Msg
formView appState questionnaire model =
    let
        typeHintInputConfig =
            { viewItem = TypeHintItem.templateSuggestion
            , wrapMsg = TemplateTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = True
            }

        typeHintInput isInvalid =
            let
                selectedTemplateId =
                    Maybe.map .id model.templateTypeHintInputModel.selected

                questionnaireTemplateId =
                    Maybe.map .id questionnaire.documentTemplate

                templateFlash =
                    if selectedTemplateId == questionnaireTemplateId then
                        case ( questionnaire.documentTemplateState, questionnaire.documentTemplatePhase ) of
                            ( Just DocumentTemplateState.UnsupportedMetamodelVersion, _ ) ->
                                Flash.error (gettext "The used version of the document template is no longer supported. Select a newer version or another supported template." appState.locale)

                            ( _, Just DocumentTemplatePhase.Deprecated ) ->
                                Flash.warning (gettext "This document template is now deprecated." appState.locale)

                            _ ->
                                Html.nothing

                    else
                        Html.nothing
            in
            div []
                [ templateFlash
                , TypeHintInput.view appState typeHintInputConfig model.templateTypeHintInputModel isInvalid
                ]

        formatInput =
            case model.templateTypeHintInputModel.selected of
                Just selectedTemplate ->
                    FormGroup.formatRadioGroup appState.locale selectedTemplate.formats model.form "formatUuid" (gettext "Default document format" appState.locale)

                _ ->
                    Html.nothing

        isTemplateInput =
            if Feature.projectTemplatesCreate appState then
                [ hr [] []
                , Html.map FormMsg <| FormGroup.toggle model.form "isTemplate" <| gettext "Project Template" appState.locale
                , FormExtra.mdAfter (gettext "Other users can use project templates so they don't have to start their new projects from scratch. Project templates follow the same sharing policy as projects, so make sure to share them with users who should use them." appState.locale)
                ]

            else
                []

        projectTagsInput =
            if Feature.projectTagging appState then
                projectTagsFormGroup appState model

            else
                Html.nothing

        originalTagCount =
            List.length questionnaire.projectTags

        currentTagCount =
            List.length (Form.getListIndexes "projectTags" model.form) - 1

        tagsChanged =
            originalTagCount /= currentTagCount

        formChanged =
            (not << Set.isEmpty << Set.filter ((/=) (lastProjectTagFieldName model.form))) (Form.getChangedFields model.form)

        formActionsConfig =
            { text = Nothing
            , actionResult = model.savingQuestionnaire
            , formChanged = tagsChanged || formChanged
            , wide = False
            }
    in
    form [ onSubmit (FormMsg Form.Submit) ]
        ([ FormResult.errorOnlyView model.savingQuestionnaire
         , Html.map FormMsg <| FormGroup.input appState.locale model.form "name" <| gettext "Name" appState.locale
         , Html.map FormMsg <| FormGroup.input appState.locale model.form "description" <| gettext "Description" appState.locale
         , Html.map FormMsg <| projectTagsInput
         , hr [] []
         , FormGroup.formGroupCustom typeHintInput appState.locale model.form "documentTemplateId" <| gettext "Default document template" appState.locale
         , Html.map FormMsg <| formatInput
         ]
            ++ isTemplateInput
            ++ [ FormActions.viewDynamic formActionsConfig appState
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
            (List.map (projectTagView model.form) tags ++ projectTagInput appState model)
        ]


projectTagView : Form FormError QuestionnaireSettingsForm -> Int -> Html Form.Msg
projectTagView form i =
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
            [ faRemove ]
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
                        [ text (Form.errorToString appState.locale "" err) ]
                    )

                Nothing ->
                    ( False, Html.nothing )

        typehints =
            ActionResult.unwrap [] .items model.projectTagsSuggestions

        typehintMessage =
            Form.Input (lastProjectTagFieldName model.form) Form.Text << Field.String

        typehintView tagName =
            li [ onMouseDown <| typehintMessage tagName, dataCy "project_settings_tag-suggestion" ]
                [ text tagName ]

        typehintsView hasFocus =
            if List.isEmpty typehints || not hasFocus || hasError then
                Html.nothing

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
            , type_ "button"
            ]
            [ text (gettext "Add" appState.locale) ]
        ]
    , errorView
    , typehintsView field.hasFocus
    ]


knowledgeModel : AppState -> QuestionnaireSettings -> Html Msg
knowledgeModel appState questionnaire =
    let
        tagList =
            if List.isEmpty questionnaire.selectedQuestionTagUuids then
                div [ class "rounded bg-light px-3 py-2 fw-bold" ]
                    [ faQuestionnaireSettingsKmAllQuestions
                    , span [ class "ms-2" ] [ text (gettext "All questions are used" appState.locale) ]
                    ]

            else
                div []
                    [ div [ class "rounded bg-light px-3 py-2 fw-bold mb-2" ]
                        [ faQuestionnaireSettingsKmFiltered
                        , span [ class "ms-2" ] [ text (gettext "Filtered by question tags" appState.locale) ]
                        ]
                    , Tag.viewList { showDescription = True } questionnaire.knowledgeModelTags
                    ]

        deprecatedWarning =
            if questionnaire.package.phase == PackagePhase.Deprecated then
                Flash.warning (gettext "This knowledge model is now deprecated." appState.locale)

            else
                Html.nothing
    in
    div []
        [ h2 [] [ text (gettext "Knowledge Model" appState.locale) ]
        , deprecatedWarning
        , linkTo (Routes.knowledgeModelsDetail questionnaire.package.id)
            [ class "package-link mb-2" ]
            [ TypeHintItem.packageSuggestionWithVersion (PackageSuggestion.fromPackage questionnaire.package) ]
        , tagList
        , div [ class "mt-3" ]
            [ linkTo (Routes.projectsCreateMigration questionnaire.uuid)
                [ class "btn btn-outline-secondary migration-link" ]
                [ text (gettext "Create migration" appState.locale) ]
            ]
        ]


dangerZone : AppState -> QuestionnaireSettings -> Html Msg
dangerZone appState questionnaire =
    div [ class "pb-6" ]
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
                    , onClick (DeleteModalMsg (DeleteModal.open (QuestionnaireDescriptor.fromQuestionnaireSettings questionnaire)))
                    ]
                    [ text (gettext "Delete this project" appState.locale) ]
                ]
            ]
        ]



-- UTILS


lastProjectTagFieldName : Form FormError QuestionnaireSettingsForm -> String
lastProjectTagFieldName form =
    Form.getListIndexes "projectTags" form
        |> List.last
        |> Maybe.withDefault 0
        |> String.fromInt
        |> (++) "projectTags."
