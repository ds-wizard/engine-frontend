module Wizard.Projects.Detail.Components.Settings exposing
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
import Html exposing (Html, a, br, button, div, h2, hr, label, li, p, strong, text, ul)
import Html.Attributes exposing (class, classList, disabled, id, name, style)
import Html.Events exposing (onClick, onMouseDown)
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.Templates as TemplatesApi
import Shared.Data.Package exposing (Package)
import Shared.Data.PackageSuggestion as PackageSuggestion
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Permission exposing (Permission)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lg, lgx, lx)
import Shared.Setters exposing (setSelected)
import Shared.Utils exposing (dispatch, listFilterJust)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Ports as Ports
import Wizard.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)
import Wizard.Projects.Common.QuestionnaireEditForm as QuestionnaireEditForm exposing (QuestionnaireEditForm)
import Wizard.Projects.Detail.Components.Settings.DeleteModal as DeleteModal
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.Settings"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Detail.Components.Settings"



-- MODEL


type alias Model =
    { form : Form FormError QuestionnaireEditForm
    , templateTypeHintInputModel : TypeHintInput.Model TemplateSuggestion
    , savingQuestionnaire : ActionResult String
    , deleteModalModel : DeleteModal.Model
    , projectTagsDebouncer : Debouncer Msg
    , projectTagsSuggestions : ActionResult (Pagination String)
    }


init : AppState -> Maybe QuestionnaireDetail -> Model
init appState mbQuestionnaire =
    let
        setSelectedTemplate =
            setSelected (Maybe.andThen .template mbQuestionnaire)
    in
    { form = Maybe.unwrap (QuestionnaireEditForm.initEmpty appState) (QuestionnaireEditForm.init appState) mbQuestionnaire
    , templateTypeHintInputModel = setSelectedTemplate <| TypeHintInput.init "templateId"
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
    | TemplateTypeHintInputMsg (TypeHintInput.Msg TemplateSuggestion)
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
            ( { model | savingQuestionnaire = ApiError.toActionResult appState (lg "apiError.questionnaires.putError" appState) error }
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


handleTemplateTypeHintInputMsg : UpdateConfig msg -> TypeHintInput.Msg TemplateSuggestion -> AppState -> Model -> ( Model, Cmd msg )
handleTemplateTypeHintInputMsg cfg typeHintInputMsg appState model =
    let
        formMsg =
            cfg.wrapMsg << FormMsg << Form.Input "templateId" Form.Select << Field.String

        typeHintInputCfg =
            { wrapMsg = cfg.wrapMsg << TemplateTypeHintInputMsg
            , getTypeHints = TemplatesApi.getTemplatesFor cfg.packageId
            , getError = lg "apiError.packages.getListError" appState
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
            ( { model | projectTagsSuggestions = ApiError.toActionResult appState (lg "apiError.questionnaires.getProjectTagsSuggestionsError" appState) error }
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
    }


view : AppState -> ViewConfig -> Model -> Html Msg
view appState cfg model =
    div [ class "Projects__Detail__Content Projects__Detail__Content--Settings" ]
        [ div [ detailClass "container" ]
            [ formView appState model
            , hr [ class "separator" ] []
            , knowledgeModel appState cfg
            , hr [ class "separator" ] []
            , dangerZone appState cfg
            ]
        , Html.map DeleteModalMsg <| DeleteModal.view appState model.deleteModalModel
        ]


formView : AppState -> Model -> Html Msg
formView appState model =
    let
        cfg =
            { viewItem = TypeHintItem.templateSuggestion appState
            , wrapMsg = TemplateTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = True
            }

        typeHintInput =
            TypeHintInput.view appState cfg model.templateTypeHintInputModel

        formatInput =
            case model.templateTypeHintInputModel.selected of
                Just selectedTemplate ->
                    FormGroup.formatRadioGroup appState selectedTemplate.formats model.form "formatUuid" (lg "questionnaire.defaultFormat" appState)

                _ ->
                    emptyNode

        isTemplateInput =
            if Feature.projectTemplatesCreate appState then
                [ hr [] []
                , Html.map FormMsg <| FormGroup.toggle model.form "isTemplate" <| lg "questionnaire.isTemplate" appState
                , FormExtra.mdAfter (lg "questionnaire.isTemplate.desc" appState)
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
        ([ h2 [] [ lx_ "settings.title" appState ]
         , FormResult.errorOnlyView appState model.savingQuestionnaire
         , Html.map FormMsg <| FormGroup.input appState model.form "name" <| lg "questionnaire.name" appState
         , Html.map FormMsg <| FormGroup.input appState model.form "description" <| lg "questionnaire.description" appState
         , Html.map FormMsg <| projectTagsInput
         , hr [] []
         , FormGroup.formGroupCustom typeHintInput appState model.form "templateId" <| lg "questionnaire.defaultTemplate" appState
         , Html.map FormMsg <| formatInput
         ]
            ++ isTemplateInput
            ++ [ FormActions.viewActionOnly appState
                    (ActionButton.ButtonConfig (l_ "form.save" appState) model.savingQuestionnaire (FormMsg Form.Submit) False)
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
        [ label [] [ lgx "projectTags" appState ]
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
            [ class "text-danger ml-2"
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
        , div [ class "input-group-append" ]
            [ button
                [ class "btn btn-secondary"
                , disabled (isEmpty || hasError)
                , onClick (Form.Append "projectTags")
                , dataCy "project_settings_add-tag-button"
                ]
                [ lx_ "form.addProjectTag" appState ]
            ]
        ]
    , errorView
    , typehintsView field.hasFocus
    ]


knowledgeModel : AppState -> ViewConfig -> Html Msg
knowledgeModel appState cfg =
    div []
        [ h2 [] [ lx_ "knowledgeModel.title" appState ]
        , linkTo appState
            (Routes.knowledgeModelsDetail cfg.package.id)
            [ class "package-link" ]
            [ TypeHintItem.packageSuggestionWithVersion (PackageSuggestion.fromPackage cfg.package) ]
        , div [ class "text-right mt-3" ]
            [ linkTo appState
                (Routes.projectsCreateMigration cfg.questionnaire.uuid)
                [ class "btn btn-outline-secondary migration-link" ]
                [ lx_ "knowledgeModel.createMigration" appState ]
            ]
        ]


dangerZone : AppState -> ViewConfig -> Html Msg
dangerZone appState cfg =
    div []
        [ h2 [] [ lx_ "dangerZone.title" appState ]
        , div [ class "card border-danger" ]
            [ div [ class "card-body" ]
                [ p [ class "card-text" ]
                    [ strong [] [ lx_ "dangerZone.delete.title" appState ]
                    , br [] []
                    , lx_ "dangerZone.delete.desc" appState
                    ]
                , button
                    [ class "btn btn-outline-danger"
                    , onClick (DeleteModalMsg (DeleteModal.open cfg.questionnaire))
                    ]
                    [ lx_ "dangerZone.delete.title" appState ]
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
