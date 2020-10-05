module Wizard.Projects.Detail.Components.Settings exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Html exposing (Html, br, button, div, h2, hr, p, strong)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.Templates as TemplatesApi
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.Template exposing (Template)
import Shared.Data.Template.TemplateState as TemplateState
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg, lx)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Page as Page
import Wizard.Ports as Ports
import Wizard.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)
import Wizard.Projects.Common.QuestionnaireEditForm as QuestionnaireEditForm exposing (QuestionnaireEditForm)
import Wizard.Projects.Detail.Components.Settings.DeleteModal as DeleteModal


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.Settings"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Detail.Components.Settings"



-- MODEL


type alias Model =
    { form : Form FormError QuestionnaireEditForm
    , savingQuestionnaire : ActionResult String
    , templates : ActionResult (List Template)
    , deleteModalModel : DeleteModal.Model
    }


init : Maybe QuestionnaireDetail -> Model
init mbQuestionnaire =
    { form = Maybe.unwrap QuestionnaireEditForm.initEmpty QuestionnaireEditForm.init mbQuestionnaire
    , savingQuestionnaire = Unset
    , templates = Loading
    , deleteModalModel = DeleteModal.initialModel
    }



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | PutQuestionnaireComplete (Result ApiError ())
    | GetTemplatesComplete (Result ApiError (List Template))
    | DeleteModalMsg DeleteModal.Msg


fetchData : AppState -> String -> Cmd Msg
fetchData appState packageId =
    TemplatesApi.getTemplatesFor packageId appState GetTemplatesComplete


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , redirectCmd : Cmd msg
    }


update : UpdateConfig msg -> Msg -> AppState -> Uuid -> Model -> ( Model, Cmd msg )
update cfg msg appState questionnaireUuid model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg cfg formMsg appState questionnaireUuid model

        PutQuestionnaireComplete result ->
            handlePutQuestionnaireComplete appState model result

        GetTemplatesComplete result ->
            handleGetTemplatesComplete appState model result

        DeleteModalMsg deleteModalMsg ->
            handleDeleteModalMsg cfg deleteModalMsg appState model


handleFormMsg : UpdateConfig msg -> Form.Msg -> AppState -> Uuid -> Model -> ( Model, Cmd msg )
handleFormMsg cfg formMsg appState questionnaireUuid model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    QuestionnaireEditForm.encode form

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        QuestionnairesApi.putQuestionnaire questionnaireUuid body appState PutQuestionnaireComplete
            in
            ( { model | savingQuestionnaire = Loading }
            , cmd
            )

        _ ->
            ( { model | form = Form.update QuestionnaireEditForm.validation formMsg model.form }
            , Cmd.none
            )


handlePutQuestionnaireComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd msg )
handlePutQuestionnaireComplete appState model result =
    case result of
        Ok _ ->
            ( { model | savingQuestionnaire = Unset }
            , Ports.refresh ()
            )

        Err error ->
            ( { model | savingQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.putError" appState) error }
            , Cmd.none
            )


handleGetTemplatesComplete : AppState -> Model -> Result ApiError (List Template) -> ( Model, Cmd msg )
handleGetTemplatesComplete appState model result =
    case result of
        Ok templates ->
            ( { model | templates = Success templates }, Cmd.none )

        Err error ->
            ( { model | templates = ApiError.toActionResult (lg "apiError.templates.getError" appState) error }
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



-- VIEW


type alias ViewConfig =
    { questionnaire : QuestionnaireDescriptor }


view : AppState -> ViewConfig -> Model -> Html Msg
view appState cfg model =
    Page.actionResultView appState (viewContent appState cfg model) model.templates


viewContent : AppState -> ViewConfig -> Model -> List Template -> Html Msg
viewContent appState cfg model templates =
    div [ class "Plans__Detail__Content Plans__Detail__Content--Settings" ]
        [ div [ detailClass "container" ]
            [ Html.map FormMsg (formView appState model templates)
            , hr [] []
            , dangerZone cfg appState
            ]
        , Html.map DeleteModalMsg <| DeleteModal.view appState model.deleteModalModel
        ]


formView : AppState -> Model -> List Template -> Html Form.Msg
formView appState model templates =
    let
        templateInput =
            FormGroup.selectWithDisabled appState templateOptions model.form "templateId" (lg "questionnaire.template" appState)

        createTemplateOption { id, name, state } =
            let
                visibleName =
                    if appState.config.template.recommendedTemplateId == Just id then
                        name ++ " (" ++ lg "questionnaire.template.recommended" appState ++ ")"

                    else
                        name
            in
            ( id, visibleName, state == TemplateState.UnsupportedMetamodelVersion )

        templateOptions =
            ( "", "--", False ) :: (List.map createTemplateOption <| List.sortBy (String.toLower << .name) templates)

        mbSelectedTemplateId =
            (Form.getFieldAsString "templateId" model.form).value

        mbSelectedTemplate =
            List.find (.id >> Just >> (==) mbSelectedTemplateId) templates

        formatInput =
            case mbSelectedTemplate of
                Just selectedTemplate ->
                    FormGroup.formatRadioGroup appState selectedTemplate.formats model.form "formatUuid" (lg "questionnaire.defaultFormat" appState)

                _ ->
                    emptyNode
    in
    div []
        [ h2 [] [ lx_ "settings.title" appState ]
        , FormGroup.input appState model.form "name" <| lg "questionnaire.name" appState
        , templateInput
        , formatInput
        , FormActions.viewActionOnly appState
            (ActionButton.ButtonConfig (l_ "form.save" appState) model.savingQuestionnaire Form.Submit False)
        ]


dangerZone : ViewConfig -> AppState -> Html Msg
dangerZone cfg appState =
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
