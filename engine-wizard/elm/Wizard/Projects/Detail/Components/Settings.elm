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
import Form exposing (Form)
import Form.Field as Field
import Html exposing (Html, br, button, div, h2, hr, p, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.Templates as TemplatesApi
import Shared.Data.Permission exposing (Permission)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg, lx)
import Shared.Setters exposing (setSelected)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
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
    , templateTypeHintInputModel : TypeHintInput.Model TemplateSuggestion
    , savingQuestionnaire : ActionResult String
    , deleteModalModel : DeleteModal.Model
    }


init : Maybe QuestionnaireDetail -> Model
init mbQuestionnaire =
    let
        setSelectedTemplate =
            setSelected (Maybe.andThen .template mbQuestionnaire)
    in
    { form = Maybe.unwrap QuestionnaireEditForm.initEmpty QuestionnaireEditForm.init mbQuestionnaire
    , templateTypeHintInputModel = setSelectedTemplate <| TypeHintInput.init "templateId"
    , savingQuestionnaire = Unset
    , deleteModalModel = DeleteModal.initialModel
    }



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | PutQuestionnaireComplete (Result ApiError ())
    | DeleteModalMsg DeleteModal.Msg
    | TemplateTypeHintInputMsg (TypeHintInput.Msg TemplateSuggestion)


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map TemplateTypeHintInputMsg <|
        TypeHintInput.subscriptions model.templateTypeHintInputModel



-- VIEW


type alias ViewConfig =
    { questionnaire : QuestionnaireDescriptor }


view : AppState -> ViewConfig -> Model -> Html Msg
view appState cfg model =
    div [ class "Plans__Detail__Content Plans__Detail__Content--Settings" ]
        [ div [ detailClass "container" ]
            [ formView appState model
            , hr [] []
            , dangerZone cfg appState
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
    in
    div []
        [ h2 [] [ lx_ "settings.title" appState ]
        , FormResult.errorOnlyView appState model.savingQuestionnaire
        , Html.map FormMsg <| FormGroup.input appState model.form "name" <| lg "questionnaire.name" appState
        , FormGroup.formGroupCustom typeHintInput appState model.form "templateId" <| lg "questionnaire.defaultTemplate" appState
        , Html.map FormMsg <| formatInput
        , FormActions.viewActionOnly appState
            (ActionButton.ButtonConfig (l_ "form.save" appState) model.savingQuestionnaire (FormMsg Form.Submit) False)
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
