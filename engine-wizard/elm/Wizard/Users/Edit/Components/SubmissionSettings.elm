module Wizard.Users.Edit.Components.SubmissionSettings exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, div, p, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Html.Extra as Html
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.Form as Form
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.RequestHelpers as RequestHelpers
import String.Format as String
import Wizard.Api.Models.SubmissionProps exposing (SubmissionProps)
import Wizard.Api.Users as UsersApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Ports as Ports
import Wizard.Users.Common.SubmissionPropsEditForm as SubmissionPropsEditForm exposing (SubmissionPropsEditForm)


type alias Model =
    { submissionProps : ActionResult (List SubmissionProps)
    , savingProps : ActionResult String
    , form : Form FormError SubmissionPropsEditForm
    }


initialModel : Model
initialModel =
    { submissionProps = ActionResult.Loading
    , savingProps = ActionResult.Unset
    , form = SubmissionPropsEditForm.initEmpty
    }


type Msg
    = GetSubmissionPropsComplete (Result ApiError (List SubmissionProps))
    | FormMsg Form.Msg
    | PutSubmissionPropsComplete (Result ApiError ())


fetchData : AppState -> Cmd Msg
fetchData appState =
    UsersApi.getCurrentUserSubmissionProps appState GetSubmissionPropsComplete


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetSubmissionPropsComplete result ->
            handleGetSubmissionPropsCompleted cfg appState model result

        FormMsg formMsg ->
            handleFormMsg cfg appState formMsg model

        PutSubmissionPropsComplete result ->
            handlePutSubmissionPropsComplete cfg appState model result


handleGetSubmissionPropsCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError (List SubmissionProps) -> ( Model, Cmd msg )
handleGetSubmissionPropsCompleted cfg appState model result =
    let
        newModel =
            case result of
                Ok submissionProps ->
                    let
                        submissionPropsForm =
                            SubmissionPropsEditForm.init submissionProps
                    in
                    { model
                        | form = submissionPropsForm
                        , submissionProps = ActionResult.Success submissionProps
                    }

                Err _ ->
                    { model | submissionProps = ActionResult.Error <| gettext "Unable to get the user." appState.locale }

        cmd =
            RequestHelpers.getResultCmd cfg.logoutMsg result
    in
    ( newModel, cmd )


handleFormMsg : UpdateConfig msg -> AppState -> Form.Msg -> Model -> ( Model, Cmd msg )
handleFormMsg cfg appState formMsg model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    SubmissionPropsEditForm.encode form

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        UsersApi.putCurrentUserSubmissionProps appState body PutSubmissionPropsComplete
            in
            ( { model | savingProps = ActionResult.Loading }, cmd )

        _ ->
            let
                form =
                    Form.update SubmissionPropsEditForm.validation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )


handlePutSubmissionPropsComplete : UpdateConfig msg -> AppState -> Model -> Result ApiError () -> ( Model, Cmd msg )
handlePutSubmissionPropsComplete cfg appState model result =
    case result of
        Ok _ ->
            ( { model | savingProps = ActionResult.Success <| gettext "Submission settings were successfully updated." appState.locale }
            , Ports.scrollToTop ".Users__Edit__content"
            )

        Err err ->
            ( { model
                | savingProps = ApiError.toActionResult appState (gettext "Submission settings could not be saved." appState.locale) err
                , form = Form.setFormErrors appState err model.form
              }
            , Cmd.batch
                [ RequestHelpers.getResultCmd cfg.logoutMsg result
                , Ports.scrollToTop ".Users__Edit__content"
                ]
            )


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (submissionPropsView appState model) model.submissionProps


submissionPropsView : AppState -> Model -> List SubmissionProps -> Html Msg
submissionPropsView appState model props =
    let
        content =
            if List.isEmpty props then
                Flash.info (gettext "There are no submission services to configure." appState.locale)

            else
                Html.map FormMsg <|
                    formView appState model
    in
    div []
        [ Page.header (gettext "Submission Settings" appState.locale) []
        , FormResult.view model.savingProps
        , content
        ]


formView : AppState -> Model -> Html Form.Msg
formView appState model =
    let
        submissionPropsIndexes =
            Form.getListIndexes "submissionProps" model.form

        noSubmissionFields =
            submissionPropsIndexes
                |> List.map (\i -> Form.getListIndexes ("submissionProps." ++ String.fromInt i ++ ".values") model.form)
                |> List.all List.isEmpty

        submissionSettingsSection i =
            let
                field name =
                    "submissionProps." ++ String.fromInt i ++ "." ++ name

                sectionName =
                    Maybe.withDefault "" (Form.getFieldAsString (field "name") model.form).value

                valueIndexes =
                    Form.getListIndexes (field "values") model.form

                sectionContent =
                    if List.length valueIndexes > 0 then
                        div []
                            (List.map (submissionSettingsSectionProp (field "values")) valueIndexes)

                    else
                        p [ class "text-muted" ] [ text <| String.format (gettext "There is no settings for %s." appState.locale) [ sectionName ] ]
            in
            div [ class "mb-4" ]
                [ strong [ class "d-block mb-1" ] [ text sectionName ]
                , sectionContent
                ]

        submissionSettingsSectionProp prefix i =
            let
                field name =
                    prefix ++ "." ++ String.fromInt i ++ "." ++ name

                valueField =
                    Form.getFieldAsString (field "value") model.form

                propName =
                    Maybe.withDefault "" (Form.getFieldAsString (field "key") model.form).value
            in
            div [ class "row mb-1" ]
                [ div [ class "col-4 d-flex align-items-center" ] [ text propName ]
                , div [ class "col-8" ] [ Input.textInput valueField [ class "form-control" ] ]
                ]

        saveButtonRow =
            if noSubmissionFields then
                Html.nothing

            else
                div [ class "mt-5" ]
                    [ ActionButton.submit (ActionButton.SubmitConfig (gettext "Save" appState.locale) model.savingProps) ]
    in
    Html.form [ onSubmit Form.Submit ]
        (List.map submissionSettingsSection submissionPropsIndexes ++ [ saveButtonRow ])
