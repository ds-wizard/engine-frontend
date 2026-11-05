module Common.Pages.Users.Edit.SubmissionSettings exposing
    ( Model
    , Msg
    , fetchData
    , footer
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.SubmissionProps exposing (SubmissionProps)
import Common.Api.Users as UsersApi
import Common.Components.Form as CommonForm
import Common.Components.Form.Field as Field
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.SubmissionPropsEditForm as SubmissionPropsEditForm exposing (SubmissionPropsEditForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Users.Components.Section as Section
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Ui.Alert as Alert
import Ui.Form as UiForm
import Ui.Text as Text



-- MODEL


type alias Model =
    { submissionProps : ActionResult (List SubmissionProps)
    , savingProps : ActionResult String
    , form : Form FormError SubmissionPropsEditForm
    }


initialModel : Model
initialModel =
    { submissionProps = Loading
    , savingProps = Unset
    , form = SubmissionPropsEditForm.initEmpty
    }


fetchData : AppState -> Cmd Msg
fetchData appState =
    UsersApi.getCurrentUserSubmissionProps appState GetSubmissionPropsComplete



-- UPDATE


type Msg
    = GetSubmissionPropsComplete (Result ApiError (List SubmissionProps))
    | FormMsg Form.Msg
    | PutSubmissionPropsComplete (Result ApiError ())
    | Discard
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Discard ->
            -- The bar's buttons go with the bar, so focus would be left on
            -- nothing: it goes to the page's heading, as it does in settings.
            ( { model
                | form = ActionResult.unwrap model.form SubmissionPropsEditForm.init model.submissionProps
                , savingProps = Unset
              }
            , Cmd.map cfg.wrapMsg (Section.focusHeading NoOp)
            )

        NoOp ->
            ( model, Cmd.none )

        GetSubmissionPropsComplete result ->
            let
                newModel =
                    case result of
                        Ok submissionProps ->
                            { model
                                | form = SubmissionPropsEditForm.init submissionProps
                                , submissionProps = Success submissionProps
                            }

                        Err _ ->
                            { model | submissionProps = Error (gettext "Unable to get the user." appState.locale) }
            in
            ( newModel, RequestHelpers.getResultCmd cfg.logoutMsg result )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    ( { model | savingProps = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        UsersApi.putCurrentUserSubmissionProps appState (SubmissionPropsEditForm.encode form) PutSubmissionPropsComplete
                    )

                _ ->
                    ( { model | form = Form.update SubmissionPropsEditForm.validation formMsg model.form }
                    , Cmd.none
                    )

        PutSubmissionPropsComplete result ->
            case result of
                Ok _ ->
                    -- Read back, so the form holds what was saved: while it
                    -- still differs from what was loaded, the bar goes on
                    -- offering to save what is already saved.
                    ( { model | savingProps = Success (gettext "Submission settings were successfully updated." appState.locale) }
                    , Cmd.map cfg.wrapMsg (fetchData appState)
                    )

                Err error ->
                    ( { model
                        | savingProps = ApiError.toActionResult appState (gettext "Submission settings could not be saved." appState.locale) error
                        , form = FormUtils.setFormErrors appState error model.form
                      }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , CommonForm.focusErrorSummary
                        ]
                    )



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if ActionResult.isSuccess model.submissionProps && not (noSubmissionFields model.form) then
        CommonForm.submitShortcut appState { result = model.savingProps, onSubmit = FormMsg Form.Submit }

    else
        Sub.none


noSubmissionFields : Form FormError SubmissionPropsEditForm -> Bool
noSubmissionFields form =
    Form.getListIndexes "submissionProps" form
        |> List.map (\i -> Form.getListIndexes ("submissionProps." ++ String.fromInt i ++ ".values") form)
        |> List.all List.isEmpty



-- VIEW


view : AppState -> Model -> List (Html Msg)
view appState model =
    Section.view appState
        { title = gettext "Submission Settings" appState.locale
        , guideLink = Just GuideLinks.profileSubmissionSettings
        , content = Section.actionResult appState (submissionPropsView appState model) model.submissionProps
        }


submissionPropsView : AppState -> Model -> List SubmissionProps -> List (Html Msg)
submissionPropsView appState model props =
    if List.isEmpty props then
        [ Alert.toHtml (Alert.info (gettext "There are no submission services to configure." appState.locale)) ]

    else
        let
            successAlert =
                case model.savingProps of
                    Success message ->
                        Alert.toHtml (Alert.success message)

                    _ ->
                        Html.text ""
        in
        [ CommonForm.view
            { onSubmit = FormMsg Form.Submit
            , content =
                successAlert
                    :: CommonForm.errorSummary model.savingProps
                    :: formView appState model
            }
        ]


formView : AppState -> Model -> List (Html Msg)
formView appState model =
    let
        section i =
            let
                field name =
                    "submissionProps." ++ String.fromInt i ++ "." ++ name

                sectionName =
                    Maybe.withDefault "" (Form.getFieldAsString (field "name") model.form).value

                valueIndexes =
                    Form.getListIndexes (field "values") model.form

                sectionContent =
                    if List.isEmpty valueIndexes then
                        [ Text.muted (String.format (gettext "There is no settings for %s." appState.locale) [ sectionName ]) ]

                    else
                        List.map (sectionProp (field "values")) valueIndexes
            in
            UiForm.section
                { id = "submission-settings-" ++ String.fromInt i
                , legend = sectionName
                , content = sectionContent
                }

        sectionProp prefix i =
            let
                field name =
                    prefix ++ "." ++ String.fromInt i ++ "." ++ name

                propName =
                    Maybe.withDefault "" (Form.getFieldAsString (field "key") model.form).value
            in
            Field.input appState model.form (field "value") propName
                |> Field.toHtml
                |> Html.map FormMsg
    in
    List.map section (Form.getListIndexes "submissionProps" model.form)


{-| The unsaved-changes bar, in the settings template's footer slot — the save
the rest of the profile has. Nothing to save where there is nothing to fill in.
-}
footer : AppState -> Model -> Maybe (Html Msg)
footer appState model =
    if ActionResult.isSuccess model.submissionProps && not (noSubmissionFields model.form) then
        Just
            (CommonForm.unsavedBar appState
                { changed = FormUtils.containsChanges model.form
                , result = model.savingProps
                , onSave = FormMsg Form.Submit
                , onDiscard = Just Discard
                }
            )

    else
        Nothing
