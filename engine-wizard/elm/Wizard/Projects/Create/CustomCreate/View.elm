module Wizard.Projects.Create.CustomCreate.View exposing (view)

import ActionResult
import Form
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onSubmit)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Tag as Tag
import Wizard.Projects.Create.CustomCreate.Models exposing (Model)
import Wizard.Projects.Create.CustomCreate.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    let
        buttonConfig =
            { label = gettext "Create" appState.locale
            , result = model.savingQuestionnaire
            , msg = FormMsg Form.Submit
            , dangerous = False
            , attrs = [ disabled (ActionResult.isLoading model.knowledgeModelPreview), dataCy "project_save-button" ]
            }

        submitButton =
            ActionButton.buttonWithAttrs appState buttonConfig
    in
    div [ onSubmit (FormMsg Form.Submit) ]
        [ FormResult.view appState model.savingQuestionnaire
        , formView appState model
        , tagsView appState model
        , FormActions.viewCustomButton appState
            (Routes.projectsIndex appState)
            submitButton
        ]


formView : AppState -> Model -> Html Msg
formView appState model =
    let
        parentInput =
            case model.selectedPackage of
                Just package ->
                    FormGroup.codeView package

                Nothing ->
                    let
                        cfg =
                            { viewItem = TypeHintItem.packageSuggestionWithVersion
                            , wrapMsg = PackageTypeHintInputMsg
                            , nothingSelectedItem = text "--"
                            , clearEnabled = True
                            }

                        typeHintInput =
                            TypeHintInput.view appState cfg model.packageTypeHintInputModel
                    in
                    FormGroup.formGroupCustom typeHintInput appState model.form "packageId"
    in
    div []
        [ Html.map FormMsg <| FormGroup.input appState model.form "name" <| gettext "Name" appState.locale
        , parentInput <| gettext "Knowledge Model" appState.locale
        ]


tagsView : AppState -> Model -> Html Msg
tagsView appState model =
    let
        tagListConfig =
            { selected = model.selectedTags
            , addMsg = AddTag
            , removeMsg = RemoveTag
            }
    in
    Tag.selection appState tagListConfig model.knowledgeModelPreview
