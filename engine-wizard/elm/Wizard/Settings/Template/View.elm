module Wizard.Settings.Template.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class)
import Shared.Data.BootstrapConfig.TemplateConfig exposing (TemplateConfig)
import Shared.Data.TemplateSuggestion as TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Form.FormError exposing (FormError)
import Shared.Utils exposing (getOrganizationAndItemId)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Page as Page
import Wizard.Settings.Generic.Msgs as GenericMsgs
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.Template.Models exposing (Model)
import Wizard.Settings.Template.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState model) model.templates


viewContent : AppState -> Model -> List TemplateSuggestion -> Html Msg
viewContent appState model templates =
    GenericView.view (viewProps templates) appState model.genericModel


viewProps : List TemplateSuggestion -> GenericView.ViewProps TemplateConfig Msg
viewProps templates =
    { locTitle = gettext "Document Templates"
    , locSave = gettext "Save"
    , formView = formView templates
    , wrapMsg = GenericMsg << GenericMsgs.FormMsg
    }


formView : List TemplateSuggestion -> AppState -> Form FormError TemplateConfig -> Html Msg
formView templates appState form =
    let
        recommendedTemplateField =
            Form.getFieldAsString "recommendedTemplate" form

        recommendedTemplateIdField =
            Form.getFieldAsString "recommendedTemplateId" form

        templateOptions =
            TemplateSuggestion.createOptions templates

        templateToTemplateVersionOptions template =
            templates
                |> List.filter (.id >> getOrganizationAndItemId >> (==) template)
                |> List.sortWith (\a b -> Version.compare b.version a.version)
                |> List.map (\t -> ( t.id, Version.toString t.version ))
                |> (::) ( "", "--" )

        templateVersionOptions =
            recommendedTemplateField.value
                |> Maybe.map templateToTemplateVersionOptions
                |> Maybe.withDefault []
    in
    Html.map (GenericMsg << GenericMsgs.FormMsg) <|
        div [ class "form-group" ]
            [ label [] [ text (gettext "Recommended Template" appState.locale) ]
            , div [ class "input-group" ]
                [ Input.selectInput templateOptions recommendedTemplateField [ class "form-control" ]
                , Input.selectInput templateVersionOptions recommendedTemplateIdField [ class "form-control" ]
                ]
            ]
