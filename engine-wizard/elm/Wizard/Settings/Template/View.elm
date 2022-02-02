module Wizard.Settings.Template.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class)
import List.Extra as List
import Shared.Data.BootstrapConfig.TemplateConfig exposing (TemplateConfig)
import Shared.Data.TemplateSuggestion as TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Form.FormError exposing (FormError)
import Shared.Locale exposing (l)
import Shared.Utils exposing (getOrganizationAndItemId)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Page as Page
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.Template.Models exposing (Model)
import Wizard.Settings.Template.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Template.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState model) model.templates


viewContent : AppState -> Model -> List TemplateSuggestion -> Html Msg
viewContent appState model templates =
    Html.map GenericMsg <|
        GenericView.view (viewProps templates) appState model.genericModel


viewProps : List TemplateSuggestion -> GenericView.ViewProps TemplateConfig
viewProps templates =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView templates
    }


formView : List TemplateSuggestion -> AppState -> Form FormError TemplateConfig -> Html Form.Msg
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
    div [ class "form-group" ]
        [ label [] [ text (l_ "form.recommendedTemplateId" appState) ]
        , div [ class "input-group" ]
            [ Input.selectInput templateOptions recommendedTemplateField [ class "form-control" ]
            , Input.selectInput templateVersionOptions recommendedTemplateIdField [ class "form-control" ]
            ]
        ]
