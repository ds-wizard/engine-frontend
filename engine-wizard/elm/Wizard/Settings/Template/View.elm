module Wizard.Settings.Template.View exposing (..)

import Form exposing (Form)
import Html exposing (Html, div)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Config.TemplateConfig exposing (TemplateConfig)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Page as Page
import Wizard.Documents.Common.Template exposing (Template)
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.Template.Models exposing (Model)
import Wizard.Settings.Template.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Template.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Template.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState model) model.templates


viewContent : AppState -> Model -> List Template -> Html Msg
viewContent appState model templates =
    Html.map GenericMsg <|
        GenericView.view (viewProps templates) appState model.genericModel


viewProps : List Template -> GenericView.ViewProps TemplateConfig
viewProps templates =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView templates
    }


formView : List Template -> AppState -> Form CustomFormError TemplateConfig -> Html Form.Msg
formView templates appState form =
    let
        toFormOption { uuid, name } =
            ( uuid, name )

        options =
            ( "", "- none -" ) :: List.map toFormOption templates
    in
    div []
        [ FormGroup.select appState options form "recommendedTemplateUuid" (l_ "form.recommendedTemplateUuid" appState) ]
