module Wizard.Pages.Settings.Generic.View exposing
    ( ViewProps
    , view
    )

import Common.Components.Form as Form
import Common.Components.Page as Page
import Common.Utils.Form as Form
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.GuideLinks exposing (GuideLinks)
import Form exposing (Form)
import Gettext
import Html exposing (Html, div)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Settings.Generic.Model exposing (Model)


type alias ViewProps form msg =
    { locTitle : Gettext.Locale -> String
    , locSave : Gettext.Locale -> String
    , formView : AppState -> Form FormError form -> Html msg
    , guideLink : GuideLinks -> String
    , wrapMsg : Form.Msg -> msg
    }


view : ViewProps form msg -> AppState -> Model form -> Html msg
view props appState model =
    Page.actionResultView appState (viewForm props appState model) model.config


viewForm : ViewProps form msg -> AppState -> Model form -> config -> Html msg
viewForm props appState model _ =
    let
        form =
            Form.initDynamic appState (props.wrapMsg Form.Submit) model.savingConfig
                |> Form.setFormView (props.formView appState model.form)
                |> Form.setFormChanged (model.formRemoved || Form.containsChanges model.form)
                |> Form.setFormValid (Form.isValid model.form)
                |> Form.setWide
                |> Form.viewDynamic
    in
    div []
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState props.guideLink) (props.locTitle appState.locale)
        , form
        ]
