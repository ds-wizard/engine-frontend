module Wizard.Settings.KnowledgeModels.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, button, div, hr, span)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import Shared.Utils exposing (compose2)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.Forms.EditableKnowledgeModelConfigFrom exposing (EditableKnowledgeModelConfigForm)
import Wizard.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.KnowledgeModels.Models exposing (Model)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.KnowledgeModels.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.KnowledgeModels.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps EditableKnowledgeModelConfigForm Msg
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = compose2 (Html.map FormMsg) formView
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError EditableKnowledgeModelConfigForm -> Html Form.Msg
formView appState form =
    let
        enabled =
            Maybe.withDefault False (Form.getFieldAsBool "publicEnabled" form).value

        allowedInputHeader =
            div [ class "form-list-header mb-2" ]
                [ span [] [ lx_ "form.allowedPackages.orgId" appState ]
                , span [] [ lx_ "form.allowedPackages.kmId" appState ]
                , span [] [ lx_ "form.allowedPackages.minVersion" appState ]
                , span [] [ lx_ "form.allowedPackages.maxVersion" appState ]
                ]

        allowedInput =
            if enabled then
                div [ class "nested-group" ]
                    [ FormGroup.listWithHeader appState allowedInputHeader (allowedPackageFormView appState) form "publicPackages" (l_ "form.allowedPackages" appState)
                    ]

            else
                emptyNode
    in
    div []
        [ FormGroup.toggle form "publicEnabled" (l_ "form.public" appState)
        , FormExtra.mdAfter (l_ "form.public.desc" appState)
        , allowedInput
        , hr [] []
        , FormGroup.resizableTextarea appState form "integrationConfig" (l_ "form.integrationConfig" appState)
        ]


allowedPackageFormView : AppState -> Form FormError EditableKnowledgeModelConfigForm -> Int -> Html Form.Msg
allowedPackageFormView appState form index =
    let
        fieldName name =
            "publicPackages." ++ String.fromInt index ++ "." ++ name

        getField name =
            Form.getFieldAsString (fieldName name) form

        viewField name =
            Input.textInput (getField name) [ class "form-control", id (fieldName name) ]
    in
    div [ class "input-group mb-2" ]
        [ viewField "orgId"
        , viewField "kmId"
        , viewField "minVersion"
        , viewField "maxVersion"
        , div [ class "input-group-append" ]
            [ button
                [ class "btn btn-link text-danger"
                , onClick (Form.RemoveItem "publicPackages" index)
                ]
                [ faSet "_global.delete" appState ]
            ]
        ]
