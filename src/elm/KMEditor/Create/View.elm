module KMEditor.Create.View exposing (view)

import Common.Html.Attribute exposing (detailClass)
import Common.View.ActionButton as ActionButton
import Common.View.FormActions as FormActions
import Common.View.FormExtra as FormExtra
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (..)
import KMEditor.Create.Models exposing (..)
import KMEditor.Create.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(..))
import KnowledgeModels.Common.Package as Package exposing (Package)
import Msgs
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    Page.actionResultView (content wrapMsg model) model.packages


content : (Msg -> Msgs.Msg) -> Model -> List Package -> Html Msgs.Msg
content wrapMsg model packages =
    div [ detailClass "KMEditor__Create" ]
        [ Page.header "Create Knowledge Model" []
        , div []
            [ FormResult.errorOnlyView model.savingBranch
            , formView wrapMsg model packages
            , FormActions.view
                (KMEditor IndexRoute)
                (ActionButton.ButtonConfig "Save" model.savingBranch (wrapMsg <| FormMsg Form.Submit) False)
            ]
        ]


formView : (Msg -> Msgs.Msg) -> Model -> List Package -> Html Msgs.Msg
formView wrapMsg model packages =
    let
        parentOptions =
            ( "", "--" ) :: (List.map Package.createFormOption <| List.sortBy .name packages)

        parentInput =
            case model.selectedPackage of
                Just package ->
                    FormGroup.codeView package

                Nothing ->
                    FormGroup.select parentOptions model.form "previousPackageId"

        formHtml =
            div []
                [ FormGroup.input model.form "name" "Name"
                , FormGroup.input model.form "kmId" "Knowledge Model ID"
                , FormExtra.textAfter "Knowledge Model ID can contain alphanumeric characters and dash but cannot start or end with dash."
                , parentInput "Based on"
                , FormExtra.textAfter "You can create a new Knowledge Model based on existing one or start from scratch."
                ]
    in
    formHtml |> Html.map (wrapMsg << FormMsg)
