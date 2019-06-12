module KMEditor.Create.View exposing (view)

import Common.Form exposing (CustomFormError)
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
import KnowledgeModels.Common.Package exposing (Package)
import KnowledgeModels.Common.Version as Version
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
            [ FormResult.view model.savingKnowledgeModel
            , formView wrapMsg model packages
            , FormActions.view
                (KMEditor IndexRoute)
                (ActionButton.ButtonConfig "Save" model.savingKnowledgeModel (wrapMsg <| FormMsg Form.Submit) False)
            ]
        ]


formView : (Msg -> Msgs.Msg) -> Model -> List Package -> Html Msgs.Msg
formView wrapMsg model packages =
    let
        parentOptions =
            ( "", "--" ) :: (List.map createOption <| List.sortBy .name packages)

        parentInput =
            case model.selectedPackage of
                Just package ->
                    FormGroup.codeView package

                Nothing ->
                    FormGroup.select parentOptions model.form "parentPackageId"

        formHtml =
            div []
                [ FormGroup.input model.form "name" "Name"
                , FormGroup.input model.form "kmId" "Knowledge Model ID"
                , FormExtra.textAfter "Knowledge Model ID can contain alphanumeric characters and dash but cannot start or end with dash."
                , parentInput "Parent Knowledge Model"
                ]
    in
    formHtml |> Html.map (wrapMsg << FormMsg)


createOption : Package -> ( String, String )
createOption package =
    let
        optionText =
            package.name ++ " " ++ Version.toString package.version ++ " (" ++ package.id ++ ")"
    in
    ( package.id, optionText )
