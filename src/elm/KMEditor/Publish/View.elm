module KMEditor.Publish.View exposing (view)

import Common.Form exposing (CustomFormError)
import Common.Html.Attribute exposing (detailClass)
import Common.View.FormActions as FormActions
import Common.View.FormExtra as FormExtra
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Form.Field exposing (Field, FieldValue(..))
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Common.Models exposing (KnowledgeModelDetail, kmLastVersion)
import KMEditor.Publish.Models exposing (KnowledgeModelPublishForm, Model)
import KMEditor.Publish.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(..))
import Msgs
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    Page.actionResultView (contentView wrapMsg model) model.knowledgeModel


contentView : (Msg -> Msgs.Msg) -> Model -> KnowledgeModelDetail -> Html Msgs.Msg
contentView wrapMsg model knowledgeModel =
    div [ detailClass "KMEditor__Publish" ]
        [ Page.header "Publish new version" []
        , div []
            [ FormResult.view model.publishingKnowledgeModel
            , formView wrapMsg model.form knowledgeModel
            , FormActions.view (KMEditor IndexRoute) ( "Publish", model.publishingKnowledgeModel, wrapMsg <| FormMsg Form.Submit )
            ]
        ]


formView : (Msg -> Msgs.Msg) -> Form CustomFormError KnowledgeModelPublishForm -> KnowledgeModelDetail -> Html Msgs.Msg
formView wrapMsg form knowledgeModel =
    div []
        [ FormGroup.textView knowledgeModel.name "Knowledge Model"
        , FormGroup.codeView knowledgeModel.kmId "Knowledge Model ID"
        , lastVersion (kmLastVersion knowledgeModel)
        , versionInputGroup form
        , FormGroup.textarea form "description" "Description"
        , FormExtra.textAfter "Describe what has changed in the new version."
        ]
        |> Html.map (wrapMsg << FormMsg)


lastVersion : Maybe String -> Html msg
lastVersion version =
    let
        content =
            version
                |> Maybe.withDefault "No version of this package has been published yet."
    in
    FormGroup.textView content "Last version"


versionInputGroup : Form e o -> Html Form.Msg
versionInputGroup form =
    let
        majorField =
            Form.getFieldAsString "major" form

        minorField =
            Form.getFieldAsString "minor" form

        patchField =
            Form.getFieldAsString "patch" form

        errorClass =
            case ( majorField.liveError, minorField.liveError, patchField.liveError ) of
                ( Nothing, Nothing, Nothing ) ->
                    ""

                _ ->
                    " is-invalid"
    in
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text "New version" ]
        , div [ class "version-inputs" ]
            [ Input.baseInput "number" String Form.Text majorField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0" ]
            , text "."
            , Input.baseInput "number" String Form.Text minorField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0" ]
            , text "."
            , Input.baseInput "number" String Form.Text patchField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0" ]
            ]
        , FormExtra.text "Version number is in format X.Y.Z. Increasing number Z indicates only some fixes, number Y minor changes and number X indicate major change."
        ]
