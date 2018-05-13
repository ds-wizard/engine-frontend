module KMEditor.Publish.View exposing (view)

import Common.Form exposing (CustomFormError)
import Common.Html exposing (detailContainerClassWith, emptyNode)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Models exposing (KnowledgeModel, KnowledgeModelPublishForm, kmLastVersion)
import KMEditor.Publish.Models exposing (Model)
import KMEditor.Publish.Msgs exposing (Msg(..))
import Msgs
import Routing exposing (Route(..))


view : Model -> Html Msgs.Msg
view model =
    div [ detailContainerClassWith "KMEditor__Publish" ]
        [ pageHeader "Publish new version" []
        , content model
        ]


content : Model -> Html Msgs.Msg
content model =
    case model.knowledgeModel of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success knowledgeModel ->
            div []
                [ formResultView model.publishingKnowledgeModel
                , formView model.form knowledgeModel
                , formActions KMEditorIndex ( "Publish", model.publishingKnowledgeModel, Msgs.KMEditorPublishMsg <| FormMsg Form.Submit )
                ]


formView : Form CustomFormError KnowledgeModelPublishForm -> KnowledgeModel -> Html Msgs.Msg
formView form knowledgeModel =
    let
        formHtml =
            div []
                [ textGroup knowledgeModel.name "Knowledge Model"
                , codeGroup knowledgeModel.kmId "Knowledge Model ID"
                , lastVersion (kmLastVersion knowledgeModel)
                , versionInputGroup form
                , textAreaGroup form "description" "Description"
                , p [ class "help-block help-block-after" ]
                    [ text "Describe what has changed in the new version." ]
                ]
    in
    formHtml |> Html.map (FormMsg >> Msgs.KMEditorPublishMsg)


lastVersion : Maybe String -> Html msg
lastVersion version =
    let
        content =
            version
                |> Maybe.withDefault "No version of this package has been published yet."
    in
    textGroup content "Last version"


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
                    "has-error"
    in
    div [ class ("form-group " ++ errorClass) ]
        [ label [ class "control-label" ] [ text "New version" ]
        , div [ class "version-inputs" ]
            [ Input.baseInput "number" String Form.Text majorField [ class "form-control", Html.Attributes.min "0" ]
            , text "."
            , Input.baseInput "number" String Form.Text minorField [ class "form-control", Html.Attributes.min "0" ]
            , text "."
            , Input.baseInput "number" String Form.Text patchField [ class "form-control", Html.Attributes.min "0" ]
            ]
        , p [ class "help-block" ]
            [ text "Version number is in format X.Y.Z. Increasing number Z indicates only some fixes, number Y minor changes and number X indicate major change." ]
        ]
