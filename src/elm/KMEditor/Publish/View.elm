module KMEditor.Publish.View exposing (view)

import Common.Form exposing (CustomFormError)
import Common.Html exposing (detailContainerClassWith, emptyNode)
import Common.View exposing (defaultFullPageError, fullPageActionResultView, fullPageLoader, pageHeader)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Common.Models exposing (KnowledgeModel, kmLastVersion)
import KMEditor.Publish.Models exposing (KnowledgeModelPublishForm, Model)
import KMEditor.Publish.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(Index))
import Msgs
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ detailContainerClassWith "KMEditor__Publish" ]
        [ pageHeader "Publish new version" []
        , fullPageActionResultView (content wrapMsg model) model.knowledgeModel
        ]


content : (Msg -> Msgs.Msg) -> Model -> KnowledgeModel -> Html Msgs.Msg
content wrapMsg model knowledgeModel =
    div []
        [ formResultView model.publishingKnowledgeModel
        , formView wrapMsg model.form knowledgeModel
        , formActions (KMEditor Index) ( "Publish", model.publishingKnowledgeModel, wrapMsg <| FormMsg Form.Submit )
        ]


formView : (Msg -> Msgs.Msg) -> Form CustomFormError KnowledgeModelPublishForm -> KnowledgeModel -> Html Msgs.Msg
formView wrapMsg form knowledgeModel =
    div []
        [ textGroup knowledgeModel.name "Knowledge Model"
        , codeGroup knowledgeModel.kmId "Knowledge Model ID"
        , lastVersion (kmLastVersion knowledgeModel)
        , versionInputGroup form
        , textAreaGroup form "description" "Description"
        , p [ class "form-text text-muted form-text-after" ]
            [ text "Describe what has changed in the new version." ]
        ]
        |> Html.map (wrapMsg << FormMsg)


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
        , p [ class "form-text text-muted" ]
            [ text "Version number is in format X.Y.Z. Increasing number Z indicates only some fixes, number Y minor changes and number X indicate major change." ]
        ]
