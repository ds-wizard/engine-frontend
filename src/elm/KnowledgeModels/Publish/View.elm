module KnowledgeModels.Publish.View exposing (..)

import Common.Html exposing (emptyNode)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import KnowledgeModels.Models exposing (KnowledgeModel, KnowledgeModelPublishForm)
import KnowledgeModels.Publish.Models exposing (Model)
import KnowledgeModels.Publish.Msgs exposing (Msg(..))
import Msgs
import Routing exposing (Route(..))


view : Model -> Html Msgs.Msg
view model =
    div [ class "col-xs-12 col-lg-10 col-lg-offset-1 knowledge-models-publish" ]
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
                , formActions KnowledgeModels ( "Save", model.publishingKnowledgeModel, Msgs.KnowledgeModelsPublishMsg <| FormMsg Form.Submit )
                ]


formView : Form () KnowledgeModelPublishForm -> KnowledgeModel -> Html Msgs.Msg
formView form knowledgeModel =
    let
        formHtml =
            div []
                [ knowledgeModelName knowledgeModel.name
                , artifactId knowledgeModel.artifactId
                , versionInputGroup form
                , textAreaGroup form "description" "Description"
                , p [ class "help-block help-block-after" ]
                    [ text "Describe what is changed in the new version." ]
                ]
    in
    formHtml |> Html.map (FormMsg >> Msgs.KnowledgeModelsPublishMsg)


knowledgeModelName : String -> Html msg
knowledgeModelName name =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text "Knowledge Model" ]
        , p [ class "form-value" ] [ text name ]
        ]


artifactId : String -> Html msg
artifactId artifactId =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text "Artifact ID" ]
        , code [ class "package-artifact-id" ] [ text artifactId ]
        ]


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
        [ label [ class "control-label" ] [ text "Version" ]
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
