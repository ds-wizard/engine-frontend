module KMEditor.Create.View exposing (view)

import Common.Form exposing (CustomFormError)
import Common.Html exposing (detailContainerClassWith, emptyNode)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import KMEditor.Create.Models exposing (..)
import KMEditor.Create.Msgs exposing (Msg(..))
import KMEditor.Models exposing (..)
import KMPackages.Common.Models exposing (PackageDetail)
import Msgs
import Routing exposing (Route(..))


view : Model -> Html Msgs.Msg
view model =
    div [ detailContainerClassWith "KMEditor__Create" ]
        [ pageHeader "Create Knowledge Model" []
        , content model
        ]


content : Model -> Html Msgs.Msg
content model =
    case model.packages of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success packages ->
            div []
                [ formResultView model.savingKnowledgeModel
                , formView model.form packages
                , formActions KMEditorIndex ( "Save", model.savingKnowledgeModel, Msgs.KMEditorCreateMsg <| FormMsg Form.Submit )
                ]


formView : Form CustomFormError KnowledgeModelCreateForm -> List PackageDetail -> Html Msgs.Msg
formView form packages =
    let
        parentOptions =
            ( "", "--" ) :: List.map createOption packages

        formHtml =
            div []
                [ inputGroup form "name" "Name"
                , inputGroup form "kmId" "Knowledge Model ID"
                , p [ class "help-block help-block-after" ]
                    [ text "Knowledge Model ID can contain alfanumeric characters and dash but cannot start or end with dash." ]
                , selectGroup parentOptions form "parentPackageId" "Parent Package"
                ]
    in
    formHtml |> Html.map (FormMsg >> Msgs.KMEditorCreateMsg)


createOption : PackageDetail -> ( String, String )
createOption package =
    let
        optionText =
            package.name ++ " " ++ package.version ++ " (" ++ package.id ++ ")"
    in
    ( package.id, optionText )
