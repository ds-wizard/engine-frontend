module KnowledgeModels.Create.View exposing (..)

import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import KnowledgeModels.Create.Models exposing (..)
import KnowledgeModels.Create.Msgs exposing (Msg(..))
import KnowledgeModels.Models exposing (..)
import Msgs
import PackageManagement.Models exposing (PackageDetail)
import Routing exposing (Route(..))


view : Model -> Html Msgs.Msg
view model =
    let
        content =
            if model.loading then
                fullPageLoader
            else if model.loadingError /= "" then
                defaultFullPageError model.loadingError
            else
                formView model
    in
    div [ class "col-xs-12 col-lg-10 col-lg-offset-1" ]
        [ pageHeader "Create Knowledge Model" []
        , errorView model.error
        , content
        ]


formView : Model -> Html Msgs.Msg
formView model =
    div []
        [ formFields model.form model.packages
        , formActions KnowledgeModels ( "Save", model.savingKm, Msgs.KnowledgeModelsCreateMsg <| FormMsg Form.Submit )
        ]


formFields : Form () KnowledgeModelCreateForm -> List PackageDetail -> Html Msgs.Msg
formFields form packages =
    let
        parentOptions =
            ( "", "--" ) :: List.map createOption packages

        formHtml =
            div []
                [ inputGroup form "name" "Name"
                , inputGroup form "artifactId" "Artifact ID"
                , selectGroup parentOptions form "parentPackageId" "Parent Package"
                ]
    in
    formHtml |> Html.map (FormMsg >> Msgs.KnowledgeModelsCreateMsg)


createOption : PackageDetail -> ( String, String )
createOption package =
    let
        optionText =
            package.name ++ " " ++ package.version ++ " (" ++ package.packageId ++ ")"
    in
    ( package.packageId, optionText )
