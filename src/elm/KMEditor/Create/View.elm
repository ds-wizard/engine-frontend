module KMEditor.Create.View exposing (view)

import Common.Form exposing (CustomFormError)
import Common.Html exposing (detailContainerClassWith)
import Common.View.FormExtra as FormExtra
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Forms exposing (..)
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (..)
import KMEditor.Create.Models exposing (..)
import KMEditor.Create.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(..))
import KMPackages.Common.Models exposing (PackageDetail)
import Msgs
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ detailContainerClassWith "KMEditor__Create" ]
        [ Page.header "Create Knowledge Model" []
        , Page.actionResultView (content wrapMsg model) model.packages
        ]


content : (Msg -> Msgs.Msg) -> Model -> List PackageDetail -> Html Msgs.Msg
content wrapMsg model packages =
    div []
        [ FormResult.view model.savingKnowledgeModel
        , formView wrapMsg model.form packages
        , formActions (KMEditor Index) ( "Save", model.savingKnowledgeModel, wrapMsg <| FormMsg Form.Submit )
        ]


formView : (Msg -> Msgs.Msg) -> Form CustomFormError KnowledgeModelCreateForm -> List PackageDetail -> Html Msgs.Msg
formView wrapMsg form packages =
    let
        parentOptions =
            ( "", "--" ) :: List.map createOption packages

        formHtml =
            div []
                [ FormGroup.input form "name" "Name"
                , FormGroup.input form "kmId" "Knowledge Model ID"
                , FormExtra.textAfter "Knowledge Model ID can contain alphanumeric characters and dash but cannot start or end with dash."
                , FormGroup.select parentOptions form "parentPackageId" "Parent Knowledge Model"
                ]
    in
    formHtml |> Html.map (wrapMsg << FormMsg)


createOption : PackageDetail -> ( String, String )
createOption package =
    let
        optionText =
            package.name ++ " " ++ package.version ++ " (" ++ package.id ++ ")"
    in
    ( package.id, optionText )
