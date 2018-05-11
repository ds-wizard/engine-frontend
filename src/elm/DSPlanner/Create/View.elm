module DSPlanner.Create.View exposing (..)

import Common.Form exposing (CustomFormError)
import Common.Html exposing (detailContainerClass)
import Common.View exposing (fullPageActionResultView, pageHeader)
import Common.View.Forms exposing (formActions, formResultView, inputGroup, selectGroup)
import DSPlanner.Create.Models exposing (Model, QuestionnaireCreateForm)
import DSPlanner.Create.Msgs exposing (Msg(..))
import DSPlanner.Routing
import Form exposing (Form)
import Html exposing (..)
import KMPackages.Models exposing (PackageDetail)
import Msgs
import Routing


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ detailContainerClass ]
        [ pageHeader "Create questionnaire" []
        , fullPageActionResultView (content wrapMsg model) model.packages
        ]


content : (Msg -> Msgs.Msg) -> Model -> List PackageDetail -> Html Msgs.Msg
content wrapMsg model packages =
    div []
        [ formResultView model.savingQuestionnaire
        , formView model.form packages |> Html.map (wrapMsg << FormMsg)
        , formActions (Routing.DSPlanner DSPlanner.Routing.Index) ( "Save", model.savingQuestionnaire, wrapMsg <| FormMsg Form.Submit )
        ]


formView : Form CustomFormError QuestionnaireCreateForm -> List PackageDetail -> Html Form.Msg
formView form packages =
    let
        packageOptions =
            ( "", "--" ) :: List.map createOption packages

        formHtml =
            div []
                [ inputGroup form "name" "Name"
                , selectGroup packageOptions form "packageId" "Package"
                ]
    in
    formHtml


createOption : PackageDetail -> ( String, String )
createOption package =
    let
        optionText =
            package.name ++ " " ++ package.version ++ " (" ++ package.id ++ ")"
    in
    ( package.id, optionText )
