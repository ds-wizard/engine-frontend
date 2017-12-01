module KnowledgeModels.Migration.View exposing (..)

import Common.Html exposing (emptyNode)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Common.View.Forms exposing (formResultView)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KnowledgeModels.Editor.Models.Entities exposing (KnowledgeModel)
import KnowledgeModels.Editor.Models.Events exposing (..)
import KnowledgeModels.Migration.Models exposing (Model)
import KnowledgeModels.Migration.Msgs exposing (Msg(..))
import KnowledgeModels.Models.Migration exposing (Migration)
import Msgs


view : Model -> Html Msgs.Msg
view model =
    div [ class "col-xs-12 col-lg-10 col-lg-offset-1" ]
        [ pageHeader "Migration" []
        , formResultView model.conflict
        , content model
        ]


content : Model -> Html Msgs.Msg
content model =
    case model.migration of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success migration ->
            migrationView model migration


migrationView : Model -> Migration -> Html Msgs.Msg
migrationView model migration =
    let
        view =
            case migration.migrationState.targetEvent of
                EditKnowledgeModelEvent data ->
                    viewKnowledgeModelDiff migration.currentKnowledgeModel data
                        |> viewEvent model "Edit knowledge model"

                _ ->
                    div [] [ text "Other event type" ]
    in
    view


viewEvent : Model -> String -> Html Msgs.Msg -> Html Msgs.Msg
viewEvent model name diffView =
    div []
        [ h3 [] [ text name ]
        , div [ class "well" ]
            [ diffView
            , formActions model
            ]
        ]


viewKnowledgeModelDiff : KnowledgeModel -> EditKnowledgeModelEventData -> Html Msgs.Msg
viewKnowledgeModelDiff kmData eventData =
    let
        originalChapters =
            List.map .uuid kmData.chapters

        chapterNames =
            Dict.fromList <| List.map (\c -> ( c.uuid, c.title )) kmData.chapters

        fieldDiff =
            viewDiff <| List.map3 (,,) [ "Name" ] [ kmData.name ] [ eventData.name ]

        childrenDiff =
            viewDiffChildren "Chapters" originalChapters eventData.chapterIds chapterNames
    in
    div []
        (fieldDiff ++ [ childrenDiff ])


viewDiff : List ( String, String, String ) -> List (Html Msgs.Msg)
viewDiff changes =
    List.map
        (\( fieldName, originalValue, newValue ) ->
            let
                content =
                    if originalValue == newValue then
                        [ div [ class "form-value" ] [ text originalValue ] ]
                    else
                        [ div [ class "form-value" ]
                            [ div [] [ del [] [ text originalValue ] ]
                            , div [] [ ins [] [ text newValue ] ]
                            ]
                        ]
            in
            div [ class "form-group" ]
                (label [ class "control-label" ] [ text fieldName ] :: content)
        )
        changes


viewDiffChildren : String -> List String -> List String -> Dict String String -> Html Msgs.Msg
viewDiffChildren fieldName originalOrder newOrder childrenNames =
    let
        viewChildren ulClass uuids =
            ul [ class ulClass ]
                (List.map
                    (\uuid ->
                        Dict.get uuid childrenNames
                            |> Maybe.withDefault ""
                            |> text
                            |> List.singleton
                            |> li []
                    )
                    uuids
                )

        diff =
            if originalOrder == newOrder then
                div [ class "form-value" ]
                    [ viewChildren "" originalOrder
                    ]
            else
                div [ class "form-value" ]
                    [ viewChildren "del" originalOrder
                    , viewChildren "ins" newOrder
                    ]
    in
    div [ class "form-group" ]
        [ label [ class "control-label" ]
            [ text fieldName
            , span [ class "regular" ] [ text " (order)" ]
            ]
        , diff
        ]


formActions : Model -> Html Msgs.Msg
formActions model =
    let
        actionsDisabled =
            case model.conflict of
                Loading ->
                    True

                _ ->
                    False
    in
    div [ class "form-actions" ]
        [ button [ class "btn btn-warning", onClick RejectEvent, disabled actionsDisabled ]
            [ text "Reject" ]
        , button [ class "btn btn-success", onClick AcceptEvent, disabled actionsDisabled ]
            [ text "Accept" ]
        ]
        |> Html.map Msgs.KnowledgeModelsMigrationMsg
