module KMEditor.Index.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Common.AppState exposing (AppState)
import Common.Html exposing (..)
import Common.Html.Attribute exposing (listClass)
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig)
import Common.View.Modal as Modal
import Common.View.Page as Page
import Form
import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Common.Models exposing (KnowledgeModel, KnowledgeModelState(..), kmMatchState)
import KMEditor.Index.Models exposing (..)
import KMEditor.Index.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(..))
import KnowledgeModels.Common.PackageDetail exposing (PackageDetail)
import KnowledgeModels.Common.Version as Version exposing (Version)
import KnowledgeModels.Routing
import Msgs
import Routing exposing (Route(..))
import Utils exposing (listInsertIf, packageIdToComponents)


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    Page.actionResultView (viewKMEditors wrapMsg appState model) model.knowledgeModels


viewKMEditors : (Msg -> Msgs.Msg) -> AppState -> Model -> List KnowledgeModel -> Html Msgs.Msg
viewKMEditors wrapMsg appState model knowledgeModels =
    div [ listClass "KMEditor__Index" ]
        [ Page.header "Knowledge Model Editor" indexActions
        , FormResult.view model.deletingMigration
        , Listing.view (listingConfig wrapMsg appState) <| List.sortBy (String.toLower << .name) knowledgeModels
        , deleteModal wrapMsg model
        , upgradeModal wrapMsg model
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.KMEditor <| CreateRoute Nothing)
        [ class "btn btn-primary" ]
        [ text "Create" ]
    ]


listingConfig : (Msg -> Msgs.Msg) -> AppState -> ListingConfig KnowledgeModel Msgs.Msg
listingConfig wrapMsg appState =
    { title = listingTitle appState.jwt
    , description = listingDescription
    , actions = listingActions wrapMsg appState.jwt
    , textTitle = .name
    , emptyText = "Click \"Create\" button to add a new Knowledge Model Editor."
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    }


listingTitle : Maybe JwtToken -> KnowledgeModel -> Html Msgs.Msg
listingTitle mbJwt km =
    span []
        [ linkToKM mbJwt km [] [ text km.name ]
        , listingTitleLastPublishedVersionBadge km
        , listingTitleBadge km
        ]


linkToKM : Maybe JwtToken -> KnowledgeModel -> List (Attribute Msgs.Msg) -> List (Html Msgs.Msg) -> Html Msgs.Msg
linkToKM mbJwt km =
    case km.stateType of
        Migrating ->
            if continueMigrationActionVisible mbJwt km then
                linkTo (Routing.KMEditor <| MigrationRoute <| km.uuid)

            else
                span

        Migrated ->
            if publishActionVisible mbJwt km then
                linkTo (Routing.KMEditor <| PublishRoute <| km.uuid)

            else
                span

        _ ->
            linkTo (Routing.KMEditor <| EditorRoute <| km.uuid)


listingTitleLastPublishedVersionBadge : KnowledgeModel -> Html msg
listingTitleLastPublishedVersionBadge km =
    let
        getVersion packageId =
            case String.split ":" packageId of
                _ :: _ :: version :: [] ->
                    Just version

                _ ->
                    Nothing

        badge version =
            span [ title "Last published version", class "badge badge-light" ]
                [ text <| Version.toString version ]
    in
    km.parentPackageId
        |> Maybe.andThen getVersion
        |> Maybe.andThen Version.fromString
        |> Maybe.map badge
        |> Maybe.withDefault emptyNode


listingTitleBadge : KnowledgeModel -> Html msg
listingTitleBadge km =
    case km.stateType of
        Outdated ->
            span
                [ title "There is a new version of parent knowledge model"
                , class "badge badge-warning"
                ]
                [ text "outdated" ]

        Migrating ->
            span
                [ title "This editor is in the process of migration to a new parent knowledge model"
                , class "badge badge-info"
                ]
                [ text "migrating" ]

        Migrated ->
            span
                [ title "This editor has been migrated to a new parent knowledge model, you can publish it now."
                , class "badge badge-success"
                ]
                [ text "migrated" ]

        Edited ->
            i
                [ title "This editor contains unpublished changes"
                , class <| "fa fa-pencil"
                ]
                []

        _ ->
            emptyNode


listingDescription : KnowledgeModel -> Html Msgs.Msg
listingDescription km =
    let
        parent =
            case km.lastAppliedParentPackageId of
                Just lastAppliedParentPackageId ->
                    let
                        elem =
                            case packageIdToComponents lastAppliedParentPackageId of
                                Just ( orgId, kmId, version ) ->
                                    linkTo (Routing.KnowledgeModels <| KnowledgeModels.Routing.Detail <| orgId ++ ":" ++ kmId ++ ":" ++ version)

                                _ ->
                                    span
                    in
                    elem [ class "fragment", title "Parent Knowledge Model" ]
                        [ fa "code-fork"
                        , text lastAppliedParentPackageId
                        ]

                Nothing ->
                    emptyNode
    in
    span []
        [ span [ class "fragment" ] [ code [] [ text km.kmId ] ]
        , parent
        ]


listingActions : (Msg -> Msgs.Msg) -> Maybe JwtToken -> KnowledgeModel -> List (ListingActionConfig Msgs.Msg)
listingActions wrapMsg mbJwt km =
    let
        openEditor =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = "Open Editor"
            , msg = ListingActionLink (Routing.KMEditor <| EditorRoute <| km.uuid)
            }

        publish =
            { extraClass = Nothing
            , icon = Just "cloud-upload"
            , label = "Publish"
            , msg = ListingActionLink <| Routing.KMEditor <| PublishRoute <| km.uuid
            }

        upgrade =
            { extraClass = Nothing
            , icon = Just "angle-double-up"
            , label = "Upgrade"
            , msg = ListingActionMsg <| wrapMsg <| ShowHideUpgradeModal <| Just km
            }

        continueMigration =
            { extraClass = Nothing
            , icon = Just "long-arrow-right"
            , label = "Continue Migration"
            , msg = ListingActionLink <| Routing.KMEditor <| MigrationRoute <| km.uuid
            }

        cancelMigration =
            { extraClass = Nothing
            , icon = Just "ban"
            , label = "Cancel Migration"
            , msg = ListingActionMsg <| wrapMsg <| DeleteMigration <| km.uuid
            }

        delete =
            { extraClass = Just "text-danger"
            , icon = Just "trash-o"
            , label = "Delete"
            , msg = ListingActionMsg <| wrapMsg <| ShowHideDeleteKnowledgeModal <| Just km
            }
    in
    []
        |> listInsertIf openEditor (kmMatchState [ Default, Edited, Outdated ] km)
        |> listInsertIf publish (publishActionVisible mbJwt km)
        |> listInsertIf upgrade (upgradeActionVisible mbJwt km)
        |> listInsertIf continueMigration (continueMigrationActionVisible mbJwt km)
        |> listInsertIf cancelMigration (tableActionCancelMigrationVisible mbJwt km)
        |> listInsertIf delete True


publishActionVisible : Maybe JwtToken -> KnowledgeModel -> Bool
publishActionVisible jwt km =
    hasPerm jwt Perm.knowledgeModelPublish && kmMatchState [ Edited, Migrated ] km


upgradeActionVisible : Maybe JwtToken -> KnowledgeModel -> Bool
upgradeActionVisible jwt km =
    hasPerm jwt Perm.knowledgeModelUpgrade && kmMatchState [ Outdated ] km


continueMigrationActionVisible : Maybe JwtToken -> KnowledgeModel -> Bool
continueMigrationActionVisible jwt km =
    hasPerm jwt Perm.knowledgeModelUpgrade && kmMatchState [ Migrating ] km


tableActionCancelMigrationVisible : Maybe JwtToken -> KnowledgeModel -> Bool
tableActionCancelMigrationVisible jwt km =
    hasPerm jwt Perm.knowledgeModelUpgrade && kmMatchState [ Migrating, Migrated ] km


deleteModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
deleteModal wrapMsg model =
    let
        ( visible, name ) =
            case model.kmToBeDeleted of
                Just km ->
                    ( True, km.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                [ text "Are you sure you want to permanently delete "
                , strong [] [ text name ]
                , text "?"
                ]
            ]

        modalConfig =
            { modalTitle = "Delete knowledge model"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingKnowledgeModel
            , actionName = "Delete"
            , actionMsg = wrapMsg DeleteKnowledgeModel
            , cancelMsg = Just <| wrapMsg <| ShowHideDeleteKnowledgeModal Nothing
            , dangerous = True
            }
    in
    Modal.confirm modalConfig


upgradeModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
upgradeModal wrapMsg model =
    let
        ( visible, name ) =
            case model.kmToBeUpgraded of
                Just km ->
                    ( True, km.name )

                Nothing ->
                    ( False, "" )

        options =
            case model.package of
                Success package ->
                    ( "", "- select parent package -" ) :: createOptions package

                _ ->
                    []

        modalContent =
            case model.package of
                Unset ->
                    [ emptyNode ]

                Loading ->
                    [ Page.loader ]

                Error error ->
                    [ p [ class "alert alert-danger" ] [ text error ] ]

                Success packages ->
                    [ p [ class "alert alert-info" ]
                        [ text "Select the new parent package you want to migrate "
                        , strong [] [ text name ]
                        , text " to."
                        ]
                    , FormGroup.select options model.kmUpgradeForm "targetPackageId" "New parent package"
                        |> Html.map (wrapMsg << UpgradeFormMsg)
                    ]

        modalConfig =
            { modalTitle = "Create migration"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.creatingMigration
            , actionName = "Create"
            , actionMsg = wrapMsg <| UpgradeFormMsg Form.Submit
            , cancelMsg = Just <| wrapMsg <| ShowHideUpgradeModal Nothing
            , dangerous = False
            }
    in
    Modal.confirm modalConfig


createOptions : PackageDetail -> List ( String, String )
createOptions package =
    List.map (createOption package) <| List.filter (Version.greaterThan package.version) package.versions


createOption : PackageDetail -> Version -> ( String, String )
createOption package version =
    let
        id =
            package.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString version

        optionText =
            package.name ++ " " ++ Version.toString version ++ " (" ++ id ++ ")"
    in
    ( id, optionText )
