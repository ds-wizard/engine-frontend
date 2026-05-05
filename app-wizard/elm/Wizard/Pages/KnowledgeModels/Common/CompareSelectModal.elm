module Wizard.Pages.KnowledgeModels.Common.CompareSelectModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , open
    , setInitialLeftKm
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError exposing (ApiError)
import Common.Components.Flash as Flash
import Common.Components.Modal as Modal
import Common.Components.TypeHintInput as TypeHintInput
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, div, label, option, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events.Extra exposing (onChange)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Task.Extra as Task
import Uuid exposing (Uuid)
import Version
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Api.KnowledgeModels as KnowledgeModelsApi
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelPackageDetail as KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Api.Models.VersionUuid as VersionUuid
import Wizard.Components.KMComparison.CompareInput exposing (CompareInput)
import Wizard.Components.Tag as Tag
import Wizard.Components.TypeHintInput.TypeHintInputItem as TypeHintInputItem
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { isOpen : Bool
    , leftKmTypeHintInputModel : TypeHintInput.Model KnowledgeModelPackageSuggestion
    , rightKmTypeHintInputModel : TypeHintInput.Model KnowledgeModelPackageSuggestion
    , selectedLeftPackage : Maybe KnowledgeModelPackageSuggestion
    , selectedRightPackage : Maybe KnowledgeModelPackageSuggestion
    , selectedLeftPackageDetail : ActionResult KnowledgeModelPackageDetail
    , selectedRightPackageDetail : ActionResult KnowledgeModelPackageDetail
    , selectedLeftVersion : Maybe Uuid
    , selectedRightVersion : Maybe Uuid
    , leftPreview : ActionResult KnowledgeModel
    , rightPreview : ActionResult KnowledgeModel
    , selectedLeftTags : List String
    , selectedRightTags : List String
    , useAllLeftQuestions : Bool
    , useAllRightQuestions : Bool
    }


initialModel : Model
initialModel =
    { isOpen = True
    , leftKmTypeHintInputModel = TypeHintInput.init "leftKnowledgeModelPackage"
    , rightKmTypeHintInputModel = TypeHintInput.init "rightKnowledgeModelPackage"
    , selectedLeftPackage = Nothing
    , selectedRightPackage = Nothing
    , selectedLeftPackageDetail = ActionResult.Unset
    , selectedRightPackageDetail = ActionResult.Unset
    , selectedLeftVersion = Nothing
    , selectedRightVersion = Nothing
    , leftPreview = ActionResult.Unset
    , rightPreview = ActionResult.Unset
    , selectedLeftTags = []
    , selectedRightTags = []
    , useAllLeftQuestions = True
    , useAllRightQuestions = True
    }


type Msg
    = SetInitialLeftKm KnowledgeModelPackageDetail
    | LeftKmTypeHintInputMsg (TypeHintInput.Msg KnowledgeModelPackageSuggestion)
    | RightKmTypeHintInputMsg (TypeHintInput.Msg KnowledgeModelPackageSuggestion)
    | SelectLeftKm KnowledgeModelPackageSuggestion
    | SelectRightKm KnowledgeModelPackageSuggestion
    | GetSelectedLeftKmCompleted (Result ApiError KnowledgeModelPackageDetail)
    | GetSelectedRightKmCompleted (Result ApiError KnowledgeModelPackageDetail)
    | SelectLeftVersion String
    | SelectRightVersion String
    | GetLeftPreviewCompleted (Result ApiError KnowledgeModel)
    | GetRightPreviewCompleted (Result ApiError KnowledgeModel)
    | AddLeftTag String
    | RemoveLeftTag String
    | AddRightTag String
    | RemoveRightTag String
    | ChangeUseAllLeftQuestions Bool
    | ChangeUseAllRightQuestions Bool
    | Compare
    | SetOpen Bool


open : Msg
open =
    SetOpen True


setInitialLeftKm : KnowledgeModelPackageDetail -> Msg
setInitialLeftKm =
    SetInitialLeftKm


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    , compareMsg : CompareInput -> msg
    }


update : AppState -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update appState cfg msg model =
    let
        selectVersionAfterKm : (String -> msg) -> Result ApiError KnowledgeModelPackageDetail -> Cmd msg
        selectVersionAfterKm wrap result =
            case result of
                Ok kmPackageDetail ->
                    kmPackageDetail.versions
                        |> List.sortWith VersionUuid.compare
                        |> List.reverse
                        |> List.head
                        |> Maybe.unwrap Cmd.none (Task.dispatch << wrap << Uuid.toString << .uuid)

                _ ->
                    Cmd.none
    in
    case msg of
        SetInitialLeftKm kmPackageDetail ->
            let
                leftKmTypeHintInputModel =
                    model.leftKmTypeHintInputModel

                packageSuggestion =
                    KnowledgeModelPackageDetail.toPackageSuggestion kmPackageDetail
            in
            ( { model
                | leftKmTypeHintInputModel = { leftKmTypeHintInputModel | selected = Just packageSuggestion }
                , selectedLeftPackage = Just packageSuggestion
                , selectedLeftPackageDetail = ActionResult.Success kmPackageDetail
                , selectedLeftVersion = Just kmPackageDetail.uuid
                , selectedLeftTags = []
                , useAllLeftQuestions = True
                , leftPreview = ActionResult.Loading
              }
            , KnowledgeModelsApi.fetchPreview appState (Just kmPackageDetail.uuid) [] [] (cfg.wrapMsg << GetLeftPreviewCompleted)
            )

        LeftKmTypeHintInputMsg typeHintInputMsg ->
            let
                typeHintInputCfg =
                    { wrapMsg = cfg.wrapMsg << LeftKmTypeHintInputMsg
                    , getTypeHints = KnowledgeModelPackagesApi.getKnowledgeModelPackagesSuggestions appState Nothing
                    , getError = gettext "Unable to get knowledge models." appState.locale
                    , setReply = cfg.wrapMsg << SelectLeftKm
                    , clearReply = Nothing
                    , filterResults = Nothing
                    }

                ( leftKmTypeHintInputModel, cmd ) =
                    TypeHintInput.update typeHintInputCfg typeHintInputMsg model.leftKmTypeHintInputModel
            in
            ( { model | leftKmTypeHintInputModel = leftKmTypeHintInputModel }, cmd )

        RightKmTypeHintInputMsg typeHintInputMsg ->
            let
                typeHintInputCfg =
                    { wrapMsg = cfg.wrapMsg << RightKmTypeHintInputMsg
                    , getTypeHints = KnowledgeModelPackagesApi.getKnowledgeModelPackagesSuggestions appState Nothing
                    , getError = gettext "Unable to get knowledge models." appState.locale
                    , setReply = cfg.wrapMsg << SelectRightKm
                    , clearReply = Nothing
                    , filterResults = Nothing
                    }

                ( rightKmTypeHintInputModel, cmd ) =
                    TypeHintInput.update typeHintInputCfg typeHintInputMsg model.rightKmTypeHintInputModel
            in
            ( { model | rightKmTypeHintInputModel = rightKmTypeHintInputModel }, cmd )

        SelectLeftKm kmPackage ->
            let
                getSelectedKmCmd =
                    KnowledgeModelPackagesApi.getKnowledgeModelPackageWithoutDeprecatedVersions appState kmPackage.uuid (cfg.wrapMsg << GetSelectedLeftKmCompleted)
            in
            ( { model
                | selectedLeftPackage = Just kmPackage
                , selectedLeftPackageDetail = ActionResult.Loading
                , selectedLeftVersion = Nothing
                , selectedLeftTags = []
                , useAllLeftQuestions = True
                , leftPreview = ActionResult.Unset
              }
            , getSelectedKmCmd
            )

        SelectRightKm kmPackage ->
            let
                getSelectedKmCmd =
                    KnowledgeModelPackagesApi.getKnowledgeModelPackageWithoutDeprecatedVersions appState kmPackage.uuid (cfg.wrapMsg << GetSelectedRightKmCompleted)
            in
            ( { model
                | selectedRightPackage = Just kmPackage
                , selectedRightPackageDetail = ActionResult.Loading
                , selectedRightVersion = Nothing
                , selectedRightTags = []
                , useAllRightQuestions = True
                , rightPreview = ActionResult.Unset
              }
            , getSelectedKmCmd
            )

        GetSelectedLeftKmCompleted result ->
            RequestHelpers.applyResultCmd
                { setResult = \r m -> { m | selectedLeftPackageDetail = r }
                , defaultError = gettext "Unable to get the knowledge model." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                , cmd = selectVersionAfterKm (cfg.wrapMsg << SelectLeftVersion) result
                }

        GetSelectedRightKmCompleted result ->
            RequestHelpers.applyResultCmd
                { setResult = \r m -> { m | selectedRightPackageDetail = r }
                , defaultError = gettext "Unable to get the knowledge model." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                , cmd = selectVersionAfterKm (cfg.wrapMsg << SelectRightVersion) result
                }

        SelectLeftVersion maybeVersionUuid ->
            let
                selectedLeftVersion =
                    Uuid.fromString maybeVersionUuid

                ( leftPreview, getPreviewCmd ) =
                    case selectedLeftVersion of
                        Just versionUuid ->
                            ( ActionResult.Loading
                            , KnowledgeModelsApi.fetchPreview appState (Just versionUuid) [] [] (cfg.wrapMsg << GetLeftPreviewCompleted)
                            )

                        Nothing ->
                            ( ActionResult.Unset, Cmd.none )
            in
            ( { model
                | selectedLeftVersion = selectedLeftVersion
                , leftPreview = leftPreview
              }
            , getPreviewCmd
            )

        SelectRightVersion maybeVersionUuid ->
            let
                selectedRightVersion =
                    Uuid.fromString maybeVersionUuid

                ( rightPreview, getPreviewCmd ) =
                    case selectedRightVersion of
                        Just versionUuid ->
                            ( ActionResult.Loading
                            , KnowledgeModelsApi.fetchPreview appState (Just versionUuid) [] [] (cfg.wrapMsg << GetRightPreviewCompleted)
                            )

                        Nothing ->
                            ( ActionResult.Unset, Cmd.none )
            in
            ( { model
                | selectedRightVersion = selectedRightVersion
                , rightPreview = rightPreview
              }
            , getPreviewCmd
            )

        GetLeftPreviewCompleted result ->
            RequestHelpers.applyResult
                { setResult = \r m -> { m | leftPreview = r }
                , defaultError = gettext "Unable to get question tags for the knowledge model." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        GetRightPreviewCompleted result ->
            RequestHelpers.applyResult
                { setResult = \r m -> { m | rightPreview = r }
                , defaultError = gettext "Unable to get question tags for the knowledge model." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        AddLeftTag tagUuid ->
            ( { model | selectedLeftTags = tagUuid :: model.selectedLeftTags }, Cmd.none )

        RemoveLeftTag tagUuid ->
            ( { model | selectedLeftTags = List.filter ((/=) tagUuid) model.selectedLeftTags }, Cmd.none )

        AddRightTag tagUuid ->
            ( { model | selectedRightTags = tagUuid :: model.selectedRightTags }, Cmd.none )

        RemoveRightTag tagUuid ->
            ( { model | selectedRightTags = List.filter ((/=) tagUuid) model.selectedRightTags }, Cmd.none )

        ChangeUseAllLeftQuestions useAll ->
            ( { model | useAllLeftQuestions = useAll }, Cmd.none )

        ChangeUseAllRightQuestions useAll ->
            ( { model | useAllRightQuestions = useAll }, Cmd.none )

        Compare ->
            case ( model.selectedLeftPackageDetail, model.selectedRightPackageDetail ) of
                ( ActionResult.Success leftPackage, ActionResult.Success rightPackage ) ->
                    case ( model.selectedLeftVersion, model.selectedRightVersion ) of
                        ( Just leftVersion, Just rightVersion ) ->
                            let
                                compareInput =
                                    { leftPackage = leftPackage
                                    , rightPackage = rightPackage
                                    , leftVersion = leftVersion
                                    , rightVersion = rightVersion
                                    , leftTags =
                                        if model.useAllLeftQuestions then
                                            []

                                        else
                                            model.selectedLeftTags
                                    , rightTags =
                                        if model.useAllRightQuestions then
                                            []

                                        else
                                            model.selectedRightTags
                                    }
                            in
                            ( { model | isOpen = False }
                            , Task.dispatch (cfg.compareMsg compareInput)
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetOpen isOpen ->
            ( { model | isOpen = isOpen }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    let
        submitEnabled =
            case ( model.selectedLeftVersion, model.selectedRightVersion ) of
                ( Just _, Just _ ) ->
                    ActionResult.isSuccess model.leftPreview
                        && ActionResult.isSuccess model.rightPreview

                _ ->
                    False

        modalConfig =
            Modal.confirmConfig (gettext "Compare Knowledge Models" appState.locale)
                |> Modal.confirmConfigVisible model.isOpen
                |> Modal.confirmConfigContent (kmSelectForm appState model)
                |> Modal.confirmConfigExtraClass "modal-wide"
                |> Modal.confirmConfigActionEnabled submitEnabled
                |> Modal.confirmConfigAction (gettext "Compare" appState.locale) Compare
                |> Modal.confirmConfigCancelMsg (SetOpen False)
    in
    Modal.confirm appState modalConfig


kmSelectForm : AppState -> Model -> List (Html Msg)
kmSelectForm appState model =
    let
        leftKmTypeHintInputCfg =
            { viewItem = TypeHintInputItem.packageSuggestion False
            , wrapMsg = LeftKmTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = False
            , locale = appState.locale
            }

        leftKmTypeHintInput =
            TypeHintInput.view leftKmTypeHintInputCfg model.leftKmTypeHintInputModel False

        rightKmTypeHintInputCfg =
            { viewItem = TypeHintInputItem.packageSuggestion False
            , wrapMsg = RightKmTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = False
            , locale = appState.locale
            }

        rightKmTypeHintInput =
            TypeHintInput.view rightKmTypeHintInputCfg model.rightKmTypeHintInputModel False
    in
    [ div [ class "" ]
        [ div [ class "form-group" ]
            [ label [] [ text (gettext "Knowledge Model" appState.locale) ]
            , leftKmTypeHintInput
            ]
        , versionSelect
            { locale = appState.locale
            , result = model.selectedLeftPackageDetail
            , selectedVersion = model.selectedLeftVersion
            , onSelectVersion = SelectLeftVersion
            }
        , tagsView appState
            { selectedTagUuids = model.selectedLeftTags
            , knowledgeModel = model.leftPreview
            , addTagMsg = AddLeftTag
            , removeTagMsg = RemoveLeftTag
            , useAllQuestions = model.useAllLeftQuestions
            , changeUseAllQuestionsMsg = ChangeUseAllLeftQuestions
            }
        ]
    , div [ class "horizontal-separator" ] [ text (gettext "compare with" appState.locale) ]
    , div [ class "" ]
        [ div [ class "form-group" ]
            [ label [] [ text (gettext "Knowledge Model" appState.locale) ]
            , rightKmTypeHintInput
            ]
        , versionSelect
            { locale = appState.locale
            , result = model.selectedRightPackageDetail
            , selectedVersion = model.selectedRightVersion
            , onSelectVersion = SelectRightVersion
            }
        , tagsView appState
            { selectedTagUuids = model.selectedRightTags
            , knowledgeModel = model.rightPreview
            , addTagMsg = AddRightTag
            , removeTagMsg = RemoveRightTag
            , useAllQuestions = model.useAllRightQuestions
            , changeUseAllQuestionsMsg = ChangeUseAllRightQuestions
            }
        ]
    ]


type alias VersionSelectProps =
    { locale : Gettext.Locale
    , result : ActionResult KnowledgeModelPackageDetail
    , selectedVersion : Maybe Uuid
    , onSelectVersion : String -> Msg
    }


versionSelect : VersionSelectProps -> Html Msg
versionSelect props =
    case props.result of
        ActionResult.Unset ->
            Html.nothing

        ActionResult.Loading ->
            Flash.loader props.locale

        ActionResult.Error err ->
            Flash.error err

        ActionResult.Success kmPackage ->
            let
                createVersionOption version =
                    ( Uuid.toString version.uuid, Version.toString version.version )

                options =
                    List.map createVersionOption (List.reverse (List.sortWith VersionUuid.compare kmPackage.versions))

                selectedVersion =
                    Maybe.map Uuid.toString props.selectedVersion

                buildOption ( k, v ) =
                    option
                        [ value k
                        , selected (selectedVersion == Just k)
                        ]
                        [ text v ]
            in
            div [ class "form-group" ]
                [ label [] [ text (gettext "Version" props.locale) ]
                , select
                    [ class "form-control"
                    , onChange props.onSelectVersion
                    ]
                    (List.map buildOption options)
                ]


type alias TagsViewProps =
    { selectedTagUuids : List String
    , knowledgeModel : ActionResult KnowledgeModel
    , addTagMsg : String -> Msg
    , removeTagMsg : String -> Msg
    , useAllQuestions : Bool
    , changeUseAllQuestionsMsg : Bool -> Msg
    }


tagsView : AppState -> TagsViewProps -> Html Msg
tagsView appState props =
    let
        tagListConfig =
            { selected = props.selectedTagUuids
            , addMsg = props.addTagMsg
            , removeMsg = props.removeTagMsg
            , showDescription = True
            }

        selectionConfig =
            { tagListConfig = tagListConfig
            , useAllQuestions = props.useAllQuestions
            , useAllQuestionsMsg = props.changeUseAllQuestionsMsg
            }
    in
    Tag.selection appState selectionConfig props.knowledgeModel
