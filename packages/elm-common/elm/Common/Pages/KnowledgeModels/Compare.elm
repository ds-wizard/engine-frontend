module Common.Pages.KnowledgeModels.Compare exposing
    ( Model
    , Msg
    , Side
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Comparing two knowledge models, and the dialog that chooses them.

The comparison itself is `Common.Components.KMComparison`, which the project
migration page uses too. What is here is the page around it and the dialog that
picks the two sides — a knowledge model, one of its versions, and either all its
questions or some of its tags, twice.

**The dialog belongs to the page, not to the component**:
the migration page has no selection to make, and a component carrying a dialog
nobody opens is a component with a dead branch.

Arriving with `?leftKnowledgeModelPackageId` preselects the left side at
that version, which is how a knowledge model's own actions reach this page. The
dialog opens on arrival either way, because arriving with nothing chosen is the
usual case.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Common.Api.Models.KnowledgeModelPackageDetail as KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Common.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Common.Api.Models.VersionUuid as VersionUuid
import Common.Components.DetailPage as DetailPage
import Common.Components.KMComparison as KMComparison
import Common.Components.KMComparison.CompareInput exposing (CompareInput)
import Common.Components.Page as Page
import Common.Components.RowBadge as RowBadge
import Common.Components.TypeHint as TypeHint
import Common.Data.AppState exposing (AppState)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Process
import Task
import Ui.Alert as Alert
import Ui.AsyncBlock as AsyncBlock
import Ui.Button as Button
import Ui.EmptyState as EmptyState
import Ui.Field as Field
import Ui.Form as UiForm
import Ui.Icon as Icon
import Ui.Illustration as Illustration
import Ui.Modal as Modal
import Uuid exposing (Uuid)
import Version



-- MODEL


type alias Model =
    { initialPackage : ActionResult ()
    , selectOpen : Bool
    , left : Side
    , right : Side
    , comparison : KMComparison.Model
    }


{-| One half of the choice. The preview is fetched for its tags alone: what to
compare with is the version, and which questions to compare is the tags.
-}
type alias Side =
    { typeHint : TypeHint.Model KnowledgeModelPackageSuggestion
    , package : Maybe KnowledgeModelPackageSuggestion
    , packageDetail : ActionResult KnowledgeModelPackageDetail
    , version : Maybe Uuid
    , preview : ActionResult KnowledgeModel
    , tags : List String
    , useAllQuestions : Bool
    }


type Which
    = Left
    | Right


emptySide : Side
emptySide =
    { typeHint = TypeHint.init
    , package = Nothing
    , packageDetail = Unset
    , version = Nothing
    , preview = Unset
    , tags = []
    , useAllQuestions = True
    }


initialModel : AppState -> Maybe Uuid -> Model
initialModel appState mbLeftPackageUuid =
    { initialPackage =
        if Maybe.isJust mbLeftPackageUuid then
            Loading

        else
            Success ()
    , selectOpen = True
    , left = emptySide
    , right = emptySide
    , comparison = KMComparison.initialModel appState comparisonId
    }


init : AppState -> Maybe Uuid -> ( Model, Cmd Msg )
init appState mbLeftPackageUuid =
    let
        model =
            initialModel appState mbLeftPackageUuid
    in
    ( model
    , case mbLeftPackageUuid of
        Just packageUuid ->
            -- The left side is on its way; focus follows it to the right one
            -- when it lands, rather than moving twice.
            KnowledgeModelPackagesApi.getKnowledgeModelPackageWithoutDeprecatedVersions appState packageUuid GotInitialPackage

        Nothing ->
            focusCmd model
    )


comparisonId : String
comparisonId =
    "km-comparison"


sideId : Which -> String
sideId which =
    case which of
        Left ->
            "km-comparison-left"

        Right ->
            "km-comparison-right"


getSide : Which -> Model -> Side
getSide which model =
    case which of
        Left ->
            model.left

        Right ->
            model.right


setSide : Which -> Side -> Model -> Model
setSide which side model =
    case which of
        Left ->
            { model | left = side }

        Right ->
            { model | right = side }


updateSide : Which -> (Side -> Side) -> Model -> Model
updateSide which f model =
    setSide which (f (getSide which model)) model


{-| The tags a side compares with: none at all when it is using every question.
-}
sideTags : Side -> List String
sideTags side =
    if side.useAllQuestions then
        []

    else
        side.tags


{-| Where focus goes when the selection dialog opens: the side that has not been
chosen yet, and nowhere in particular once both have.

Arriving with `?leftKnowledgeModelPackageId` chooses the left side, so the
dialog opens on the right one — which is the field there is still something to
do in.

-}
focusTarget : Model -> Maybe String
focusTarget model =
    if Maybe.isNothing model.left.package then
        Just (sideId Left ++ "-trigger")

    else if Maybe.isNothing model.right.package then
        Just (sideId Right ++ "-trigger")

    else
        Nothing


{-| The dialog itself starts on its title (`Modal.withFocusOnTitle`,
unconditionally, so the attribute is never added in the same render that opens
the dialog); this moves focus on from there when there is a field to move it to.

A turn of the task queue later than the dialog opening, so it lands after the
browser has made its own choice rather than before it.

-}
focusCmd : Model -> Cmd Msg
focusCmd model =
    case focusTarget model of
        Just fieldId ->
            Process.sleep 0
                |> Task.andThen (\_ -> Dom.focus fieldId)
                |> Task.attempt (always NoOp)

        Nothing ->
            Cmd.none


{-| Both sides have to have chosen a version before there is anything to compare.
-}
compareInput : Model -> Maybe CompareInput
compareInput model =
    case ( ( model.left.packageDetail, model.left.version ), ( model.right.packageDetail, model.right.version ) ) of
        ( ( Success leftPackage, Just leftVersion ), ( Success rightPackage, Just rightVersion ) ) ->
            Just
                { leftPackage = leftPackage
                , rightPackage = rightPackage
                , leftVersion = leftVersion
                , rightVersion = rightVersion
                , leftTags = sideTags model.left
                , rightTags = sideTags model.right
                }

        _ ->
            Nothing



-- UPDATE


type Msg
    = GotInitialPackage (Result ApiError KnowledgeModelPackageDetail)
    | TypeHintMsg Which (TypeHint.Msg KnowledgeModelPackageSuggestion)
    | SelectPackage Which KnowledgeModelPackageSuggestion
    | GotPackage Which (Result ApiError KnowledgeModelPackageDetail)
    | SelectVersion Which String
    | GotPreview Which (Result ApiError KnowledgeModel)
    | SetUseAllQuestions Which Bool
    | ToggleTag Which String
    | SetSelectOpen Bool
    | StartCompare
    | ComparisonMsg KMComparison.Msg
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    let
        plain ( newModel, cmd ) =
            ( newModel, Cmd.map cfg.wrapMsg cmd )
    in
    case msg of
        GotInitialPackage result ->
            case result of
                Ok packageDetail ->
                    let
                        ( withPackage, packageCmd ) =
                            selectPackageDetail appState Left (Just packageDetail.uuid) packageDetail { model | initialPackage = Success () }
                    in
                    plain ( withPackage, Cmd.batch [ packageCmd, focusCmd withPackage ] )

                Err error ->
                    ( { model
                        | initialPackage =
                            ActionResult.map (always ())
                                (ApiError.toActionResult appState (packageError appState) error)
                      }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        TypeHintMsg which typeHintMsg ->
            let
                ( typeHint, cmd ) =
                    TypeHint.update appState
                        { wrapMsg = cfg.wrapMsg << TypeHintMsg which
                        , getTypeHints = KnowledgeModelPackagesApi.getKnowledgeModelPackagesSuggestions appState Nothing
                        , getError = gettext "Unable to get knowledge models." appState.locale
                        , setReply = cfg.wrapMsg << SelectPackage which
                        , clearReply = Nothing
                        , filterResults = Nothing
                        , logoutMsg = cfg.logoutMsg
                        , id = sideId which
                        , toKey = Uuid.toString << .uuid
                        }
                        typeHintMsg
                        (getSide which model).typeHint
            in
            ( updateSide which (\side -> { side | typeHint = typeHint }) model, cmd )

        SelectPackage which package ->
            plain
                ( updateSide which
                    (\side ->
                        { side
                            | package = Just package
                            , packageDetail = Loading
                            , version = Nothing
                            , preview = Unset
                            , tags = []
                        }
                    )
                    model
                , KnowledgeModelPackagesApi.getKnowledgeModelPackageWithoutDeprecatedVersions appState package.uuid (GotPackage which)
                )

        GotPackage which result ->
            case result of
                Ok packageDetail ->
                    plain (selectPackageDetail appState which Nothing packageDetail model)

                Err error ->
                    ( updateSide which
                        (\side -> { side | packageDetail = ApiError.toActionResult appState (packageError appState) error })
                        model
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        SelectVersion which value ->
            let
                version =
                    if String.isEmpty value then
                        Nothing

                    else
                        Just (Uuid.fromUuidString value)
            in
            plain (fetchPreview appState which version (updateSide which (\side -> { side | tags = [] }) model))

        GotPreview which result ->
            case result of
                Ok knowledgeModel ->
                    plain ( updateSide which (\side -> { side | preview = Success knowledgeModel }) model, Cmd.none )

                Err error ->
                    ( updateSide which
                        (\side ->
                            { side
                                | preview =
                                    ApiError.toActionResult appState
                                        (gettext "Unable to get question tags for the knowledge model." appState.locale)
                                        error
                            }
                        )
                        model
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        SetUseAllQuestions which useAll ->
            ( updateSide which (\side -> { side | useAllQuestions = useAll }) model, Cmd.none )

        ToggleTag which tagUuid ->
            ( updateSide which
                (\side ->
                    { side
                        | tags =
                            if List.member tagUuid side.tags then
                                List.filter ((/=) tagUuid) side.tags

                            else
                                tagUuid :: side.tags
                    }
                )
                model
            , Cmd.none
            )

        SetSelectOpen open ->
            ( { model | selectOpen = open }
            , if open then
                Cmd.map cfg.wrapMsg (focusCmd model)

              else
                Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        StartCompare ->
            case compareInput model of
                Just input ->
                    updateComparison cfg appState (KMComparison.compare input) { model | selectOpen = False }

                Nothing ->
                    ( model, Cmd.none )

        ComparisonMsg comparisonMsg ->
            updateComparison cfg appState comparisonMsg model


updateComparison : UpdateConfig Msg msg -> AppState -> KMComparison.Msg -> Model -> ( Model, Cmd msg )
updateComparison cfg appState comparisonMsg model =
    let
        ( comparison, cmd ) =
            KMComparison.update appState
                { wrapMsg = cfg.wrapMsg << ComparisonMsg
                , logoutMsg = cfg.logoutMsg
                }
                comparisonMsg
                model.comparison
    in
    ( { model | comparison = comparison }, cmd )


{-| A package has arrived for a side: it becomes the side's choice, the asked
version is preselected if it is one of its versions, its newest otherwise, and
the preview that names its tags is fetched.
-}
selectPackageDetail : AppState -> Which -> Maybe Uuid -> KnowledgeModelPackageDetail -> Model -> ( Model, Cmd Msg )
selectPackageDetail appState which mbVersion packageDetail model =
    let
        version =
            case mbVersion of
                Just askedVersion ->
                    if List.any ((==) askedVersion << .uuid) packageDetail.versions then
                        Just askedVersion

                    else
                        KnowledgeModelPackageDetail.getLatestPackageUuid packageDetail

                Nothing ->
                    KnowledgeModelPackageDetail.getLatestPackageUuid packageDetail

        suggestion =
            KnowledgeModelPackageDetail.toPackageSuggestion packageDetail

        withPackage =
            updateSide which
                (\side ->
                    { side
                        | packageDetail = Success packageDetail
                        , package = Just suggestion
                        , typeHint = TypeHint.setSelected (Just suggestion) side.typeHint
                    }
                )
                model
    in
    fetchPreview appState which version withPackage


fetchPreview : AppState -> Which -> Maybe Uuid -> Model -> ( Model, Cmd Msg )
fetchPreview appState which mbVersion model =
    case mbVersion of
        Just version ->
            ( updateSide which (\side -> { side | version = Just version, preview = Loading }) model
            , KnowledgeModelsApi.fetchPreview appState (Just version) [] [] (GotPreview which)
            )

        Nothing ->
            ( updateSide which (\side -> { side | version = Nothing, preview = Unset }) model, Cmd.none )


packageError : AppState -> String
packageError appState =
    gettext "Unable to get the knowledge model." appState.locale



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map (TypeHintMsg Left) (TypeHint.subscriptions model.left.typeHint)
        , Sub.map (TypeHintMsg Right) (TypeHint.subscriptions model.right.typeHint)
        , Sub.map ComparisonMsg (KMComparison.subscriptions model.comparison)
        ]



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResult appState (viewPage appState model) model.initialPackage


viewPage : AppState -> Model -> () -> Html Msg
viewPage appState model _ =
    DetailPage.toHtml
        (DetailPage.detailPage
            { title = gettext "Compare Knowledge Models" appState.locale
            , content = [ viewContent appState model, viewSelectModal appState model ]
            }
            |> DetailPage.withActions
                [ Button.primary (gettext "Select knowledge models" appState.locale) (SetSelectOpen True)
                    |> Button.withIcon (Icon.solid "fa-code-compare")
                    |> Button.withTestId "km-comparison_select"
                ]
            |> DetailPage.withFlushContent True
        )


viewContent : AppState -> Model -> Html Msg
viewContent appState model =
    if KMComparison.isStarted model.comparison then
        Html.map ComparisonMsg (KMComparison.view appState model.comparison)

    else
        -- The page's content is flush, so the comparison can run to the edges;
        -- the empty state is prose and asks for the margin back.
        div [ class "c-km-comparison-empty" ]
            [ EmptyState.toHtml
                (EmptyState.emptyState
                    { title = gettext "Nothing to compare yet" appState.locale
                    , description = gettext "Choose two knowledge models to see what changed between them." appState.locale
                    }
                    |> EmptyState.withIllustration (Illustration.toHtml Illustration.empty)
                    |> EmptyState.withTestId "km-compare_empty"
                )
            ]



-- CHOOSING THE TWO SIDES


viewSelectModal : AppState -> Model -> Html Msg
viewSelectModal appState model =
    Modal.plain
        { title = gettext "Select knowledge models" appState.locale
        , open = model.selectOpen
        , onClose = SetSelectOpen False
        , body =
            [ div [ class "c-km-comparison-select" ]
                [ viewSide appState model Left (gettext "Compare" appState.locale)
                , viewSide appState model Right (gettext "with" appState.locale)
                ]
            ]
        }
        |> Modal.withWide
        -- The first field opens a list of suggestions on focus, which is not
        -- where this dialog should start; `focusCmd` moves on from the title to
        -- whichever side still has nothing chosen.
        |> Modal.withFocusOnTitle
        |> Modal.withActions
            [ Button.ghost (gettext "Cancel" appState.locale) (SetSelectOpen False)
            , Button.primary (gettext "Compare" appState.locale) StartCompare
                |> Button.withDisabled (Maybe.isNothing (compareInput model))
                |> Button.withTestId "km-comparison_compare"
            ]
        |> Modal.withTestId "km-comparison_select-modal"
        |> Modal.toHtml


viewSide : AppState -> Model -> Which -> String -> Html Msg
viewSide appState model which legend =
    let
        side =
            getSide which model
    in
    UiForm.section
        { id = sideId which ++ "-section"
        , legend = legend
        , content =
            TypeHint.view appState
                { id = sideId which
                , label = gettext "Knowledge Model" appState.locale
                , placeholder = gettext "Select knowledge model" appState.locale
                , required = True
                , error = Nothing
                , hint = Nothing
                , clearable = False
                , viewItem =
                    \package ->
                        -- No package id: the choice ends in a version, and the
                        -- version is the next field down.
                        { label = package.name
                        , code = Nothing
                        , badges = RowBadge.owner appState package.workspaceUuid
                        , description = Just package.description
                        , icon = Nothing
                        }
                , toKey = Uuid.toString << .uuid
                , wrapMsg = TypeHintMsg which
                }
                side.typeHint
                :: viewVersion appState which side
                ++ viewTags appState which side
        }


viewVersion : AppState -> Which -> Side -> List (Html Msg)
viewVersion appState which side =
    let
        label =
            gettext "Version" appState.locale
    in
    case side.packageDetail of
        Success packageDetail ->
            let
                options =
                    ( "", "--" )
                        :: List.map
                            (\version -> ( Uuid.toString version.uuid, Version.toString version.version ))
                            (List.reverse (List.sortWith VersionUuid.compare packageDetail.versions))
            in
            [ Field.toHtml
                (Field.select
                    { id = sideId which ++ "-version"
                    , label = label
                    , value = Maybe.unwrap "" Uuid.toString side.version
                    , options = options
                    , onInput = SelectVersion which
                    }
                    |> Field.withRequired True
                )
            ]

        Error error ->
            [ Alert.toHtml (Alert.danger error) ]

        Loading ->
            [ AsyncBlock.view
                { loading = True
                , error = Nothing
                , loadingLabel = gettext "Loading..." appState.locale
                , content = []
                }
            ]

        Unset ->
            [ Field.toHtml
                (Field.readOnly
                    { id = sideId which ++ "-version"
                    , label = label
                    , value = gettext "Select knowledge model first" appState.locale
                    }
                )
            ]


viewTags : AppState -> Which -> Side -> List (Html Msg)
viewTags appState which side =
    case side.preview of
        Unset ->
            []

        Loading ->
            [ AsyncBlock.view
                { loading = True
                , error = Nothing
                , loadingLabel = gettext "Loading..." appState.locale
                , content = []
                }
            ]

        Error error ->
            [ Alert.toHtml (Alert.danger error) ]

        Success knowledgeModel ->
            case KnowledgeModel.getTags knowledgeModel of
                [] ->
                    []

                tags ->
                    [ Field.toHtml
                        (Field.radioGroup
                            { id = sideId which ++ "-question-tags-mode"
                            , label = gettext "Question Tags" appState.locale
                            , value = boolValue side.useAllQuestions
                            , options =
                                [ Field.choice { value = boolValue True, label = gettext "Use all questions" appState.locale }
                                , Field.choice { value = boolValue False, label = gettext "Filter by question tags" appState.locale }
                                ]
                            , onChange = SetUseAllQuestions which << (==) (boolValue True)
                            }
                        )
                    , if side.useAllQuestions then
                        Html.text ""

                      else
                        viewTagsSelection appState which side tags
                    ]


viewTagsSelection : AppState -> Which -> Side -> List Tag -> Html Msg
viewTagsSelection appState which side tags =
    let
        tagChoice tag =
            let
                choice =
                    Field.choice { value = tag.uuid, label = tag.name }
                        |> Field.withChoiceColor tag.color
            in
            case tag.description of
                Just description ->
                    Field.withChoiceDescription description choice

                Nothing ->
                    choice
    in
    Field.toHtml
        (Field.checkboxGroup
            { id = sideId which ++ "-question-tags"
            , label = gettext "Filter by question tags" appState.locale
            , values = side.tags
            , options = List.map tagChoice (List.sortBy .name tags)
            , onToggle = ToggleTag which
            }
            |> Field.withHiddenLabel
        )


boolValue : Bool -> String
boolValue value =
    if value then
        "true"

    else
        "false"
