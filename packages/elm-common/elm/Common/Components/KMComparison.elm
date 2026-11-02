module Common.Components.KMComparison exposing
    ( Model
    , Msg
    , UpdateConfig
    , compare
    , initialModel
    , isStarted
    , subscriptions
    , update
    , view
    )

{-| Two knowledge models, side by side, with what happened to every entity
between them.

It is a component because two features need it: the Compare Knowledge Models
page and the project migration page.

**It is a workspace, not a table.** A two-column `<table>` in a flex pane beside
a hand-built side panel would need layout classes at the call site for its
heights to resolve. `Ui.Page.Workspace` is that shape and owns the
parts that are easy to get wrong: the tree scrolls on its own, the panel is
resizable, and on a narrow screen the panel becomes an overlay that closes with
Escape.

**What changed is never said by color alone.** A green table cell as the only
sign that an entity was added would fail WCAG 1.4.1, so every row carries a word — "Added", "Removed", "Changed" — beside the entity's icon, and
the tint stays as the second signal.

The diff itself is `KMComparison.Rows`, which is pure and tested; this module
fetches, holds what is open, and draws.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Common.Components.KMComparison.CompareInput as CompareInput exposing (CompareInput)
import Common.Components.KMComparison.Detail as Detail
import Common.Components.KMComparison.Rows as Rows exposing (Row, State(..))
import Common.Components.Page as Page
import Common.Components.UiWords as UiWords
import Common.Data.AppState exposing (AppState)
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, button, div, li, span, text)
import Html.Attributes exposing (attribute, class, type_)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Html.Keyed as Keyed
import Maybe.Extra as Maybe
import Set exposing (Set)
import Ui.Badge as Badge
import Ui.Copy as Copy
import Ui.Icon as Icon exposing (Icon)
import Ui.ItemIcon as ItemIcon
import Ui.Page.Workspace as Workspace
import Ui.Resizer as Resizer
import Ui.Tag as Tag
import Ui.Text as Text
import Uuid exposing (Uuid)
import Version



-- MODEL


type alias Model =
    { id : String
    , compareInput : Maybe CompareInput
    , leftKm : ActionResult KnowledgeModel
    , rightKm : ActionResult KnowledgeModel
    , collapsed : Set String
    , openEntity : Maybe Rows.EntityDiff
    , asideResizer : Resizer.Model
    , copy : Copy.Model
    }


{-| `id` names the workspace and everything with a fixed id inside it, so two
comparisons could sit on one page without colliding.
-}
initialModel : AppState -> String -> Model
initialModel appState id =
    { id = id
    , compareInput = Nothing
    , leftKm = Unset
    , rightKm = Unset
    , collapsed = Set.empty
    , openEntity = Nothing
    , asideResizer = Resizer.init (asideResizerConfig appState)
    , copy = Copy.init
    }


asideResizerConfig : AppState -> Resizer.Config
asideResizerConfig appState =
    { edge = Resizer.Leading
    , label = gettext "Resize panel" appState.locale
    , min = 288
    , max = 800
    , default = 384
    , collapse = Nothing
    }


{-| True once a comparison has been asked for. The page around it shows its own
empty state until then, rather than an empty comparison.
-}
isStarted : Model -> Bool
isStarted model =
    model.leftKm /= Unset



-- UPDATE


type Msg
    = Compare CompareInput
    | GetLeftKmCompleted (Result ApiError KnowledgeModel)
    | GetRightKmCompleted (Result ApiError KnowledgeModel)
    | ToggleCollapse String
    | OpenEntity Rows.EntityDiff
    | CloseEntity
    | AsideResizerMsg Resizer.Msg
    | CopyMsg Copy.Msg


{-| The only way to start a comparison.
-}
compare : CompareInput -> Msg
compare =
    Compare


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : AppState -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update appState cfg msg model =
    case msg of
        Compare compareInput ->
            ( { model
                | compareInput = Just compareInput
                , leftKm = Loading
                , rightKm = Loading
                , collapsed = Set.empty
                , openEntity = Nothing
              }
            , Cmd.batch
                [ Cmd.map cfg.wrapMsg <|
                    KnowledgeModelsApi.fetchPreview appState
                        (Just compareInput.leftVersion)
                        []
                        compareInput.leftTags
                        GetLeftKmCompleted
                , Cmd.map cfg.wrapMsg <|
                    KnowledgeModelsApi.fetchPreview appState
                        (Just compareInput.rightVersion)
                        []
                        compareInput.rightTags
                        GetRightKmCompleted
                ]
            )

        GetLeftKmCompleted result ->
            RequestHelpers.applyResult
                { setResult = \value m -> { m | leftKm = value }
                , defaultError = knowledgeModelError appState
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        GetRightKmCompleted result ->
            RequestHelpers.applyResult
                { setResult = \value m -> { m | rightKm = value }
                , defaultError = knowledgeModelError appState
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        ToggleCollapse uuid ->
            ( { model
                | collapsed =
                    if Set.member uuid model.collapsed then
                        Set.remove uuid model.collapsed

                    else
                        Set.insert uuid model.collapsed
              }
            , Cmd.none
            )

        OpenEntity entity ->
            ( { model | openEntity = Just entity }, Cmd.none )

        CloseEntity ->
            ( { model | openEntity = Nothing }, Cmd.none )

        AsideResizerMsg resizerMsg ->
            ( { model | asideResizer = Resizer.update resizerMsg model.asideResizer }, Cmd.none )

        CopyMsg copyMsg ->
            let
                ( copy, cmd ) =
                    Copy.update copyMsg model.copy
            in
            ( { model | copy = copy }, Cmd.map (cfg.wrapMsg << CopyMsg) cmd )


{-| A sentence the rest of the app already says, so it is translated.
-}
knowledgeModelError : AppState -> String
knowledgeModelError appState =
    gettext "Unable to get the knowledge model." appState.locale


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map AsideResizerMsg (Resizer.subscriptions model.asideResizer)



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResult appState
        (viewComparison appState model)
        (ActionResult.combine model.leftKm model.rightKm)


viewComparison : AppState -> Model -> ( KnowledgeModel, KnowledgeModel ) -> Html Msg
viewComparison appState model ( leftKm, rightKm ) =
    let
        rows =
            Rows.fold
                { locale = appState.locale
                , leftKm = leftKm
                , rightKm = rightKm
                , collapsed = model.collapsed
                }

        words =
            stateWords appState

        main_ =
            [ div [ class "c-km-comparison" ]
                (viewHeader model leftKm rightKm
                    ++ [ Keyed.ul [ class "c-km-comparison__rows" ]
                            (List.map (viewKeyedRow words model) rows)
                       ]
                )
            ]
    in
    div [ class "c-km-comparison-pane" ]
        [ Workspace.toHtml
            (withAside appState
                model
                leftKm
                rightKm
                (Workspace.withFlushMain (Workspace.workspace { id = model.id, main = main_ }))
            )
        ]


withAside : AppState -> Model -> KnowledgeModel -> KnowledgeModel -> Workspace.Workspace Msg -> Workspace.Workspace Msg
withAside appState model leftKm rightKm workspace =
    case model.openEntity of
        Just entity ->
            let
                detailConfig =
                    { locale = appState.locale
                    , leftKm = leftKm
                    , rightKm = rightKm
                    , copy = model.copy
                    , toCopyMsg = CopyMsg
                    }
            in
            Workspace.withAside
                { title = Detail.kind detailConfig entity
                , content = Detail.view detailConfig entity
                , resizer = model.asideResizer
                , toResizerMsg = AsideResizerMsg
                , resizerWords = UiWords.resizer appState (gettext "Resize panel" appState.locale)
                , onClose = CloseEntity
                , closeLabel = gettext "Close panel" appState.locale

                -- A row opened this, and that row scrolls away; nothing else
                -- would close it.
                , closeInPanel = True
                }
                workspace

        Nothing ->
            workspace



-- WHAT EACH SIDE IS


viewHeader : Model -> KnowledgeModel -> KnowledgeModel -> List (Html Msg)
viewHeader model leftKm rightKm =
    case model.compareInput of
        Just input ->
            [ div [ class "c-km-comparison__header" ]
                [ viewPackage leftKm input.leftPackage input.leftVersion input.leftTags
                , viewPackage rightKm input.rightPackage input.rightVersion input.rightTags
                ]
            ]

        Nothing ->
            []


viewPackage : KnowledgeModel -> KnowledgeModelPackageDetail -> Uuid -> List String -> Html Msg
viewPackage km package versionUuid tagUuids =
    let
        tags =
            List.filterMap (\uuid -> KnowledgeModel.getTag uuid km) tagUuids

        tagsView =
            if List.isEmpty tags then
                Html.nothing

            else
                Tag.list (List.map (\tag -> Tag.withColor tag.color (Tag.tag tag.name)) tags)
    in
    div [ class "c-km-comparison__package" ]
        [ ItemIcon.toHtml (ItemIcon.view package.name)
        , div [ class "c-km-comparison__package-text" ]
            [ div [ class "c-km-comparison__package-name" ]
                [ span [ class "ui-subtitle" ] [ text package.name ]
                , Badge.toHtml (Badge.neutral (Version.toString (CompareInput.versionOf package versionUuid)))
                ]
            , Text.muted package.description
            , tagsView
            ]
        ]



-- ROWS


{-| The words a row says about itself. Built once for a whole fold rather than
per row, so the lazy rows below take primitives.
-}
type alias Words =
    { added : String
    , removed : String
    , changed : String
    , before : String
    , after : String
    , show : String
    , hide : String
    }


stateWords : AppState -> Words
stateWords appState =
    { added = gettext "Added" appState.locale
    , removed = gettext "Removed" appState.locale
    , changed = gettext "Changed" appState.locale
    , before = gettext "before" appState.locale
    , after = gettext "after" appState.locale
    , show = gettext "Show what is inside" appState.locale
    , hide = gettext "Hide what is inside" appState.locale
    }


viewKeyedRow : Words -> Model -> Row -> ( String, Html Msg )
viewKeyedRow words model row =
    ( row.id, viewRow words (Set.member row.id model.collapsed) row )


viewRow : Words -> Bool -> Row -> Html Msg
viewRow words collapsed row =
    let
        bothSides =
            Maybe.isJust row.left && Maybe.isJust row.right
    in
    li
        [ class "c-km-comparison__row"
        , class (stateClass row.state)
        , attribute "style" ("--c-km-comparison-level:" ++ String.fromInt row.level)
        , attribute "aria-level" (String.fromInt (row.level + 1))
        ]
        [ viewStatus words row.state
        , viewCell words collapsed row bothSides Leading row.left
        , viewCell words collapsed row bothSides Trailing row.right
        ]


type Side
    = Leading
    | Trailing


{-| The state, once per row rather than once per column: an icon for the eye and
a word for everything else (1.4.1). An unchanged row says nothing.
-}
viewStatus : Words -> State -> Html Msg
viewStatus words state =
    case statusOf words state of
        Just ( icon, word ) ->
            span [ class "c-km-comparison__status" ]
                [ Icon.toHtml (Icon.labeled word icon) ]

        Nothing ->
            span [ class "c-km-comparison__status" ] []


statusOf : Words -> State -> Maybe ( Icon, String )
statusOf words state =
    case state of
        Added ->
            Just ( Icon.solid "fa-plus", words.added )

        Removed ->
            Just ( Icon.solid "fa-minus", words.removed )

        Changed ->
            Just ( Icon.solid "fa-pen", words.changed )

        Unchanged ->
            Nothing

        Section ->
            Nothing


viewCell : Words -> Bool -> Row -> Bool -> Side -> Maybe String -> Html Msg
viewCell words collapsed row bothSides side content =
    case ( content, row.entity ) of
        ( Nothing, _ ) ->
            div [ class "c-km-comparison__cell c-km-comparison__cell--empty" ] []

        ( Just title, Nothing ) ->
            div [ class "c-km-comparison__cell c-km-comparison__cell--section" ]
                [ span [ class "c-km-comparison__indent" ] []
                , span [ class "c-km-comparison__section" ] [ text title ]
                ]

        ( Just title, Just entity ) ->
            div [ class "c-km-comparison__cell" ]
                [ span [ class "c-km-comparison__indent" ] []
                , viewToggle words collapsed row
                , button
                    [ type_ "button"
                    , class "c-km-comparison__entity"
                    , onClick (OpenEntity entity)
                    ]
                    [ Icon.toHtml (entityIcon entity)
                    , span [ class "c-km-comparison__title" ] [ text title ]
                    , viewSideWord words bothSides side
                    ]
                ]


{-| Both columns of a changed row hold a control with the same name. The side
each one is on is what tells them apart, so each says which it is — to the ear
only, since the eye can see it.
-}
viewSideWord : Words -> Bool -> Side -> Html Msg
viewSideWord words bothSides side =
    if bothSides then
        Text.visuallyHiddenInline
            (", "
                ++ (case side of
                        Leading ->
                            words.before

                        Trailing ->
                            words.after
                   )
            )

    else
        Html.nothing


viewToggle : Words -> Bool -> Row -> Html Msg
viewToggle words collapsed row =
    if row.hasChildren then
        button
            [ type_ "button"
            , class "c-km-comparison__toggle"
            , attribute "aria-expanded"
                (if collapsed then
                    "false"

                 else
                    "true"
                )
            , attribute "aria-label"
                (if collapsed then
                    words.show

                 else
                    words.hide
                )
            , onClick (ToggleCollapse row.id)
            ]
            [ Icon.toHtml
                (Icon.solid
                    (if collapsed then
                        "fa-chevron-right"

                     else
                        "fa-chevron-down"
                    )
                )
            ]

    else
        span [ class "c-km-comparison__toggle c-km-comparison__toggle--none" ] []


stateClass : State -> String
stateClass state =
    case state of
        Added ->
            "c-km-comparison__row--added"

        Removed ->
            "c-km-comparison__row--removed"

        Changed ->
            "c-km-comparison__row--changed"

        Unchanged ->
            "c-km-comparison__row--unchanged"

        Section ->
            "c-km-comparison__row--section"


{-| The wizard draws an expert with the chapter's icon and a resource page with
the reference's. Both are fixed here.
-}
entityIcon : Rows.EntityDiff -> Icon
entityIcon entity =
    case entity of
        Rows.ChapterDiff _ ->
            Icon.regular "fa-file"

        Rows.QuestionDiff _ ->
            Icon.regular "fa-comment"

        Rows.AnswerDiff _ ->
            Icon.regular "fa-circle-dot"

        Rows.ChoiceDiff _ ->
            Icon.regular "fa-square-check"

        Rows.ReferenceDiff _ ->
            Icon.regular "fa-bookmark"

        Rows.ExpertDiff _ ->
            Icon.regular "fa-user"

        Rows.MetricDiff _ ->
            Icon.solid "fa-chart-column"

        Rows.PhaseDiff _ ->
            Icon.regular "fa-clock"

        Rows.TagDiff _ ->
            Icon.solid "fa-tag"

        Rows.IntegrationDiff _ ->
            Icon.solid "fa-right-left"

        Rows.ResourceCollectionDiff _ ->
            Icon.solid "fa-book"

        Rows.ResourcePageDiff _ ->
            Icon.regular "fa-file-lines"
