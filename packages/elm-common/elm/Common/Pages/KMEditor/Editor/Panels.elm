module Common.Pages.KMEditor.Editor.Panels exposing
    ( Config
    , Panel(..)
    , UrlChecker
    , brokenCount
    , check
    , title
    , urlCheckerInit
    , view
    , warningCount
    )

{-| The two panels beside the editor: what is not finished, and which links are
broken.

**Warnings** are computed from the knowledge model on every event
(`EditorContext.computeWarnings`), so the count beside the toolbar button is
what is wrong right now. Each one is a link to the entity it belongs to.

**The URL checker** asks a separate service — `appState.urlCheckerUrl`, which a
deployment may not have — whether every URL reference in the knowledge model
answers. It is asked on a press rather than on arrival, because it makes a
request per link.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Reference.URLReferenceData as URLReferenceData exposing (URLReferenceData)
import Common.Api.Models.UrlCheckResponse as UrlCheckResponse exposing (UrlCheckResponse)
import Common.Api.Models.UrlCheckResponse.UrlResult as UrlResult
import Common.Pages.KMEditor.Editor.EditorContext exposing (EditorContext)
import Common.Pages.KMEditor.Editor.Warnings exposing (Warning)
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, a, div, li, p, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import List.Extra as List
import String.Format as String
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Text as Text


type Panel
    = Warnings
    | UrlCheck


type alias UrlChecker =
    ActionResult UrlCheckResponse


urlCheckerInit : UrlChecker
urlCheckerInit =
    Unset


type alias Config msg =
    { locale : Gettext.Locale
    , entityUrl : String -> String
    , onCheckUrls : List String -> msg

    -- Which warning is being followed, by the field it is about, and whether
    -- this click is the one that navigates. A plain click is answered here so
    -- the field is known before the page changes; a click with a modifier is
    -- the browser opening a new tab, and nothing is followed in this one.
    , onFollowWarning : { url : String, field : Maybe String, follow : Bool } -> msg
    }


title : Config msg -> Panel -> String
title config panel =
    case panel of
        Warnings ->
            gettext "Warnings" config.locale

        UrlCheck ->
            gettext "URL Checker" config.locale


warningCount : EditorContext -> Int
warningCount editorContext =
    List.length editorContext.warnings


{-| How many URL references did not answer. Nothing has been checked until the
button is pressed, and an unchecked link is not a broken one.
-}
brokenCount : EditorContext -> UrlChecker -> Int
brokenCount editorContext urlChecker =
    case urlChecker of
        Success response ->
            urlReferences editorContext
                |> List.filterNot (\reference -> answered reference response)
                |> List.length

        _ ->
            0


answered : URLReferenceData -> UrlCheckResponse -> Bool
answered reference response =
    UrlCheckResponse.getResultByUrl reference.url response
        |> Maybe.map .ok
        |> Maybe.withDefault True


urlReferences : EditorContext -> List URLReferenceData
urlReferences editorContext =
    KnowledgeModel.getAllUrlReferences editorContext.kmEditor.knowledgeModel


{-| The workspace leaves a panel flush, because a panel of rows that highlight
wants the whole width. This one is lists and prose, which want the gutter the
rest of the workspace has.
-}
view : Config msg -> EditorContext -> UrlChecker -> Panel -> List (Html msg)
view config editorContext urlChecker panel =
    [ div [ class "c-km-editor-panel" ]
        (case panel of
            Warnings ->
                viewWarnings config editorContext

            UrlCheck ->
                viewUrlChecker config editorContext urlChecker
        )
    ]


{-| A click on a warning, unless it is the kind of click that opens a new tab —
which the browser should go on handling by itself.
-}
onFollow : Config msg -> Warning -> Html.Attribute msg
onFollow config warning =
    let
        follow ctrl meta shift =
            let
                isPlain =
                    not ctrl && not meta && not shift
            in
            ( config.onFollowWarning
                { url = config.entityUrl warning.editorUuid
                , field = warning.field
                , follow = isPlain
                }
            , isPlain
            )
    in
    preventDefaultOn "click"
        (D.map3 follow
            (D.field "ctrlKey" D.bool)
            (D.field "metaKey" D.bool)
            (D.field "shiftKey" D.bool)
        )


viewWarnings : Config msg -> EditorContext -> List (Html msg)
viewWarnings config editorContext =
    if List.isEmpty editorContext.warnings then
        [ Alert.toHtml (Alert.info (gettext "There are no more warnings." config.locale)) ]

    else
        [ ul [ class "c-km-editor-panel__list" ]
            (List.map
                (\warning ->
                    li []
                        [ -- A real link — it has an `href`, so middle-click,
                          -- copy link and the screen reader's list of links all
                          -- work. A plain click is answered here instead of by
                          -- the browser, because which warning was followed has
                          -- to be known before the page changes: an entity can
                          -- have several, about different fields, and the one
                          -- to answer is the one that was clicked. `Ui.Link`
                          -- carries no message, so the anchor is built here
                          -- with its class.
                          a
                            [ class "ui-link"
                            , href (config.entityUrl warning.editorUuid)
                            , onFollow config warning
                            ]
                            [ text warning.message ]
                        ]
                )
                editorContext.warnings
            )
        ]


viewUrlChecker : Config msg -> EditorContext -> UrlChecker -> List (Html msg)
viewUrlChecker config editorContext urlChecker =
    let
        references =
            urlReferences editorContext
    in
    if List.isEmpty references then
        [ Alert.toHtml (Alert.info (gettext "There are no URL references in the knowledge model to check." config.locale)) ]

    else
        [ Layout.stack
            [ Text.muted
                (String.format
                    (ngettext ( "There is %s URL reference to check.", "There are %s URL references to check." )
                        (List.length references)
                        config.locale
                    )
                    [ String.fromInt (List.length references) ]
                )
            , Button.toHtml
                (Button.secondary (gettext "Check URLs" config.locale) (config.onCheckUrls (List.unique (List.map .url references)))
                    |> Button.withIcon (Icon.solid "fa-link")
                    |> Button.withLoading (ActionResult.isLoading urlChecker)
                    |> Button.withTestId "km-editor_check-urls"
                )
            , viewUrlCheckResult config references urlChecker
            ]
        ]


viewUrlCheckResult : Config msg -> List URLReferenceData -> UrlChecker -> Html msg
viewUrlCheckResult config references urlChecker =
    case urlChecker of
        Success response ->
            let
                broken =
                    List.filterNot (\reference -> answered reference response) references
            in
            if List.isEmpty broken then
                Alert.toHtml (Alert.success (gettext "Every URL reference answered." config.locale))

            else
                Layout.stack
                    (Alert.toHtml
                        (Alert.warning
                            (String.format
                                (ngettext ( "%s reference did not answer.", "%s references did not answer." ) (List.length broken) config.locale)
                                [ String.fromInt (List.length broken) ]
                            )
                        )
                        :: List.map (viewBroken config response) broken
                    )

        Error error ->
            Alert.toHtml (Alert.danger error)

        _ ->
            text ""


viewBroken : Config msg -> UrlCheckResponse -> URLReferenceData -> Html msg
viewBroken config response reference =
    let
        why =
            UrlCheckResponse.getResultByUrl reference.url response
                |> Maybe.andThen (UrlResult.toReadableErrorString config.locale)
                |> Maybe.withDefault (gettext "Unknown error." config.locale)
    in
    div [ class "c-km-editor-panel__broken" ]
        [ Link.toHtml (Link.link { url = config.entityUrl reference.uuid, label = URLReferenceData.toLabel reference })
        , p [] [ Text.muted why ]
        ]


{-| The request the Check URLs button makes, and what comes back.
-}
check : { a | locale : Gettext.Locale } -> Result ApiError UrlCheckResponse -> UrlChecker
check appState result =
    case result of
        Ok response ->
            Success response

        Err error ->
            ApiError.toActionResult appState (gettext "Unable to check the URLs." appState.locale) error
