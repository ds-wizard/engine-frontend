module Common.Components.LibraryDeleteModal exposing
    ( Kind(..)
    , Model
    , Msg
    , Target
    , UpdateConfig
    , initialModel
    , open
    , target
    , update
    , view
    )

{-| Deleting a knowledge model package or a document template, one version or
all of them.

The dialog asks the server what the deletion takes with it before anything else
(`/dependents`), lists what the person may see by name and the rest as counts,
and has them type the id to confirm. Delete stays disabled until the
dependents are in and the server allows the deletion: it refuses when some of
what would go is hidden from the person.

What goes with each:

  - **A knowledge model**: the knowledge models derived from it, the editors
    and projects based on any of them.
  - **A document template**: the documents made from it. Projects that use it
    stay and lose it.

Links to dependents open in a new tab and say so.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.DocumentTemplates as DocumentTemplatesApi
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.Models.LibraryDependents as LibraryDependents exposing (Hidden, LibraryDependents)
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.RowPolicy exposing (RowPolicy)
import Common.Ports.Dom as Dom
import Common.Routes as Routes
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, li, p, strong, text, ul)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Alert as Alert
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Field as Field
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.ListItem as ListItem
import Ui.Modal as Modal
import Ui.Page.Loading as Loading
import Ui.Separator as Separator
import Ui.Text as Text
import Uuid exposing (Uuid)
import Version exposing (Version)


type Kind
    = KnowledgeModel
    | DocumentTemplate


{-| The row to delete. With all versions, the version only tells the row
apart; every version in its library goes.
-}
type alias Target =
    { kind : Kind
    , uuid : Uuid
    , id : String
    , version : Version
    }


target : Kind -> { a | uuid : Uuid, id : String, version : Version } -> Target
target kind row =
    { kind = kind, uuid = row.uuid, id = row.id, version = row.version }


type alias Model =
    { toBeDeleted : Maybe Target
    , dependents : ActionResult LibraryDependents
    , allVersions : Bool
    , deleting : ActionResult String
    , confirmInputValue : String
    , confirmSubmitted : Bool
    }


initialModel : Bool -> Model
initialModel allVersions =
    { toBeDeleted = Nothing
    , dependents = Unset
    , allVersions = allVersions
    , deleting = Unset
    , confirmInputValue = ""
    , confirmSubmitted = False
    }


type Msg
    = Open Target
    | Close
    | GetDependentsCompleted (Result ApiError LibraryDependents)
    | UpdateConfirmInputValue String
    | Delete
    | DeleteCompleted (Result ApiError ())


open : Target -> Msg
open =
    Open


{-| `afterCloseCmd` runs when the dialog is dismissed without deleting — for
putting focus back on the menu that opened it.
-}
type alias UpdateConfig msg =
    { afterDeleteCmd : Cmd msg
    , afterCloseCmd : Cmd msg
    , wrapMsg : Msg -> msg
    }


confirmInputId : String
confirmInputId =
    "library-delete-confirm"


update : AppState -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update appState cfg msg model =
    case msg of
        Open row ->
            let
                getDependents =
                    case row.kind of
                        KnowledgeModel ->
                            KnowledgeModelPackagesApi.getKnowledgeModelPackageDependents

                        DocumentTemplate ->
                            DocumentTemplatesApi.getTemplateDependents
            in
            ( { model | toBeDeleted = Just row, dependents = Loading }
            , Cmd.map cfg.wrapMsg <| getDependents appState row.uuid model.allVersions GetDependentsCompleted
            )

        Close ->
            -- The dialog also reports closing after a delete, when there is
            -- nothing any more to return focus to.
            ( initialModel model.allVersions
            , if Maybe.isNothing model.toBeDeleted then
                Cmd.none

              else
                cfg.afterCloseCmd
            )

        GetDependentsCompleted result ->
            case result of
                Ok dependents ->
                    ( { model | dependents = Success dependents }
                    , if dependents.deleteAllowed then
                        Dom.focus ("#" ++ confirmInputId)

                      else
                        Cmd.none
                    )

                Err error ->
                    ( { model | dependents = ApiError.toActionResult appState (gettext "Unable to get what depends on it." appState.locale) error }
                    , Cmd.none
                    )

        UpdateConfirmInputValue value ->
            ( { model | confirmInputValue = value }, Cmd.none )

        Delete ->
            case model.toBeDeleted of
                Just row ->
                    if ActionResult.isLoading model.deleting || not (canDelete model) then
                        ( model, Cmd.none )

                    else if model.confirmInputValue /= label model.allVersions row then
                        ( { model | confirmSubmitted = True }
                        , Dom.focus ("#" ++ confirmInputId)
                        )

                    else
                        let
                            delete =
                                case row.kind of
                                    KnowledgeModel ->
                                        KnowledgeModelPackagesApi.deleteKnowledgeModelPackage

                                    DocumentTemplate ->
                                        DocumentTemplatesApi.deleteTemplate
                        in
                        ( { model | deleting = Loading, confirmSubmitted = True }
                        , Cmd.map cfg.wrapMsg <| delete appState row.uuid model.allVersions DeleteCompleted
                        )

                Nothing ->
                    ( model, Cmd.none )

        DeleteCompleted result ->
            case result of
                Ok _ ->
                    ( initialModel model.allVersions
                    , cfg.afterDeleteCmd
                    )

                Err error ->
                    ( { model | deleting = ApiError.toActionResult appState (couldNotDelete appState model) error }
                    , Cmd.none
                    )


canDelete : Model -> Bool
canDelete model =
    case model.dependents of
        Success dependents ->
            dependents.deleteAllowed

        _ ->
            False


couldNotDelete : AppState -> Model -> String
couldNotDelete appState model =
    case Maybe.map .kind model.toBeDeleted of
        Just DocumentTemplate ->
            gettext "Document template could not be deleted." appState.locale

        _ ->
            gettext "Knowledge model could not be deleted." appState.locale


{-| What the person types to confirm: the id, with the version when only one
version goes.
-}
label : Bool -> Target -> String
label allVersions row =
    if allVersions then
        row.id

    else
        row.id ++ ":" ++ Version.toString row.version



-- VIEW


view : RowPolicy -> AppState -> Model -> Html Msg
view policy appState model =
    let
        rowLabel =
            Maybe.unwrap "" (label model.allVersions) model.toBeDeleted

        kind =
            Maybe.unwrap KnowledgeModel .kind model.toBeDeleted

        body =
            case model.dependents of
                Success dependents ->
                    viewDependents policy appState kind model.allVersions rowLabel dependents
                        ++ (if dependents.deleteAllowed then
                                viewConfirm appState model rowLabel

                            else
                                []
                           )

                Error message ->
                    [ Alert.toHtml (Alert.danger message) ]

                _ ->
                    [ Loading.toHtml (gettext "Loading..." appState.locale) ]

        -- Nothing can be deleted before the server has said what goes with
        -- it. When it refuses, the button says why.
        deleteButton =
            case model.dependents of
                Success dependents ->
                    if dependents.deleteAllowed then
                        Button.dangerSubmit (gettext "Delete" appState.locale)

                    else
                        Button.dangerSubmit (gettext "Delete" appState.locale)
                            |> Button.withDisabledReason
                                { id = "library-delete-reason"
                                , reason = gettext "It is also used where you have no access." appState.locale
                                }

                _ ->
                    Button.dangerSubmit (gettext "Delete" appState.locale)
                        |> Button.withDisabled True

        testId =
            case kind of
                KnowledgeModel ->
                    "km-delete"

                DocumentTemplate ->
                    "template-delete"
    in
    Modal.plain
        { title = String.format (gettext "Delete %s" appState.locale) [ rowLabel ]
        , open = Maybe.isJust model.toBeDeleted
        , onClose = Close
        , body = body
        }
        |> Modal.withForm { onSubmit = Delete, submitLabel = gettext "Delete" appState.locale }
        |> Modal.withActions
            [ Button.ghost (gettext "Cancel" appState.locale) Close
            , deleteButton
                |> Button.withLoading (ActionResult.isLoading model.deleting)
                |> Button.withTestId (testId ++ "_submit")
            ]
        |> Modal.withTestId testId
        |> Modal.toHtml


viewConfirm : AppState -> Model -> String -> List (Html Msg)
viewConfirm appState model rowLabel =
    let
        confirmLabel =
            String.format (gettext "Type \"%s\" to confirm" appState.locale) [ rowLabel ]

        confirmError =
            if model.confirmSubmitted && model.confirmInputValue /= rowLabel then
                Just confirmLabel

            else
                Nothing
    in
    [ Separator.line
    , Alert.toHtml (Alert.warning (gettext "This action cannot be undone." appState.locale))
    , Field.input
        { id = confirmInputId
        , label = confirmLabel
        , value = model.confirmInputValue
        , onInput = UpdateConfirmInputValue
        }
        |> Field.withRequired True
        |> Field.withAutocomplete "off"
        |> Field.withError confirmError
        |> Field.toHtml
    , case model.deleting of
        Error message ->
            Alert.toHtml (Alert.danger message)

        _ ->
            text ""
    ]


viewDependents : RowPolicy -> AppState -> Kind -> Bool -> String -> LibraryDependents -> List (Html msg)
viewDependents policy appState kind allVersions rowLabel dependents =
    let
        explanation =
            case ( kind, allVersions ) of
                ( KnowledgeModel, True ) ->
                    gettext "You are about to permanently delete all versions of %s, together with the knowledge models derived from them and the knowledge model editors and projects based on any of them." appState.locale

                ( KnowledgeModel, False ) ->
                    gettext "You are about to permanently delete %s, together with the knowledge models derived from it and the knowledge model editors and projects based on any of them." appState.locale

                ( DocumentTemplate, True ) ->
                    gettext "You are about to permanently delete all versions of %s, together with the documents made from them. Projects using them are kept and lose their document template." appState.locale

                ( DocumentTemplate, False ) ->
                    gettext "You are about to permanently delete %s, together with the documents made from it. Projects using it are kept and lose their document template." appState.locale

        newTabLink name url =
            Link.link { label = name, url = url }
                |> Link.external
                |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                |> Link.toHtml

        -- An editor or a project is in a workspace. Seen from the
        -- organization's own pages there is no workspace to open it in, so it
        -- is named rather than linked.
        workspaceLink name route =
            if policy.derive then
                newTabLink name (Routes.toUrl appState route)

            else
                text name

        namedRow toTitle namedItem =
            ListItem.item (toTitle namedItem)
                |> ListItem.withIcon (itemIcon namedItem.name)

        projectsDeleted =
            kind == KnowledgeModel

        groups =
            viewGroup
                { title = gettext "Knowledge models that will be deleted" appState.locale
                , items = dependents.knowledgeModelPackages
                , viewItem =
                    \package ->
                        ListItem.item (newTabLink package.name (Routes.toUrl appState (Routes.knowledgeModelsDetail package.uuid)))
                            |> ListItem.withIcon (itemIcon package.name)
                            |> ListItem.withFragments
                                [ Text.code package.id
                                , Badge.toHtml (Badge.neutral (Version.toString package.version))
                                ]
                }
                ++ viewGroup
                    { title = gettext "Knowledge model editors that will be deleted" appState.locale
                    , items = dependents.editors
                    , viewItem = namedRow (\editor -> workspaceLink editor.name (Routes.kmEditorEditor editor.uuid Nothing))
                    }
                ++ viewGroup
                    { title =
                        if projectsDeleted then
                            gettext "Projects that will be deleted" appState.locale

                        else
                            gettext "Projects that will lose the document template" appState.locale
                    , items = dependents.projects
                    , viewItem = namedRow (\project -> workspaceLink project.name (Routes.projectsDetail project.uuid))
                    }
                ++ viewGroup
                    { title = gettext "Documents that will be deleted" appState.locale
                    , items = dependents.documents
                    , viewItem = namedRow (.name >> text)
                    }

        nothingVisible =
            List.isEmpty groups && LibraryDependents.hiddenTotal dependents.hidden == 0
    in
    [ Layout.stack
        (p [] (String.formatHtml explanation [ strong [] [ text rowLabel ] ])
            :: (if nothingVisible then
                    [ Text.body (gettext "Nothing else depends on it." appState.locale) ]

                else
                    viewSummary appState projectsDeleted dependents ++ groups
               )
            ++ viewHidden appState dependents.hidden
        )
    ]


{-| How much goes, at a glance, before the lists that may be long. Projects
that only lose a document template are not deleted, so they are left to their
own list.
-}
viewSummary : AppState -> Bool -> LibraryDependents -> List (Html msg)
viewSummary appState projectsDeleted dependents =
    let
        countBadge items msgids =
            let
                count =
                    List.length items
            in
            if count > 0 then
                [ Badge.toHtml (Badge.danger (String.format (ngettext msgids count appState.locale) [ String.fromInt count ])) ]

            else
                []

        deleted =
            countBadge dependents.knowledgeModelPackages ( "%s knowledge model", "%s knowledge models" )
                ++ countBadge dependents.editors ( "%s knowledge model editor", "%s knowledge model editors" )
                ++ (if projectsDeleted then
                        countBadge dependents.projects ( "%s project", "%s projects" )

                    else
                        []
                   )
                ++ countBadge dependents.documents ( "%s document", "%s documents" )
    in
    if List.isEmpty deleted then
        []

    else
        [ Layout.inline (strong [] [ text (gettext "This will also permanently delete:" appState.locale) ] :: deleted) ]


itemIcon : String -> Html msg
itemIcon name =
    ItemIcon.toHtml (ItemIcon.withSize ItemIcon.Small (UserAvatar.item name Nothing))


{-| A group's count is a badge beside its title; the list itself is a `ul`, so
a screen reader says how many there are anyway.
-}
viewGroup : { title : String, items : List a, viewItem : a -> ListItem.ListItem msg } -> List (Html msg)
viewGroup props =
    if List.isEmpty props.items then
        []

    else
        [ Layout.stack
            [ Layout.inline
                [ Text.label props.title
                , Badge.toHtml (Badge.neutral (String.fromInt (List.length props.items)))
                ]
            , ListItem.tiles (List.map props.viewItem props.items)
            ]
        ]


{-| What the person may not see, as counts. It is why the server refuses the
deletion, so it is said as the reason.
-}
viewHidden : AppState -> Hidden -> List (Html msg)
viewHidden appState hidden =
    let
        countItem count msgids =
            if count > 0 then
                [ li [] [ text (String.format (ngettext msgids count appState.locale) [ String.fromInt count ]) ] ]

            else
                []
    in
    if LibraryDependents.hiddenTotal hidden == 0 then
        []

    else
        let
            workspaces =
                if hidden.workspaces > 1 then
                    [ p [] [ text (String.format (ngettext ( "They are in %s workspace.", "They are in %s workspaces." ) hidden.workspaces appState.locale) [ String.fromInt hidden.workspaces ]) ] ]

                else
                    []
        in
        [ Alert.danger ""
            |> Alert.toHtmlWithContent
                ([ p [] [ text (gettext "You cannot delete it: it is also used where you have no access." appState.locale) ]
                 , ul []
                    (countItem hidden.knowledgeModelPackages ( "%s knowledge model", "%s knowledge models" )
                        ++ countItem hidden.editors ( "%s knowledge model editor", "%s knowledge model editors" )
                        ++ countItem hidden.projects ( "%s project", "%s projects" )
                        ++ countItem hidden.documents ( "%s document", "%s documents" )
                    )
                 ]
                    ++ workspaces
                    ++ [ p [] [ text (gettext "Ask someone who can see all of them to delete it." appState.locale) ] ]
                )
        ]
