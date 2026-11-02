module Common.Pages.CommandPalette exposing (Destination, items, shortcut, view)

{-| The palette the application opens with Cmd+K, and the key that opens it.

What is in it is every page the application has and can be reached by its
address alone: the pages in the navigation, the sections of the settings, the
tabs of the profile, and the pages that make something new. Each of them once —
a group in the navigation opens its first child, and a row that opens a page
another row already opens is worse than no row at all.

What is not in it is anything that needs a question put to the server first: a
project, a knowledge model, a document. Where that changes, it changes in
`items` — the palette takes whatever list it is given and says so while more of
it is on its way, so nothing in `Ui.CommandPalette` has to change for it.

-}

import Common.Components.UiWords as UiWords
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.LocaleKind exposing (LocaleKind(..))
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Pages.Settings as Settings
import Common.Pages.Settings.Routes as SettingsRoutes
import Common.Routes as Routes
import Gettext exposing (gettext)
import Html exposing (Html)
import Html.Attributes exposing (attribute, style)
import Shortcut
import Ui.CommandPalette as CommandPalette
import Ui.Icon as Icon


{-| A page of the client's navigation, as `Common.Components.AppFrame.destinations`
gives it: each client has its own navigation and hands it in. A place the client
draws a mark for — a workspace, say — brings it as `leading`; everything else is a
page, and has the arrow every page has here.
-}
type alias Destination =
    { id : String, label : String, section : String, url : String, leading : Maybe (Html Never) }


{-| The pages the palette offers for what has been typed. The filter is the
palette's own rule, applied here rather than there so that a list which one day
also comes from a server can be assembled before it is shown.
-}
items : (Routes.Route -> Bool) -> AppState -> List Destination -> CommandPalette.Model -> List CommandPalette.Item
items reachable appState navigation model =
    destinations reachable appState navigation
        |> List.filter (CommandPalette.matches (CommandPalette.query model))


destinations : (Routes.Route -> Bool) -> AppState -> List Destination -> List CommandPalette.Item
destinations reachable appState navigation =
    List.concat
        [ navigationPages navigation
        , List.map asPage (creationPages reachable appState)
        , List.map asPage (settingsPages appState)
        , List.map asPage (profilePages appState)
        ]


{-| A page: the arrow says the row goes somewhere, as every row of the palette
does, and sets the pages apart from the places a client draws its own mark for.
-}
asPage : CommandPalette.Item -> CommandPalette.Item
asPage =
    CommandPalette.withIcon (Icon.solid "fa-arrow-right")


{-| The pages of the navigation, and only the pages: `AppFrame.destinations`
drops a group's own link, which goes where its first child goes.
-}
navigationPages : List Destination -> List CommandPalette.Item
navigationPages navigation =
    List.map
        (\destination ->
            CommandPalette.item
                { id = destination.id
                , label = destination.label
                , section = destination.section
                , url = destination.url
                }
                |> (case destination.leading of
                        Just mark ->
                            CommandPalette.withLeading mark

                        Nothing ->
                            asPage
                   )
        )
        navigation


{-| The settings, from the settings menu itself, so a section hidden there —
a plugin's, the registry's — is missing here too. Its
groups are the sections, as they are in the menu.
-}
settingsPages : AppState -> List CommandPalette.Item
settingsPages appState =
    let
        sectionName group =
            Maybe.withDefault (gettext "Settings" appState.locale) (Tuple.first group)

        toItem group entry =
            CommandPalette.item
                { id = "settings-" ++ entry.url
                , label = entry.label
                , section = sectionName group
                , url = entry.url
                }
    in
    -- The route is what the menu marks as the one you are on, which is nothing
    -- to do with the palette; any of them gives the same list.
    Settings.navigation (Settings.defaultMount appState) appState (SettingsRoutes.defaultRoute appState)
        |> List.concatMap (\group -> List.map (toItem group) (Tuple.second group))


{-| The tabs of the account's own settings, under one heading. Each is behind
the permission that shows it on the page.
-}
profilePages : AppState -> List CommandPalette.Item
profilePages appState =
    let
        section =
            gettext "Profile" appState.locale

        current =
            UuidOrCurrent.current

        tab id label url visible =
            if visible then
                [ CommandPalette.item { id = id, label = label, section = section, url = Routes.toUrl appState url } ]

            else
                []
    in
    List.concat
        [ tab "profile" (gettext "Profile" appState.locale) (Routes.usersEdit current) True
        , tab "profile-password" (gettext "Password" appState.locale) (Routes.usersEditPassword current) True
        , tab "profile-connected-accounts"
            (gettext "Connected Accounts" appState.locale)
            Routes.usersEditConnectedAccounts
            (Feature.userEditConnectedAccounts appState current)
        , tab "profile-language"
            (gettext "Language" appState.locale)
            (Routes.usersEditLanguage current)
            (Feature.userEditLanguage appState current)
        , tab "profile-tours"
            (gettext "Tours" appState.locale)
            (Routes.usersEditTours current)
            (appState.config.features.toursEnabled && Feature.userEditTours appState current)
        , tab "profile-api-keys"
            (gettext "API Keys" appState.locale)
            (Routes.usersEditApiKeys current)
            (Feature.userEditApiKeys appState current)
        , tab "profile-active-sessions"
            (gettext "Active Sessions" appState.locale)
            (Routes.usersEditActiveSessions current)
            (Feature.userEditActiveSessions appState current)
        , tab "profile-submission-settings"
            (gettext "Submission Settings" appState.locale)
            (Routes.usersEditSubmissionSettings current)
            (Feature.userEditSubmissionSettings appState current)
        ]


{-| The pages that make something new. They are named by what they do rather
than by the section they are in — "Create User" is what somebody types, and
"Create" on its own is four different pages.
-}
creationPages : (Routes.Route -> Bool) -> AppState -> List CommandPalette.Item
creationPages reachable appState =
    let
        -- Only what can be reached from here: a client says which pages
        -- those are.
        page id label section url visible =
            if visible && reachable url then
                [ CommandPalette.item { id = id, label = label, section = section, url = Routes.toUrl appState url } ]

            else
                []

        knowledgeModels =
            gettext "Knowledge Models" appState.locale

        documentTemplates =
            gettext "Document Templates" appState.locale

        projects =
            gettext "Projects" appState.locale

        administration =
            gettext "Administration" appState.locale
    in
    List.concat
        [ page "create-project"
            (gettext "Create Project" appState.locale)
            projects
            Routes.projectsCreate
            (Feature.projectsCreate appState)
        , page "create-knowledge-model-editor"
            (gettext "Create Knowledge Model Editor" appState.locale)
            knowledgeModels
            (Routes.kmEditorCreate Nothing Nothing)
            (Feature.knowledgeModelEditorsCreate appState)
        , page "import-knowledge-model"
            (gettext "Import Knowledge Model" appState.locale)
            knowledgeModels
            (Routes.knowledgeModelsImport Nothing)
            (Feature.knowledgeModelsImport appState)
        , page "create-document-template-editor"
            (gettext "Create Document Template Editor" appState.locale)
            documentTemplates
            (Routes.documentTemplateEditorCreate Nothing Nothing)
            (Feature.documentTemplateEditorsCreate appState)
        , page "import-document-template"
            (gettext "Import Document Template" appState.locale)
            documentTemplates
            (Routes.documentTemplatesImport Nothing)
            (Feature.documentTemplatesImport appState)
        , page "create-user"
            (gettext "Create User" appState.locale)
            (gettext "Users" appState.locale)
            Routes.usersCreate
            (Feature.usersCreate appState)
        , page "upload-translation"
            (gettext "Upload translation" appState.locale)
            (gettext "Application Translations" appState.locale)
            (Routes.settingsLocalesUpload UiLocales)
            (Feature.localesManage appState)
        , page "create-tenant"
            "Create Tenant"
            "Tenants"
            Routes.tenantsCreate
            (Feature.tenants appState)
        , page "create-role"
            (gettext "Create Role" appState.locale)
            administration
            Routes.settingsRoleCreate
            (Feature.roles appState)
        , page "create-openid"
            (gettext "Add OpenID Service" appState.locale)
            administration
            Routes.settingsOpenIdCreate
            (Feature.settings appState)
        ]


{-| Cmd+K on a Mac, Ctrl+K elsewhere — the combination every application that
has one of these uses, and the one the browsers give to their own address bar,
which is what it is standing in for here.

`data-in-input` because it has to open from wherever you are, including from a
field you are typing in; `display: none` because the element listens on the
document and has no business taking a box.

-}
shortcut : msg -> Html msg
shortcut onOpen =
    Shortcut.shortcutElement
        [ Shortcut.metaShortcut (Shortcut.Regular "k") onOpen
        , Shortcut.ctrlShortcut (Shortcut.Regular "k") onOpen
        ]
        [ attribute "data-in-input" "", style "display" "none" ]
        []


view :
    AppState
    ->
        { open : Bool
        , navigation : List Destination

        -- Which of the pages that create something can be reached from where
        -- the reader is: the application says.
        , reachable : Routes.Route -> Bool
        , wrapMsg : CommandPalette.Msg -> msg
        , onClose : msg
        }
    -> CommandPalette.Model
    -> Html msg
view appState config model =
    CommandPalette.view
        { open = config.open
        , items = items config.reachable appState config.navigation model
        , loading = False
        , words = UiWords.commandPalette appState
        , wrapMsg = config.wrapMsg
        , onClose = config.onClose
        }
        model
