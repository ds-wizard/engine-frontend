module Wizard.Msgs exposing
    ( Msg(..)
    , logoutMsg
    , logoutToMsg
    )

import Browser exposing (UrlRequest)
import Time
import Url exposing (Url)
import Wizard.Auth.Msgs
import Wizard.Comments.Msgs
import Wizard.Common.Components.AIAssistant
import Wizard.Common.Menu.Msgs
import Wizard.Dashboard.Msgs
import Wizard.Dev.Msgs
import Wizard.DocumentTemplateEditors.Msgs
import Wizard.DocumentTemplates.Msgs
import Wizard.Documents.Msgs
import Wizard.KMEditor.Msgs
import Wizard.KnowledgeModels.Msgs
import Wizard.Locales.Msgs
import Wizard.ProjectActions.Msgs
import Wizard.ProjectImporters.Msgs
import Wizard.Projects.Msgs
import Wizard.Public.Msgs
import Wizard.Registry.Msgs
import Wizard.Routes as Routes
import Wizard.Settings.Msgs
import Wizard.Tenants.Msgs
import Wizard.Users.Msgs


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | HistoryBackCallback String
    | OnTime Time.Posix
    | OnTimeZone Time.Zone
    | AcceptCookies
    | AuthMsg Wizard.Auth.Msgs.Msg
    | AIAssistantMsg Wizard.Common.Components.AIAssistant.Msg
    | SetSidebarCollapsed Bool
    | SetRightPanelCollapsed Bool
    | SetFullscreen Bool
    | SetLocale String
    | HideSessionExpiresSoonModal
    | MenuMsg Wizard.Common.Menu.Msgs.Msg
    | AdminMsg Wizard.Dev.Msgs.Msg
    | CommentsMsg Wizard.Comments.Msgs.Msg
    | DashboardMsg Wizard.Dashboard.Msgs.Msg
    | DocumentsMsg Wizard.Documents.Msgs.Msg
    | DocumentTemplateEditorsMsg Wizard.DocumentTemplateEditors.Msgs.Msg
    | DocumentTemplatesMsg Wizard.DocumentTemplates.Msgs.Msg
    | KMEditorMsg Wizard.KMEditor.Msgs.Msg
    | KnowledgeModelsMsg Wizard.KnowledgeModels.Msgs.Msg
    | LocaleMsg Wizard.Locales.Msgs.Msg
    | ProjectActionsMsg Wizard.ProjectActions.Msgs.Msg
    | ProjectImportersMsg Wizard.ProjectImporters.Msgs.Msg
    | ProjectsMsg Wizard.Projects.Msgs.Msg
    | PublicMsg Wizard.Public.Msgs.Msg
    | RegistryMsg Wizard.Registry.Msgs.Msg
    | SettingsMsg Wizard.Settings.Msgs.Msg
    | TenantsMsg Wizard.Tenants.Msgs.Msg
    | UsersMsg Wizard.Users.Msgs.Msg


logoutMsg : Msg
logoutMsg =
    AuthMsg Wizard.Auth.Msgs.Logout


logoutToMsg : Routes.Route -> Msg
logoutToMsg =
    AuthMsg << Wizard.Auth.Msgs.LogoutTo
