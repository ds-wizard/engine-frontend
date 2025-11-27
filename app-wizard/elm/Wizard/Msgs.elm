module Wizard.Msgs exposing
    ( Msg(..)
    , logoutMsg
    , logoutToMsg
    )

import Browser exposing (UrlRequest)
import Common.Components.AIAssistant
import Common.Components.NewsModal as NewsModal
import Time
import Url exposing (Url)
import Wizard.Components.Menu.Msgs
import Wizard.Pages.Auth.Msgs
import Wizard.Pages.Comments.Msgs
import Wizard.Pages.Dashboard.Msgs
import Wizard.Pages.Dev.Msgs
import Wizard.Pages.DocumentTemplateEditors.Msgs
import Wizard.Pages.DocumentTemplates.Msgs
import Wizard.Pages.Documents.Msgs
import Wizard.Pages.KMEditor.Msgs
import Wizard.Pages.KnowledgeModelSecrets.Msgs
import Wizard.Pages.KnowledgeModels.Msgs
import Wizard.Pages.Locales.Msgs
import Wizard.Pages.ProjectActions.Msgs
import Wizard.Pages.ProjectFiles.Msgs
import Wizard.Pages.ProjectImporters.Msgs
import Wizard.Pages.Projects.Msgs
import Wizard.Pages.Public.Msgs
import Wizard.Pages.Registry.Msgs
import Wizard.Pages.Settings.Msgs
import Wizard.Pages.Tenants.Msgs
import Wizard.Pages.Users.Msgs
import Wizard.Routes as Routes


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | HistoryBackCallback String
    | OnTime Time.Posix
    | OnTimeZone Time.Zone
    | AcceptCookies
    | AuthMsg Wizard.Pages.Auth.Msgs.Msg
    | AIAssistantMsg Common.Components.AIAssistant.Msg
    | SetSidebarCollapsed Bool
    | SetRightPanelCollapsed Bool
    | SetFullscreen Bool
    | HideSessionExpiresSoonModal
    | MenuMsg Wizard.Components.Menu.Msgs.Msg
    | AdminMsg Wizard.Pages.Dev.Msgs.Msg
    | CommentsMsg Wizard.Pages.Comments.Msgs.Msg
    | DashboardMsg Wizard.Pages.Dashboard.Msgs.Msg
    | DocumentsMsg Wizard.Pages.Documents.Msgs.Msg
    | DocumentTemplateEditorsMsg Wizard.Pages.DocumentTemplateEditors.Msgs.Msg
    | DocumentTemplatesMsg Wizard.Pages.DocumentTemplates.Msgs.Msg
    | KMEditorMsg Wizard.Pages.KMEditor.Msgs.Msg
    | KnowledgeModelsMsg Wizard.Pages.KnowledgeModels.Msgs.Msg
    | KnowledgeModelSecretsMsg Wizard.Pages.KnowledgeModelSecrets.Msgs.Msg
    | LocaleMsg Wizard.Pages.Locales.Msgs.Msg
    | ProjectActionsMsg Wizard.Pages.ProjectActions.Msgs.Msg
    | ProjectFilesMsg Wizard.Pages.ProjectFiles.Msgs.Msg
    | ProjectImportersMsg Wizard.Pages.ProjectImporters.Msgs.Msg
    | ProjectsMsg Wizard.Pages.Projects.Msgs.Msg
    | PublicMsg Wizard.Pages.Public.Msgs.Msg
    | RegistryMsg Wizard.Pages.Registry.Msgs.Msg
    | SettingsMsg Wizard.Pages.Settings.Msgs.Msg
    | TenantsMsg Wizard.Pages.Tenants.Msgs.Msg
    | UsersMsg Wizard.Pages.Users.Msgs.Msg
    | TourDone String
    | TourPutCompleted
    | NewsModalMsg NewsModal.Msg
    | SetLastSeenNewsId String
    | SetLastSeenNewsIdCompleted


logoutMsg : Msg
logoutMsg =
    AuthMsg Wizard.Pages.Auth.Msgs.Logout


logoutToMsg : Routes.Route -> Msg
logoutToMsg =
    AuthMsg << Wizard.Pages.Auth.Msgs.LogoutTo
