module Wizard.Msgs exposing
    ( Msg(..)
    , logoutMsg
    )

import Browser exposing (UrlRequest)
import Time
import Url exposing (Url)
import Wizard.Apps.Msgs
import Wizard.Auth.Msgs
import Wizard.Common.Menu.Msgs
import Wizard.Dashboard.Msgs
import Wizard.Dev.Msgs
import Wizard.Documents.Msgs
import Wizard.KMEditor.Msgs
import Wizard.KnowledgeModels.Msgs
import Wizard.ProjectImporters.Msgs
import Wizard.Projects.Msgs
import Wizard.Public.Msgs
import Wizard.Registry.Msgs
import Wizard.Settings.Msgs
import Wizard.Templates.Msgs
import Wizard.Users.Msgs


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | OnTime Time.Posix
    | OnTimeZone Time.Zone
    | AcceptCookies
    | AuthMsg Wizard.Auth.Msgs.Msg
    | SetSidebarCollapsed Bool
    | SetFullscreen Bool
    | MenuMsg Wizard.Common.Menu.Msgs.Msg
    | AdminMsg Wizard.Dev.Msgs.Msg
    | AppsMsg Wizard.Apps.Msgs.Msg
    | DashboardMsg Wizard.Dashboard.Msgs.Msg
    | DocumentsMsg Wizard.Documents.Msgs.Msg
    | KMEditorMsg Wizard.KMEditor.Msgs.Msg
    | KnowledgeModelsMsg Wizard.KnowledgeModels.Msgs.Msg
    | ProjectImportersMsg Wizard.ProjectImporters.Msgs.Msg
    | ProjectsMsg Wizard.Projects.Msgs.Msg
    | PublicMsg Wizard.Public.Msgs.Msg
    | RegistryMsg Wizard.Registry.Msgs.Msg
    | SettingsMsg Wizard.Settings.Msgs.Msg
    | TemplatesMsg Wizard.Templates.Msgs.Msg
    | UsersMsg Wizard.Users.Msgs.Msg


logoutMsg : Msg
logoutMsg =
    AuthMsg Wizard.Auth.Msgs.Logout
