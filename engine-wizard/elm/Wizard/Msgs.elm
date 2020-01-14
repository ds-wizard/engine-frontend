module Wizard.Msgs exposing (Msg(..))

import Browser exposing (UrlRequest)
import Time
import Url exposing (Url)
import Wizard.Auth.Msgs
import Wizard.Common.Menu.Msgs
import Wizard.Dashboard.Msgs
import Wizard.KMEditor.Msgs
import Wizard.KnowledgeModels.Msgs
import Wizard.Organization.Msgs
import Wizard.Public.Msgs
import Wizard.Questionnaires.Msgs
import Wizard.Users.Msgs


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | OnTime Time.Posix
    | AuthMsg Wizard.Auth.Msgs.Msg
    | SetSidebarCollapsed Bool
    | MenuMsg Wizard.Common.Menu.Msgs.Msg
    | DashboardMsg Wizard.Dashboard.Msgs.Msg
    | KMEditorMsg Wizard.KMEditor.Msgs.Msg
    | KnowledgeModelsMsg Wizard.KnowledgeModels.Msgs.Msg
    | OrganizationMsg Wizard.Organization.Msgs.Msg
    | PublicMsg Wizard.Public.Msgs.Msg
    | QuestionnairesMsg Wizard.Questionnaires.Msgs.Msg
    | UsersMsg Wizard.Users.Msgs.Msg
