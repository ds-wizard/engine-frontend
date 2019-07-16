module Msgs exposing (Msg(..))

import Auth.Msgs
import Browser exposing (UrlRequest)
import Common.Menu.Msgs
import Dashboard.Msgs
import KMEditor.Msgs
import KnowledgeModels.Msgs
import Organization.Msgs
import Public.Msgs
import Questionnaires.Msgs
import Time
import Url exposing (Url)
import Users.Msgs


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | OnTime Time.Posix
    | AuthMsg Auth.Msgs.Msg
    | SetSidebarCollapsed Bool
    | MenuMsg Common.Menu.Msgs.Msg
    | DashboardMsg Dashboard.Msgs.Msg
    | KMEditorMsg KMEditor.Msgs.Msg
    | KnowledgeModelsMsg KnowledgeModels.Msgs.Msg
    | OrganizationMsg Organization.Msgs.Msg
    | PublicMsg Public.Msgs.Msg
    | QuestionnairesMsg Questionnaires.Msgs.Msg
    | UsersMsg Users.Msgs.Msg
