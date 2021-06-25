module Wizard.Projects.Create.Msgs exposing (Msg(..))

import Wizard.Projects.Create.CustomCreate.Msgs as CustomCreateMsgs
import Wizard.Projects.Create.TemplateCreate.Msgs as TemplateCreateMsgs


type Msg
    = TemplateCreateMsg TemplateCreateMsgs.Msg
    | CustomCreateMsg CustomCreateMsgs.Msg
