module Shared.Api.Apps exposing (getCurrentPlans)

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet)
import Shared.Data.Plan as Plan exposing (Plan)


getCurrentPlans : AbstractAppState a -> ToMsg (List Plan) msg -> Cmd msg
getCurrentPlans =
    jwtGet "/apps/current/plans" (D.list Plan.decoder)
