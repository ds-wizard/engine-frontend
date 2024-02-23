module Registry2.Api.Organizations exposing (getOrganization, postOrganization, putOrganizationState)

import Json.Encode as E
import Registry2.Api.Models.Organization as Organization exposing (Organization)
import Registry2.Api.Requests as Requests
import Registry2.Data.AppState exposing (AppState)
import Registry2.Data.Forms.SignupForm as SignupForm exposing (SignupForm)
import Shared.Api exposing (ToMsg)


getOrganization : AppState -> String -> ToMsg Organization msg -> Cmd msg
getOrganization appState organizationId =
    Requests.get appState ("/organizations/" ++ organizationId) Organization.decoder


postOrganization : AppState -> SignupForm -> ToMsg () msg -> Cmd msg
postOrganization appState form =
    Requests.postWhatever appState "/organizations" (SignupForm.encode form)


putOrganizationState : AppState -> { organizationId : String, hash : String, active : Bool } -> ToMsg Organization msg -> Cmd msg
putOrganizationState appState { organizationId, hash, active } =
    let
        body =
            E.object [ ( "active", E.bool active ) ]
    in
    Requests.put appState ("/organizations/" ++ organizationId ++ "/state?hash=" ++ hash) Organization.decoder body
