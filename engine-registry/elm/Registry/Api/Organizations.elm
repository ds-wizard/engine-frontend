module Registry.Api.Organizations exposing
    ( getOrganization
    , postOrganization
    , putOrganization
    , putOrganizationState
    , putOrganizationToken
    )

import Json.Encode as E
import Registry.Api.Models.Organization as Organization exposing (Organization)
import Registry.Api.Requests as Requests
import Registry.Data.AppState exposing (AppState)
import Registry.Data.Forms.OrganizationForm as OrganizationForm exposing (OrganizationForm)
import Registry.Data.Forms.SignupForm as SignupForm exposing (SignupForm)
import Shared.Api exposing (ToMsg)


getOrganization : AppState -> String -> ToMsg Organization msg -> Cmd msg
getOrganization appState organizationId =
    Requests.get appState ("/organizations/" ++ organizationId) Organization.decoder


postOrganization : AppState -> SignupForm -> ToMsg () msg -> Cmd msg
postOrganization appState form =
    Requests.postWhatever appState "/organizations" (SignupForm.encode form)


putOrganization : AppState -> String -> OrganizationForm -> ToMsg Organization msg -> Cmd msg
putOrganization appState organizationId formData =
    let
        body =
            OrganizationForm.encode formData
    in
    Requests.put appState ("/organizations/" ++ organizationId) Organization.decoder body


putOrganizationState : AppState -> { organizationId : String, hash : String, active : Bool } -> ToMsg Organization msg -> Cmd msg
putOrganizationState appState { organizationId, hash, active } =
    let
        body =
            E.object [ ( "active", E.bool active ) ]
    in
    Requests.put appState ("/organizations/" ++ organizationId ++ "/state?hash=" ++ hash) Organization.decoder body


putOrganizationToken : AppState -> { organizationId : String, hash : String } -> ToMsg Organization msg -> Cmd msg
putOrganizationToken appState { organizationId, hash } =
    Requests.putEmpty appState ("/organizations/" ++ organizationId ++ "/token?hash=" ++ hash) Organization.decoder
