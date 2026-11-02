module Common.Data.UpdateConfig exposing (UpdateConfig, entityWorkspace, map, notAllowed, notFound)

{-| What a page may ask of the application around it.

Pages never import the application's `Msg`. Everything they need to trigger
outside themselves — logging out, handing over a token after login, opening the
news, reloading the configuration after a settings change — arrives here, the
same record for every page, so adding a page never changes the application's
wiring.

-}

import Common.Api.Models.Token exposing (Token)
import Common.Data.NotFound exposing (NotFound)
import Task.Extra as Task
import Uuid exposing (Uuid)


type alias UpdateConfig pageMsg msg =
    { wrapMsg : pageMsg -> msg
    , logoutMsg : msg
    , logoutToLoginMsg : Maybe String -> msg
    , gotTokenMsg : Token -> Maybe String -> msg
    , openNewsModalMsg : msg
    , reloadConfigMsg : msg
    , setNotFoundMsg : NotFound -> msg
    , setNotAllowedMsg : msg
    , entityWorkspaceMsg : Maybe Uuid -> msg
    }


{-| The thing this page is about is not there — the server answered 404 for the
one entity the whole page is showing.

It goes to the application rather than staying in the page: what should be seen
is the application's Not Found page, and every page that can be asked for a
thing that does not exist would otherwise carry its own copy of one. The address
is left alone, so a reload still asks the server and a corrected link still
works.

What to say, and where to go instead, come from `Common.Data.NotFound` — the page
names the kind of thing it was asked for and nothing else.

-}
notFound : UpdateConfig pageMsg msg -> NotFound -> Cmd msg
notFound cfg value =
    Task.dispatch (cfg.setNotFoundMsg value)


{-| The thing this page is about is there, but not for this reader — the server
answered 403 for the one entity the whole page is showing, such as a knowledge
model that is not public opened by somebody signed out.

Like `notFound`, it goes to the application, which shows its Not Allowed page in
place of the route and leaves the address alone.

-}
notAllowed : UpdateConfig pageMsg msg -> Cmd msg
notAllowed cfg =
    Task.dispatch cfg.setNotAllowedMsg


{-| For a router handing its config down to a sub-page.
-}
map : (subMsg -> pageMsg) -> UpdateConfig pageMsg msg -> UpdateConfig subMsg msg
map toPageMsg cfg =
    { wrapMsg = cfg.wrapMsg << toPageMsg
    , logoutMsg = cfg.logoutMsg
    , logoutToLoginMsg = cfg.logoutToLoginMsg
    , gotTokenMsg = cfg.gotTokenMsg
    , openNewsModalMsg = cfg.openNewsModalMsg
    , reloadConfigMsg = cfg.reloadConfigMsg
    , setNotFoundMsg = cfg.setNotFoundMsg
    , setNotAllowedMsg = cfg.setNotAllowedMsg
    , entityWorkspaceMsg = cfg.entityWorkspaceMsg
    }


{-| The one entity this page is about belongs to this workspace (`Nothing`: the
tenant). A client with workspaces in its addresses takes the reader to the
right one when the address names another — a project of B asked for in A
loads, because a detail does not care which workspace it was asked in, but it
is shown in B. A client without them ignores it.
-}
entityWorkspace : UpdateConfig pageMsg msg -> Maybe Uuid -> Cmd msg
entityWorkspace cfg mbWorkspaceUuid =
    Task.dispatch (cfg.entityWorkspaceMsg mbWorkspaceUuid)
