module Common.Data.BuiltInIntegration exposing
    ( BuiltInIntegration
    , FieldsContext
    , RequestContext
    , find
    , findByKey
    , key
    , requestContext
    )

{-| An integration a client has built in (`Common.Data.Extensions.integrations`).

It is stored as a plugin integration — a plugin's uuid, the id of the
integration it offers, and settings as JSON — so the knowledge model is the
same for every client, and a client without it sees a plugin it does not have.
The shared pages show it as a type of integration of its own instead, and what
the settings mean is the client's alone, as it is a plugin's:

  - **`load`** — what the editor's fields need, asked for when the editor
    opens and kept as it came.
  - **`fields`** — the fields of the knowledge model editor after the name,
    which send the new settings. A field with `fieldId "plugin"` is the one a
    warning puts the caret in.
  - **`warning`** — what is not finished in the settings, if anything.
  - **`search`** — the hints for a search term in the questionnaire, or
    `Nothing` when there is nothing to search. They are answered as an API
    integration's are, with no custom reply, and an empty search is a search.

-}

import ActionResult exposing (ActionResult)
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Common.Api.Models.TypeHint exposing (TypeHint)
import Common.Api.Params exposing (RequestParams)
import Common.Api.Request exposing (ServerInfo)
import Gettext
import Html exposing (Html)
import Json.Encode as E
import Task exposing (Task)
import Uuid exposing (Uuid)


type alias BuiltInIntegration =
    { pluginUuid : Uuid
    , integrationId : String
    , name : String
    , load : RequestContext -> Task ApiError E.Value
    , fields : FieldsContext -> List (Html String)
    , warning : String -> Maybe String
    , search : RequestContext -> { settings : String, q : String } -> Maybe (Task ApiError (List TypeHint))
    }


{-| Where the fields are drawn: the settings, what `load` answered, the
workspace of the editor and the configuration in force, which says what the
reader may do.
-}
type alias FieldsContext =
    { locale : Gettext.Locale
    , fieldId : String -> String
    , settings : String
    , loaded : ActionResult E.Value
    , workspaceUuid : Maybe Uuid
    , config : BootstrapConfig
    }


{-| What a request needs: the server, the parameters the client adds to its
requests, and the workspace it is made in.
-}
type alias RequestContext =
    { serverInfo : ServerInfo
    , requestParams : RequestParams
    , workspaceUuid : Maybe Uuid
    }


requestContext : Context a -> Maybe Uuid -> RequestContext
requestContext context workspaceUuid =
    { serverInfo = Context.toServerInfo context
    , requestParams = context.requestParams
    , workspaceUuid = workspaceUuid
    }


{-| The one a plugin integration is, if it is one.
-}
find : List BuiltInIntegration -> { a | pluginUuid : String, pluginIntegrationId : String } -> Maybe BuiltInIntegration
find builtIns data =
    findByKey builtIns (data.pluginUuid ++ "/" ++ data.pluginIntegrationId)


findByKey : List BuiltInIntegration -> String -> Maybe BuiltInIntegration
findByKey builtIns value =
    List.filter ((==) value << key) builtIns
        |> List.head


{-| The plugin and the integration in one string, as a choice in a select.
-}
key : BuiltInIntegration -> String
key builtIn =
    Uuid.toString builtIn.pluginUuid ++ "/" ++ builtIn.integrationId
