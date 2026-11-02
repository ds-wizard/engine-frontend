module Common.Data.Extensions exposing
    ( Extensions
    , LinkedEntity
    , LoginService
    , SettingsEntry
    , SettingsGroup(..)
    , none
    )

{-| Where a client adds its own entry points to the shared pages, as data.

A shared page draws what is here next to its own content and does nothing else
with it: an entry is a link to one of the client's own pages, and what happens
there is that client's business. A client with nothing to add leaves
everything empty (`none`).

  - **`settings`** — more sections in the settings navigation. Each one names its
    group and the section it follows (by that section's URL; `Nothing` puts it
    first in the group, a URL that is not there puts it last). The client
    renders its own sections inside the same layout with
    `Common.Pages.Settings.frame`.
  - **`loginServices`** — more ways to sign in on the login screen, after the
    OpenID providers. Choosing one keeps the address the visitor came from, as
    an OpenID login does, and leaves for `url`.
  - **`loginCaptcha`** — the login asks for a captcha before it sends the email
    and password (`Common.Ports.Captcha`). Only a client that answers the
    captcha ports may turn it on.
  - **`aiAssistant`** — the Features settings have the switch of the AI
    assistant. Only a client that has one turns it on.
  - **`openIdLink`** — something of the client's own an OpenID configuration
    may have one of, on its detail: a link to it when there is one, a button
    that creates it when there is not (`LinkedEntity`).
  - **`integrations`** — integrations the client has built in, offered in the
    knowledge model editor as types of their own and answered in the
    questionnaire from the API (`Common.Data.BuiltInIntegration`).

-}

import Common.Api.Models.AuthServiceProviderButtonStyle exposing (AuthServiceProviderButtonStyle)
import Common.Data.BuiltInIntegration exposing (BuiltInIntegration)
import Uuid exposing (Uuid)


type alias Extensions =
    { settings : List SettingsEntry
    , loginServices : List LoginService
    , loginCaptcha : Bool
    , aiAssistant : Bool
    , openIdLink : Maybe LinkedEntity
    , integrations : List BuiltInIntegration
    }


{-| The groups of the settings navigation.
-}
type SettingsGroup
    = UsersAndAccessSettings
    | ProjectsSettings
    | AppearanceSettings
    | FeaturesAndIntegrationsSettings
    | TranslationsSettings
    | PlanAndUsageSettings


type alias SettingsEntry =
    { group : SettingsGroup
    , after : Maybe String
    , label : String
    , url : String
    }


type alias LoginService =
    { uuid : Uuid
    , name : String
    , style : AuthServiceProviderButtonStyle
    , url : String
    }


{-| What a configuration's detail links to.

  - `field` is the field of the configuration's detail that holds the UUID of
    what it has, `null` or missing when it has none.
  - `createPath` is added to the configuration's address in the API; posting
    to it creates one, and the UUID of what was created comes back.
  - `url` is where the client shows the one with that UUID.
  - The rest are the words: the label of the field and its hint, the link, the
    button, and what is said when creating fails.

-}
type alias LinkedEntity =
    { field : String
    , createPath : String
    , url : Uuid -> String
    , label : String
    , hint : String
    , viewLabel : String
    , createLabel : String
    , createError : String
    }


none : Extensions
none =
    { settings = []
    , loginServices = []
    , loginCaptcha = False
    , aiAssistant = False
    , openIdLink = Nothing
    , integrations = []
    }
