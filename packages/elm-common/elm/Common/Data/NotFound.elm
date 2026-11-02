module Common.Data.NotFound exposing
    ( Context
    , NotFound
    , back
    , document
    , documentTemplate
    , documentTemplateEditor
    , file
    , knowledgeModel
    , knowledgeModelEditor
    , knowledgeModelMigration
    , openIdConfig
    , persistentCommand
    , project
    , resourcePage
    , role
    , tenant
    , user
    )

{-| What to say when the thing an address names is not there.

One place for all of it, because the answer has two halves that belong
together: the sentence naming what was looked for, and where to go instead. A
page that fetches a project and gets a 404 hands one of these to the
application, which draws the Not Found page from it.

**The way out is the list the thing would have been in.** "Go to projects" from
a missing project is a step towards finding it — the reader mistyped an address,
or followed a link to something since deleted, and the list is where the rest of
them are. The dashboard is the fallback for what has no list of its own, and for
a reader who may not see the one it has (`Common.Routes.isAllowed`, applied where
the page is drawn).

-}

import Common.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Common.Data.Session exposing (Session)
import Common.Routes as Routes
import Gettext exposing (gettext)


type alias NotFound =
    { message : String
    , back : Maybe { label : String, route : Routes.Route }
    }


type alias Context a =
    { a | locale : Gettext.Locale, session : Session, config : BootstrapConfig, toUrl : Routes.Route -> String }


{-| Where the Not Found page's button goes: the list, when there is one and this
reader may see it, and the dashboard otherwise.

The permission check has no case today — every list here is open to anyone who
could have reached the thing in it, so the fallback is reached only by an entry
with no list at all. It is kept because the day those diverge, the alternative
is offering a reader a page they will be turned away from.

-}
back : Context a -> NotFound -> { label : String, url : String }
back appState value =
    let
        dashboard =
            { label = gettext "Go to dashboard" appState.locale
            , url = Routes.toUrl appState Routes.appHome
            }
    in
    case value.back of
        Just target ->
            if Routes.isAllowed target.route appState then
                { label = target.label, url = Routes.toUrl appState target.route }

            else
                dashboard

        Nothing ->
            dashboard


project : Context a -> NotFound
project appState =
    { message = gettext "The project you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to projects" appState.locale, route = Routes.projectsIndex appState }
    }


knowledgeModel : Context a -> NotFound
knowledgeModel appState =
    { message = gettext "The knowledge model you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to knowledge models" appState.locale, route = Routes.knowledgeModelsIndex }
    }


{-| A page of a knowledge model's documentation. It has no list of its own; the
knowledge models are the nearest thing to one.
-}
resourcePage : Context a -> NotFound
resourcePage appState =
    { message = gettext "The resource page you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to knowledge models" appState.locale, route = Routes.knowledgeModelsIndex }
    }


knowledgeModelEditor : Context a -> NotFound
knowledgeModelEditor appState =
    { message = gettext "The knowledge model editor you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to knowledge model editors" appState.locale, route = Routes.kmEditorIndex }
    }


knowledgeModelMigration : Context a -> NotFound
knowledgeModelMigration appState =
    { message = gettext "The knowledge model migration you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to knowledge model editors" appState.locale, route = Routes.kmEditorIndex }
    }


documentTemplate : Context a -> NotFound
documentTemplate appState =
    { message = gettext "The document template you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to document templates" appState.locale, route = Routes.documentTemplatesIndex }
    }


documentTemplateEditor : Context a -> NotFound
documentTemplateEditor appState =
    { message = gettext "The document template editor you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to document template editors" appState.locale, route = Routes.documentTemplateEditorsIndex }
    }


document : Context a -> NotFound
document appState =
    { message = gettext "The document you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to documents" appState.locale, route = Routes.documentsIndex }
    }


file : Context a -> NotFound
file appState =
    { message = gettext "The file you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to project files" appState.locale, route = Routes.projectFilesIndex }
    }


user : Context a -> NotFound
user appState =
    { message = gettext "The user you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to users" appState.locale, route = Routes.usersIndex }
    }


tenant : Context a -> NotFound
tenant appState =
    { message = gettext "The tenant you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to tenants" appState.locale, route = Routes.tenantsIndex }
    }


role : Context a -> NotFound
role appState =
    { message = gettext "The role you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to roles" appState.locale, route = Routes.settingsRoles }
    }


openIdConfig : Context a -> NotFound
openIdConfig appState =
    { message = gettext "The OpenID configuration you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to OpenID settings" appState.locale, route = Routes.settingsOpenId }
    }


persistentCommand : Context a -> NotFound
persistentCommand appState =
    { message = gettext "The persistent command you are looking for was not found." appState.locale
    , back = Just { label = gettext "Go to persistent commands" appState.locale, route = Routes.persistentCommandsIndex }
    }
