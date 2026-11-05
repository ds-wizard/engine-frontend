module Registry.Data.Kind exposing
    ( Section
    , count
    , description
    , idLabel
    , isTranslation
    , nothingPublished
    , sections
    , tabTitle
    , title
    , translations
    , translationsDescription
    , translationsTitle
    , unableToGet
    )

{-| The words and the icon of each kind of thing the registry publishes. The
msgids are the client's where they say the same.

The two kinds of translations are one place, Translations, with Application and
Emails in it, as in a wizard's settings.

-}

import Gettext exposing (gettext, ngettext)
import Registry.Api.Models.Item exposing (Kind(..))
import Registry.Data.AppState exposing (AppState)
import String.Format as String
import Ui.Icon as Icon exposing (Icon)


{-| The kinds this registry offers: translations only where it says it has
them.
-}
all : AppState -> List Kind
all appState =
    [ KnowledgeModel, DocumentTemplate ]
        ++ (if appState.config.localesEnabled then
                [ Locale, MailTemplateLocale ]

            else
                []
           )


{-| The kinds of translations this registry offers, in the order of their tabs.
-}
translations : AppState -> List Kind
translations appState =
    List.filter isTranslation (all appState)


{-| The registry's places, on the home page and in the navigation: knowledge
models, document templates and, where the registry has them, translations,
which open on their first kind. `kinds` are what a place holds, so the
navigation knows where a page belongs.
-}
type alias Section =
    { title : String
    , description : String
    , icon : Icon
    , kind : Kind
    , kinds : List Kind
    , testId : String
    }


sections : AppState -> List Section
sections appState =
    let
        one kind testId =
            { title = title appState kind
            , description = description appState kind
            , icon = icon kind
            , kind = kind
            , kinds = [ kind ]
            , testId = testId
            }
    in
    [ one KnowledgeModel "knowledge-models", one DocumentTemplate "document-templates" ]
        ++ (case translations appState of
                first :: _ ->
                    [ { title = translationsTitle appState
                      , description = translationsDescription appState
                      , icon = Icon.solid "fa-language"
                      , kind = first
                      , kinds = translations appState
                      , testId = "translations"
                      }
                    ]

                [] ->
                    []
           )


translationsTitle : AppState -> String
translationsTitle appState =
    gettext "Translations" appState.locale


translationsDescription : AppState -> String
translationsDescription appState =
    gettext "Adapt the user interface and the emails to different languages for a more inclusive experience." appState.locale


{-| Translations are a language and a version, with no page of their own.
-}
isTranslation : Kind -> Bool
isTranslation kind =
    case kind of
        Locale ->
            True

        MailTemplateLocale ->
            True

        _ ->
            False


title : AppState -> Kind -> String
title appState kind =
    case kind of
        KnowledgeModel ->
            gettext "Knowledge Models" appState.locale

        DocumentTemplate ->
            gettext "Document Templates" appState.locale

        Locale ->
            gettext "Application Translations" appState.locale

        MailTemplateLocale ->
            gettext "Email Translations" appState.locale


{-| A translation's tab under Translations.
-}
tabTitle : AppState -> Kind -> String
tabTitle appState kind =
    case kind of
        MailTemplateLocale ->
            gettext "Emails" appState.locale

        Locale ->
            gettext "Application" appState.locale

        _ ->
            title appState kind


description : AppState -> Kind -> String
description appState kind =
    case kind of
        KnowledgeModel ->
            gettext "Choose the appropriate Knowledge Model to shape the structure of your questionnaire." appState.locale

        DocumentTemplate ->
            gettext "Compose documents from questionnaires by selecting templates that handle the transformation of replies." appState.locale

        Locale ->
            gettext "Adapt the user interface to different languages for a more inclusive experience." appState.locale

        MailTemplateLocale ->
            gettext "Send the mails of the wizard in the languages of its users." appState.locale


icon : Kind -> Icon
icon kind =
    case kind of
        KnowledgeModel ->
            Icon.solid "fa-sitemap"

        DocumentTemplate ->
            Icon.solid "fa-file-code"

        Locale ->
            Icon.solid "fa-language"

        MailTemplateLocale ->
            Icon.solid "fa-envelope"


idLabel : AppState -> Kind -> String
idLabel appState kind =
    case kind of
        KnowledgeModel ->
            gettext "Knowledge Model ID" appState.locale

        DocumentTemplate ->
            gettext "Document Template ID" appState.locale

        Locale ->
            gettext "Language code" appState.locale

        MailTemplateLocale ->
            gettext "Language code" appState.locale


unableToGet : AppState -> Kind -> String
unableToGet appState kind =
    case kind of
        KnowledgeModel ->
            gettext "Unable to get knowledge models." appState.locale

        DocumentTemplate ->
            gettext "Unable to get document templates." appState.locale

        Locale ->
            gettext "Unable to get application translations." appState.locale

        MailTemplateLocale ->
            gettext "Unable to get email translations." appState.locale


nothingPublished : AppState -> Kind -> String
nothingPublished appState kind =
    case kind of
        KnowledgeModel ->
            gettext "No knowledge models are published yet." appState.locale

        DocumentTemplate ->
            gettext "No document templates are published yet." appState.locale

        Locale ->
            gettext "No application translations are published yet." appState.locale

        MailTemplateLocale ->
            gettext "No email translations are published yet." appState.locale


count : AppState -> Kind -> Int -> String
count appState kind n =
    let
        msgids =
            case kind of
                KnowledgeModel ->
                    ( "%s knowledge model", "%s knowledge models" )

                DocumentTemplate ->
                    ( "%s document template", "%s document templates" )

                Locale ->
                    ( "%s application translation", "%s application translations" )

                MailTemplateLocale ->
                    ( "%s email translation", "%s email translations" )
    in
    String.format (ngettext msgids n appState.locale) [ String.fromInt n ]
