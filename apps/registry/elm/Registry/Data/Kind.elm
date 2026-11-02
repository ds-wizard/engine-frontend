module Registry.Data.Kind exposing
    ( all
    , count
    , description
    , icon
    , idLabel
    , isTranslation
    , nothingPublished
    , title
    , unableToGet
    )

{-| The words and the icon of each kind of thing the registry publishes. The
msgids are the client's where they say the same.
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
            gettext "Locales" appState.locale

        MailTemplateLocale ->
            gettext "Mail Translations" appState.locale


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
            gettext "Unable to get locales." appState.locale

        MailTemplateLocale ->
            gettext "Unable to get mail translations." appState.locale


nothingPublished : AppState -> Kind -> String
nothingPublished appState kind =
    case kind of
        KnowledgeModel ->
            gettext "No knowledge models are published yet." appState.locale

        DocumentTemplate ->
            gettext "No document templates are published yet." appState.locale

        Locale ->
            gettext "No locales are published yet." appState.locale

        MailTemplateLocale ->
            gettext "No mail translations are published yet." appState.locale


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
                    ( "%s locale", "%s locales" )

                MailTemplateLocale ->
                    ( "%s mail translation", "%s mail translations" )
    in
    String.format (ngettext msgids n appState.locale) [ String.fromInt n ]
