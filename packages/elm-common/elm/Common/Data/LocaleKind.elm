module Common.Data.LocaleKind exposing
    ( LocaleKind(..)
    , apiPath
    , urlSegment
    )

{-| The two kinds of translation: of the user interface, and of the
mails. They are managed alike — a list, an upload, a registry import — over
their own endpoints.
-}


type LocaleKind
    = UiLocales
    | MailTemplateLocales


apiPath : LocaleKind -> String
apiPath kind =
    case kind of
        UiLocales ->
            "/locales"

        MailTemplateLocales ->
            "/mail-template-locales"


{-| The segment of the settings section's address.
-}
urlSegment : LocaleKind -> String
urlSegment kind =
    case kind of
        UiLocales ->
            "application-translations"

        MailTemplateLocales ->
            "email-translations"
