module Common.Pages.DocumentTemplateEditors.Editor.Routes exposing
    ( DTEditorRoute(..)
    , DTSettingsSection(..)
    )

{-| The tabs of the document template editor, and the sections of its settings.

The settings are three sections beside a menu, as the application's settings
are, and each has a URL of its own: a section is somewhere you can be sent, and
the back button should walk back through them.

-}


type DTEditorRoute
    = Files
    | Preview
    | Settings DTSettingsSection


type DTSettingsSection
    = General
    | KnowledgeModels
    | Formats
