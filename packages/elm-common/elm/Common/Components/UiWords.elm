module Common.Components.UiWords exposing
    ( appShell
    , avatarGroup
    , breadcrumbs
    , commandPalette
    , dataGrid
    , drawer
    , field
    , fileUpload
    , form
    , listField
    , newTab
    , pagination
    , progressBar
    , resizer
    , sideNav
    , table
    )

{-| The words `Ui.*` says of its own accord, in the language of the page.

`Ui` knows no `Gettext`: every component that has something to say without
being told — a skip link, a pagination step, the name of a remove button — says
it in English and takes a `Words` record for anything else: every string is
passed in. This module is the one place those records are
filled in with gettext, so every composite, and every page that uses a `Ui`
component directly, says the same thing with the same msgids.

    Pagination.toHtmlWithWords (UiWords.pagination appState) config

    Table.table columns rows
        |> Table.withWords (UiWords.table appState)
        |> Table.toHtml

Where the wizard already had a msgid for the same words, it is reused, so the
translations carry over.

-}

import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext, ngettext)
import String.Format as String
import Ui.AppShell as AppShell
import Ui.AvatarGroup as AvatarGroup
import Ui.Breadcrumbs as Breadcrumbs
import Ui.CommandPalette as CommandPalette
import Ui.DataGrid as DataGrid
import Ui.Drawer as Drawer
import Ui.Field as Field
import Ui.FileUpload as FileUpload
import Ui.Form
import Ui.ListField as ListField
import Ui.Pagination as Pagination
import Ui.ProgressBar as ProgressBar
import Ui.Resizer as Resizer
import Ui.SideNav as SideNav
import Ui.Table as Table


{-| "(opens in a new tab)", for `Link.withExternalNotice`,
`Button.withExternalNotice`, `Menu.withExternalNotice` and
`SideNav.withExternalNotice`.
-}
newTab : WithLocale a -> String
newTab appState =
    gettext "(opens in a new tab)" appState.locale


pagination : WithLocale a -> Pagination.Words
pagination appState =
    { pagination = gettext "Pagination" appState.locale
    , first = gettext "First" appState.locale
    , previous = gettext "Prev" appState.locale
    , next = gettext "Next" appState.locale
    , last = gettext "Last" appState.locale
    }


appShell : WithLocale a -> AppShell.Words
appShell appState =
    { skipToContent = gettext "Skip to content" appState.locale
    , skipToNavigation = gettext "Skip to navigation" appState.locale
    }


drawer : WithLocale a -> Drawer.Words
drawer appState =
    { close = gettext "Close" appState.locale
    }


sideNav : WithLocale a -> SideNav.Words
sideNav appState =
    let
        separator =
            resizer appState (gettext "Resize navigation" appState.locale)
    in
    { collapse = gettext "Collapse sidebar" appState.locale
    , expand = gettext "Expand sidebar" appState.locale
    , resize = separator.label
    , collapsed = separator.collapsed
    , width = separator.width
    , newTab = newTab appState
    }


{-| A resize separator named `label`.
-}
resizer : WithLocale a -> String -> Resizer.Words
resizer appState label =
    { label = label
    , collapsed = gettext "Collapsed" appState.locale
    , width = \pixels -> String.format (gettext "%s pixels" appState.locale) [ String.fromInt pixels ]
    }


field : WithLocale a -> Field.Words
field appState =
    { show = \label -> String.format (gettext "Show %s" appState.locale) [ label ]
    , pick = \label -> String.format (gettext "Pick %s" appState.locale) [ label ]
    , clear = \label -> String.format (gettext "Clear %s" appState.locale) [ label ]
    , invalidDate = \value -> String.format (gettext "The stored value \"%s\" is not a valid date." appState.locale) [ value ]
    , checking = gettext "Checking…" appState.locale
    , strength =
        { veryWeak = gettext "Very weak" appState.locale
        , weak = gettext "Weak" appState.locale
        , reasonable = gettext "Reasonable" appState.locale
        , strong = gettext "Strong" appState.locale
        , veryStrong = gettext "Very strong" appState.locale
        }
    , markdown =
        { heading2 = gettext "Heading 2" appState.locale
        , heading3 = gettext "Heading 3" appState.locale
        , bold = gettext "Bold" appState.locale
        , italic = gettext "Italic" appState.locale
        , strikethrough = gettext "Strikethrough" appState.locale
        , link = gettext "Link" appState.locale
        , image = gettext "Image" appState.locale
        , bulletList = gettext "Bullet List" appState.locale
        , numberedList = gettext "Ordered List" appState.locale
        , inlineCode = gettext "Inline Code" appState.locale
        , codeBlock = gettext "Code Block" appState.locale
        , quote = gettext "Quote" appState.locale
        , linkAddress = gettext "Link address" appState.locale
        , imageAddress = gettext "Image address" appState.locale
        , apply = gettext "Apply" appState.locale
        , cancel = gettext "Cancel" appState.locale
        , editingMode = gettext "Editing mode" appState.locale
        , formatted = gettext "Rich Text" appState.locale
        , markdown = gettext "Markdown" appState.locale
        , toolbar = \label -> String.format (gettext "%s formatting" appState.locale) [ label ]
        }
    }


listField : WithLocale a -> ListField.Words
listField appState =
    { remove = \name -> String.format (gettext "Remove %s" appState.locale) [ name ]
    , row = \position -> String.format (gettext "Item %s" appState.locale) [ String.fromInt position ]
    }


progressBar : WithLocale a -> ProgressBar.Words
progressBar appState =
    { value =
        \value max ->
            String.format (gettext "%s of %s" appState.locale) [ String.fromFloat value, String.fromFloat max ]
    }


fileUpload : WithLocale a -> FileUpload.Words
fileUpload appState =
    { chosen =
        \name others ->
            if others > 0 then
                String.format (ngettext ( "%s and %s other file", "%s and %s other files" ) others appState.locale)
                    [ name, String.fromInt others ]

            else
                name
    , remove = \name -> String.format (gettext "Remove %s" appState.locale) [ name ]
    }


table : WithLocale a -> Table.Words
table appState =
    { sorted =
        \label ascending ->
            if ascending then
                String.format (gettext "Sorted by %s, ascending" appState.locale) [ label ]

            else
                String.format (gettext "Sorted by %s, descending" appState.locale) [ label ]
    }


dataGrid : WithLocale a -> DataGrid.Words
dataGrid appState =
    { sort = \label -> String.format (gettext "Sort by %s" appState.locale) [ label ]
    , sorted = (table appState).sorted
    , rowNumber = gettext "Row" appState.locale
    , filtered = gettext "filtered" appState.locale
    , resizeColumn = \label -> String.format (gettext "Resize column %s" appState.locale) [ label ]
    , width = \pixels -> String.format (gettext "%s pixels" appState.locale) [ String.fromInt pixels ]
    }


breadcrumbs : WithLocale a -> Breadcrumbs.Words
breadcrumbs appState =
    { breadcrumb = gettext "Breadcrumb" appState.locale }


avatarGroup : WithLocale a -> AvatarGroup.Words
avatarGroup appState =
    { more = \count -> String.format (ngettext ( "%s more", "%s more" ) count appState.locale) [ String.fromInt count ] }


form : WithLocale a -> Ui.Form.Words
form appState =
    { errorSummaryTitle = gettext "This form could not be saved" appState.locale }


{-| What the command palette says of its own accord.
-}
commandPalette : WithLocale a -> CommandPalette.Words
commandPalette appState =
    { title = gettext "Go to" appState.locale
    , placeholder = gettext "Search pages..." appState.locale
    , empty = gettext "No pages matching your search were found." appState.locale
    , loading = gettext "Searching..." appState.locale
    }
