module Common.Pages.Settings.Components.AnnouncementFields exposing (view)

{-| The list of announcements of a settings section: each has a level and a
text.
-}

import Common.Api.Models.Announcement.AnnouncementLevel as AnnouncementLevel
import Common.Components.Form.Field as Field
import Common.Data.AppState exposing (AppState)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Field as UiField


view : AppState -> Form FormError a -> { field : String, hint : String } -> Html Form.Msg
view appState form config =
    Field.listWithHint config.hint
        appState
        form
        { field = config.field
        , label = gettext "Announcements" appState.locale
        , addLabel = gettext "Add announcement" appState.locale
        , rowLabel = always ""
        , row = row appState form config.field
        }


row : AppState -> Form FormError a -> String -> Int -> List (Html Form.Msg)
row appState form field index =
    let
        path =
            Field.listItemPath field index

        -- Each level in the colors of the alert it shows, as the wizard had them.
        levelOptions =
            [ UiField.choice { value = AnnouncementLevel.toString AnnouncementLevel.Info, label = gettext "Info" appState.locale }
                |> UiField.withChoiceTone UiField.InfoTone
            , UiField.choice { value = AnnouncementLevel.toString AnnouncementLevel.Warning, label = gettext "Warning" appState.locale }
                |> UiField.withChoiceTone UiField.WarningTone
            , UiField.choice { value = AnnouncementLevel.toString AnnouncementLevel.Critical, label = gettext "Critical" appState.locale }
                |> UiField.withChoiceTone UiField.DangerTone
            ]
    in
    [ Field.radioGroup appState form (path "level") (gettext "Level" appState.locale) levelOptions
        |> Field.withChoicesInline
        |> Field.toHtml
    , Field.markdown appState form (path "content") (gettext "Content" appState.locale)
        |> Field.toHtml
    ]
