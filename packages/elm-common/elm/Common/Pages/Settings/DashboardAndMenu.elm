module Common.Pages.Settings.DashboardAndMenu exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import Common.Api.Models.Settings.DashboardAndMenuSettings as DashboardAndMenuSettings exposing (DashboardAndMenuSettings)
import Common.Components.Form.Field as Field
import Common.Components.Markdown as Markdown
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.AnnouncementFields as AnnouncementFields
import Common.Pages.Settings.Components.FontAwesome as FontAwesome
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Separator as Separator


type alias Model =
    SettingsPage.Model DashboardAndMenuSettings DashboardAndMenuSettings


type alias Msg =
    SettingsPage.Msg DashboardAndMenuSettings


initialModel : Model
initialModel =
    SettingsPage.initialModel DashboardAndMenuSettings.initEmptyForm


init : AppState -> ( Model, Cmd Msg )
init appState =
    SettingsPage.init appState DashboardAndMenuSettings.section DashboardAndMenuSettings.initEmptyForm


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState =
    SettingsPage.update appState
        { wrapMsg = cfg.wrapMsg
        , logoutMsg = cfg.logoutMsg
        , section = DashboardAndMenuSettings.section
        , initForm = DashboardAndMenuSettings.initForm
        , formToValue = \form _ -> form
        , formValidation = DashboardAndMenuSettings.validation
        }


subscriptions : AppState -> Model -> Sub Msg
subscriptions =
    SettingsPage.subscriptions


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState =
    SettingsPage.view appState
        { title = gettext "Dashboard & Menu" appState.locale
        , description = Nothing
        , guideLink = Just (Section.guideLink appState GuideLinks.settingsDashboardAndLoginScreen)
        , formView = formView appState
        , wrapMsg = identity
        }


formView : AppState -> Form FormError DashboardAndMenuSettings -> Html Msg
formView appState form =
    Layout.stack
        [ Html.map SettingsPage.formMsg <|
            AnnouncementFields.view appState
                form
                { field = "announcements"
                , hint = gettext "Announcements are displayed on the dashboard after users log in." appState.locale
                }
        , Separator.line
        , Layout.stack
            [ customMenuLinks appState form
            , Markdown.view { headingOffset = 1 }
                (String.format
                    (gettext "Configure additional links in the menu. Choose any free icon from the [Font Awesome](%s), e.g. *fas fa-wand-magic*. Check *New window* if you want to open the link in a new window." appState.locale)
                    [ FontAwesome.link ]
                )
            ]
        ]


{-| `Field.listMapped`, because the rows carry the move buttons, which are the
section's messages and not the form's.
-}
customMenuLinks : AppState -> Form FormError DashboardAndMenuSettings -> Html Msg
customMenuLinks appState form =
    let
        listName =
            "customMenuLinks"

        fields =
            DashboardAndMenuSettings.customMenuLinkFields

        count =
            List.length (Form.getListIndexes listName form)

        path index =
            Field.listItemPath listName index

        field toField index name fieldLabel =
            Html.map SettingsPage.formMsg (Field.toHtml (toField appState form (path index name) fieldLabel))

        -- The icon drawn beside its name as it is typed; an empty or unknown
        -- one shows the question mark Icon.fromClass falls back to.
        iconField index =
            Field.input appState form (path index "icon") (gettext "Icon" appState.locale)
                |> Field.withSuffixIcon (Icon.fromClass (Maybe.withDefault "" (Form.getFieldAsString (path index "icon") form).value))
                |> Field.toHtml
                |> Html.map SettingsPage.formMsg

        -- The icon and the title side by side, and the URL on a line of its own:
        -- it is usually the longest of the three.
        toRow index =
            [ Layout.grid
                [ iconField index
                , field Field.input index "title" (gettext "Title" appState.locale)
                ]
            , field Field.input index "url" (gettext "URL" appState.locale)
            , field Field.toggle index "newWindow" (gettext "New window" appState.locale)
            ]

        -- Moving an item is the section's message, not the form's, which is why
        -- the list is `listMapped`; the buttons sit with the remove button.
        rowActions index =
            Maybe.values
                [ if index > 0 then
                    Just
                        (Button.iconOnly (Icon.solid "fa-arrow-up") (gettext "Move up" appState.locale) (SettingsPage.moveItemUp listName fields index)
                            |> Button.withTestId "button-move-up"
                        )

                  else
                    Nothing
                , if index < count - 1 then
                    Just
                        (Button.iconOnly (Icon.solid "fa-arrow-down") (gettext "Move down" appState.locale) (SettingsPage.moveItemDown listName fields index)
                            |> Button.withTestId "button-move-down"
                        )

                  else
                    Nothing
                ]
    in
    Field.listMapped appState
        form
        SettingsPage.formMsg
        { field = listName
        , label = gettext "Custom Menu Links" appState.locale
        , addLabel = gettext "Add link" appState.locale
        , rowLabel = \index -> Maybe.withDefault "" (Form.getFieldAsString (path index "title") form).value
        , row = toRow
        , rowActions = rowActions
        }


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model)
