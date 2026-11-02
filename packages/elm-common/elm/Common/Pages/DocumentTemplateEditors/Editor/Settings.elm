module Common.Pages.DocumentTemplateEditors.Editor.Settings exposing
    ( Config
    , encode
    , formats
    , general
    , knowledgeModels
    , newFormat
    )

{-| The Settings tab: what the template is called, which knowledge models it
works with, and the formats it can be rendered in.

**The draft is the form.** There is no separate form model: a field edits the
draft that is already in memory and marks it unsaved, and Save sends the whole
draft. It is the same unit of work as the files — one button, one state, one
guard — so a form state machine of its own would be a
second thing to keep in step.

Formats and their steps are lists that can be added to and taken from, in the
order they were made. They do not reorder: `Ui.ListField` has no move, and the
order of formats is not something the server reads.

-}

import Common.Api.Models.DocumentTemplate.DocumentTemplateAllowedPackage as DocumentTemplateAllowedPackage exposing (DocumentTemplateAllowedPackage)
import Common.Api.Models.DocumentTemplate.DocumentTemplateFormatStep exposing (DocumentTemplateFormatStep)
import Common.Api.Models.DocumentTemplateDraft.DocumentTemplateFormatDraft as DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Common.Api.Models.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Encode as E
import LanguageCodes
import Ui.Field as Field
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.ListField as ListField
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias Config msg =
    { locale : Gettext.Locale
    , draft : DocumentTemplateDraftDetail
    , onChange : DocumentTemplateDraftDetail -> msg

    -- A format or a step somebody can start from rather than fill in.
    , formatPrefabs : List DocumentTemplateFormatDraft

    -- A new format needs a uuid, and only the shell can make one.
    , onAddFormat : msg
    }


general : Config msg -> Html msg
general config =
    let
        draft =
            config.draft

        field id label value set =
            Field.toHtml
                (Field.input
                    { id = "dt-settings-" ++ id
                    , label = label
                    , value = value
                    , onInput = \next -> config.onChange (set next)
                    }
                )
    in
    Layout.stack
        [ field "name" (gettext "Name" config.locale) draft.name (\value -> { draft | name = value })
        , field "description" (gettext "Description" config.locale) draft.description (\value -> { draft | description = value })
        , Field.toHtml
            (Field.input
                { id = "dt-settings-templateId"
                , label = gettext "Document Template ID" config.locale
                , value = draft.id
                , onInput = \value -> config.onChange { draft | id = value }
                }
                |> Field.withHint (gettext "Document template ID can only contain alphanumeric characters, hyphens, underscores, and dots." config.locale)
            )
        , Field.groupToHtml
            (Field.group
                { id = "dt-settings-version"
                , legend = gettext "Version" config.locale
                , fields =
                    [ versionPart config "major" (gettext "Major" config.locale) Version.getMajor (\value -> Version.create value (Version.getMinor draft.version) (Version.getPatch draft.version))
                    , versionPart config "minor" (gettext "Minor" config.locale) Version.getMinor (\value -> Version.create (Version.getMajor draft.version) value (Version.getPatch draft.version))
                    , versionPart config "patch" (gettext "Patch" config.locale) Version.getPatch (\value -> Version.create (Version.getMajor draft.version) (Version.getMinor draft.version) value)
                    ]
                }
                |> Field.withGroupHint (gettext "The version number is in format X.Y.Z. Increasing number Z indicates only some fixes, number Y minor changes, and number X indicates a major change." config.locale)
            )

        -- The language the template is written in, as a code. The names
        -- are what anybody knows it by, so the list says both.
        , Field.toHtml
            (Field.select
                { id = "dt-settings-language"
                , label = gettext "Language" config.locale
                , value = draft.language
                , options = LanguageCodes.options
                , onInput = \value -> config.onChange { draft | language = value }
                }
            )
        , field "license" (gettext "License" config.locale) draft.license (\value -> { draft | license = value })
        , Field.toHtml
            (Field.markdown
                { id = "dt-settings-readme"
                , label = gettext "Readme" config.locale
                , value = draft.readme
                , onChange = \value -> config.onChange { draft | readme = value }
                }
            )
        ]


{-| One of the three numbers a version is made of.

Each one is edited on its own and put back together into a `Version`, because
that is what the draft holds; a number that cannot be read (an emptied field)
is a zero, and so is a negative one, so the three always make a version the
Save can send.

-}
versionPart : Config msg -> String -> String -> (Version -> Int) -> (Int -> Version) -> Html msg
versionPart config name label part set =
    let
        draft =
            config.draft
    in
    Field.toHtml
        (Field.input
            { id = "dt-settings-version-" ++ name
            , label = label
            , value = String.fromInt (part draft.version)
            , onInput = \next -> config.onChange { draft | version = set (max 0 (Maybe.withDefault 0 (String.toInt next))) }
            }
            |> Field.withInputMode "numeric"
            |> Field.withRequired True
            |> Field.withPlaceholder "0"
        )



-- WHICH KNOWLEDGE MODELS IT WORKS WITH


knowledgeModels : Config msg -> Html msg
knowledgeModels config =
    let
        draft =
            config.draft

        change packages =
            config.onChange { draft | allowedPackages = packages }

        row index package =
            ListField.row
                { label = packageLabel config index package
                , content =
                    [ maybeField ("allowed-km-" ++ String.fromInt index) (gettext "Knowledge Model ID" config.locale) package.id (\value -> change (setAt index { package | id = value } draft.allowedPackages))
                    , maybeField ("allowed-min-" ++ String.fromInt index) (gettext "Min Version" config.locale) package.minVersion (\value -> change (setAt index { package | minVersion = value } draft.allowedPackages))
                    , maybeField ("allowed-max-" ++ String.fromInt index) (gettext "Max Version" config.locale) package.maxVersion (\value -> change (setAt index { package | maxVersion = value } draft.allowedPackages))
                    ]
                }
                -- Positional, as its fields' ids are: the rows have no key of their own.
                |> ListField.withRowTestId ("dt-settings-allowed-packages_row_" ++ String.fromInt index)
    in
    Layout.stack
        [ ListField.toHtml
            { id = "dt-settings-allowed-packages"
            , label = gettext "Allowed Knowledge Models" config.locale
            , rows = List.indexedMap row draft.allowedPackages
            , addLabel = gettext "Add knowledge model" config.locale
            , onAdd = change (draft.allowedPackages ++ [ { id = Nothing, minVersion = Nothing, maxVersion = Nothing } ])
            , onRemove = Just (\index -> change (removeAt index draft.allowedPackages))
            , hint = Nothing
            , error = Nothing
            }
        ]


packageLabel : Config msg -> Int -> DocumentTemplateAllowedPackage -> String
packageLabel config index package =
    case package.id of
        Just id ->
            id

        Nothing ->
            String.replace "%s" (String.fromInt (index + 1)) (gettext "Knowledge model %s" config.locale)



-- THE FORMATS IT RENDERS IN


formats : Config msg -> Html msg
formats config =
    let
        draft =
            config.draft

        change list =
            config.onChange { draft | formats = list }

        row index format =
            ListField.row
                { label =
                    if String.isEmpty format.name then
                        String.replace "%s" (String.fromInt (index + 1)) (gettext "Format %s" config.locale)

                    else
                        format.name
                , content =
                    -- The icon and the name on one line where there is room for
                    -- two, the icon first: it is the narrower of the pair and it
                    -- is what the format is recognized by in a menu. The icon is
                    -- drawn beside its own name as it is typed, as the custom
                    -- menu links are; an empty or unknown one shows the question
                    -- mark `Icon.fromClass` falls back to.
                    [ Layout.grid
                        [ Field.toHtml
                            (Field.input
                                { id = "dt-settings-format-icon-" ++ String.fromInt index
                                , label = gettext "Icon" config.locale
                                , value = format.icon
                                , onInput = \value -> change (setAt index { format | icon = value } draft.formats)
                                }
                                |> Field.withSuffixIcon (Icon.fromClass format.icon)
                            )
                        , Field.toHtml
                            (Field.input
                                { id = "dt-settings-format-name-" ++ String.fromInt index
                                , label = gettext "Name" config.locale
                                , value = format.name
                                , onInput = \value -> change (setAt index { format | name = value } draft.formats)
                                }
                            )
                        ]
                    , steps config index format (\next -> change (setAt index next draft.formats))
                    ]
                }
                -- Positional, as its fields' ids are: the rows have no key of their own.
                |> ListField.withRowTestId ("dt-settings-format-list_row_" ++ String.fromInt index)
    in
    Layout.stack
        [ ListField.toHtml
            { id = "dt-settings-format-list"
            , label = gettext "Formats" config.locale
            , rows = List.indexedMap row draft.formats
            , addLabel = gettext "Add format" config.locale
            , onAdd = config.onAddFormat
            , onRemove = Just (\index -> change (removeAt index draft.formats))
            , hint = Nothing
            , error = Nothing
            }
        ]


{-| A new format starts from the first prefab where there is one, because a
format nobody has filled in renders nothing. The uuid is the shell's to make.
-}
newFormat : List DocumentTemplateFormatDraft -> Uuid -> DocumentTemplateFormatDraft
newFormat prefabs uuid =
    case List.head prefabs of
        Just prefab ->
            { prefab | uuid = uuid }

        Nothing ->
            { uuid = uuid, name = "", icon = "", steps = [] }


steps : Config msg -> Int -> DocumentTemplateFormatDraft -> (DocumentTemplateFormatDraft -> msg) -> Html msg
steps config formatIndex format onChange =
    let
        row index step =
            ListField.row
                { label =
                    if String.isEmpty step.name then
                        String.replace "%s" (String.fromInt (index + 1)) (gettext "Step %s" config.locale)

                    else
                        step.name
                , content =
                    [ Field.toHtml
                        (Field.input
                            { id = "dt-settings-step-" ++ String.fromInt formatIndex ++ "-" ++ String.fromInt index
                            , label = gettext "Name" config.locale
                            , value = step.name
                            , onInput = \value -> onChange { format | steps = setAt index { step | name = value } format.steps }
                            }
                        )
                    , options config
                        formatIndex
                        index
                        step
                        (\next -> onChange { format | steps = setAt index next format.steps })
                    ]
                }
                -- Positional, as its fields' ids are: the rows have no key of their own.
                |> ListField.withRowTestId (("dt-settings-steps-" ++ String.fromInt formatIndex) ++ "_row_" ++ String.fromInt index)
    in
    ListField.toHtml
        { id = "dt-settings-steps-" ++ String.fromInt formatIndex
        , label = gettext "Steps" config.locale
        , rows = List.indexedMap row format.steps
        , addLabel = gettext "Add step" config.locale
        , onAdd = onChange { format | steps = format.steps ++ [ { name = "", options = [] } ] }
        , onRemove = Just (\index -> onChange { format | steps = removeAt index format.steps })
        , hint = Nothing
        , error = Nothing
        }


{-| What a step is given to work with: named values, a pair to a row.

A step without them does nothing useful — the jinja step is told which template
to render — so they are edited here rather than left to whatever filled the
format in.

-}
options : Config msg -> Int -> Int -> DocumentTemplateFormatStep -> (DocumentTemplateFormatStep -> msg) -> Html msg
options config formatIndex stepIndex step onChange =
    let
        id part index =
            String.join "-"
                [ "dt-settings-step-option"
                , String.fromInt formatIndex
                , String.fromInt stepIndex
                , String.fromInt index
                , part
                ]

        row index ( key, value ) =
            ListField.row
                { label =
                    if String.isEmpty key then
                        String.replace "%s" (String.fromInt (index + 1)) (gettext "Option %s" config.locale)

                    else
                        key
                , content =
                    -- A name and its value are one fact, so they sit on one
                    -- line where the row is wide enough for two and stack where
                    -- it is not (`Ui.Layout.grid`).
                    [ div [ class "c-dt-settings-option" ]
                        [ Layout.grid
                            [ Field.toHtml
                                (Field.input
                                    { id = id "name" index
                                    , label = gettext "Name" config.locale
                                    , value = key
                                    , onInput = \next -> onChange { step | options = setAt index ( next, value ) step.options }
                                    }
                                )
                            , Field.toHtml
                                (Field.input
                                    { id = id "value" index
                                    , label = gettext "Value" config.locale
                                    , value = value
                                    , onInput = \next -> onChange { step | options = setAt index ( key, next ) step.options }
                                    }
                                )
                            ]
                        ]
                    ]
                }
                -- Positional, as its fields' ids are: the rows have no key of their own.
                |> ListField.withRowTestId (("dt-settings-step-options-" ++ String.fromInt formatIndex ++ "-" ++ String.fromInt stepIndex) ++ "_row_" ++ String.fromInt index)
    in
    ListField.toHtml
        { id = "dt-settings-step-options-" ++ String.fromInt formatIndex ++ "-" ++ String.fromInt stepIndex
        , label = gettext "Options" config.locale
        , rows = List.indexedMap row step.options
        , addLabel = gettext "Add option" config.locale
        , onAdd = onChange { step | options = step.options ++ [ ( "", "" ) ] }
        , onRemove = Just (\index -> onChange { step | options = removeAt index step.options })
        , hint = Nothing
        , error = Nothing
        }



-- SENDING IT


{-| The whole draft, as the `PUT` wants it. The phase is what tells a save from
a publish: the same endpoint with `Released` is how a template is published.
-}
encode : String -> DocumentTemplateDraftDetail -> E.Value
encode phase draft =
    E.object
        [ ( "name", E.string draft.name )
        , ( "description", E.string draft.description )
        , ( "language", E.string draft.language )
        , ( "license", E.string draft.license )
        , ( "readme", E.string draft.readme )
        , ( "allowedPackages", E.list DocumentTemplateAllowedPackage.encode draft.allowedPackages )
        , ( "formats", E.list DocumentTemplateFormatDraft.encode draft.formats )
        , ( "id", E.string draft.id )
        , ( "version", E.string (Version.toString draft.version) )
        , ( "phase", E.string phase )
        ]



-- ODDS AND ENDS


maybeField : String -> String -> Maybe String -> (Maybe String -> msg) -> Html msg
maybeField id label value onChange =
    Field.toHtml
        (Field.input
            { id = "dt-settings-" ++ id
            , label = label
            , value = Maybe.withDefault "" value
            , onInput =
                \next ->
                    onChange
                        (if String.isEmpty next then
                            Nothing

                         else
                            Just next
                        )
            }
        )


setAt : Int -> a -> List a -> List a
setAt index value =
    List.indexedMap
        (\i item ->
            if i == index then
                value

            else
                item
        )


removeAt : Int -> List a -> List a
removeAt index list =
    List.take index list ++ List.drop (index + 1) list
