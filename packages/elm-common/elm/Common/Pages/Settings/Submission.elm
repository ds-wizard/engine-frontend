module Common.Pages.Settings.Submission exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.DocumentTemplates as DocumentTemplatesApi
import Common.Api.Models.DocumentTemplateAllSuggestion as DocumentTemplateAllSuggestion exposing (DocumentTemplateAllSuggestion)
import Common.Api.Models.Settings.SubmissionSettings as SubmissionSettings exposing (SubmissionSettings)
import Common.Components.Form.Field as Field
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.HttpMethod as HttpMethod
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import Ui.Form as UiForm
import Ui.Layout as Layout
import Version


type alias Model =
    { templates : ActionResult (List DocumentTemplateAllSuggestion)
    , genericModel : SettingsPage.Model SubmissionSettings SubmissionSettings
    }


type Msg
    = GenericMsg (SettingsPage.Msg SubmissionSettings)
    | GetTemplatesCompleted (Result ApiError (List DocumentTemplateAllSuggestion))


initialModel : Model
initialModel =
    { templates = Loading
    , genericModel = SettingsPage.initialModel SubmissionSettings.initEmptyForm
    }


init : AppState -> ( Model, Cmd Msg )
init appState =
    let
        ( genericModel, genericCmd ) =
            SettingsPage.init appState SubmissionSettings.section SubmissionSettings.initEmptyForm
    in
    ( { initialModel | genericModel = genericModel }
    , Cmd.batch
        [ DocumentTemplatesApi.getTemplatesAll appState GetTemplatesCompleted
        , Cmd.map GenericMsg genericCmd
        ]
    )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GenericMsg genericMsg ->
            let
                ( genericModel, cmd ) =
                    SettingsPage.update appState
                        { wrapMsg = cfg.wrapMsg << GenericMsg
                        , logoutMsg = cfg.logoutMsg
                        , section = SubmissionSettings.section
                        , initForm = SubmissionSettings.initForm
                        , formToValue = \form _ -> form
                        , formValidation = SubmissionSettings.validation
                        }
                        genericMsg
                        model.genericModel
            in
            ( { model | genericModel = genericModel }, cmd )

        GetTemplatesCompleted result ->
            RequestHelpers.applyResult
                { setResult = \templates m -> { m | templates = templates }
                , defaultError = gettext "Unable to get document templates." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.map GenericMsg (SettingsPage.subscriptions appState model.genericModel)


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState model =
    let
        -- The formats are chosen from the document templates, so the form
        -- waits for them as well as for the configuration.
        genericModel_ =
            model.genericModel

        ( templates, genericModel ) =
            case ( model.templates, model.genericModel.config ) of
                ( Success loadedTemplates, _ ) ->
                    ( loadedTemplates, model.genericModel )

                ( Error error, _ ) ->
                    ( [], { genericModel_ | config = Error error } )

                ( _, Error _ ) ->
                    ( [], model.genericModel )

                _ ->
                    ( [], { genericModel_ | config = Loading } )
    in
    SettingsPage.view appState
        { title = gettext "Document Submission" appState.locale
        , description = Nothing
        , guideLink = Just (Section.guideLink appState GuideLinks.settingsDocumentSubmission)
        , formView = formView appState templates
        , wrapMsg = GenericMsg
        }
        genericModel


formView : AppState -> List DocumentTemplateAllSuggestion -> Form FormError SubmissionSettings -> Html (SettingsPage.Msg SubmissionSettings)
formView appState templates form =
    let
        enabled =
            Maybe.withDefault False (Form.getFieldAsBool "enabled" form).value

        servicesInput =
            if enabled then
                [ UiForm.nested
                    [ Field.list appState
                        form
                        { field = "services"
                        , label = gettext "Services" appState.locale
                        , addLabel = gettext "Add service" appState.locale
                        , rowLabel = \index -> stringValue form (Field.listItemPath "services" index "name")
                        , row = serviceFormView appState templates form
                        }
                    ]
                ]

            else
                []
    in
    Html.map SettingsPage.formMsg <|
        Layout.stack
            ((Field.toggle appState form "enabled" (gettext "Enabled" appState.locale)
                |> Field.withHint (gettext "If enabled, you can configure external services for uploading documents from the instance." appState.locale)
                |> Field.toHtml
             )
                :: servicesInput
            )


serviceFormView : AppState -> List DocumentTemplateAllSuggestion -> Form FormError SubmissionSettings -> Int -> List (Html Form.Msg)
serviceFormView appState templates form index =
    let
        field =
            Field.listItemPath "services" index

        supportedFormatsField =
            field "supportedFormats"

        propsField =
            field "props"
    in
    [ Field.input appState form (field "id") (gettext "ID" appState.locale)
        |> Field.toHtml
    , Field.input appState form (field "name") (gettext "Name" appState.locale)
        |> Field.toHtml
    , Field.markdown appState form (field "description") (gettext "Description" appState.locale)
        |> Field.toHtml
    , Field.listWithHint (gettext "Select document templates and formats that can be submitted using this submission service." appState.locale)
        appState
        form
        { field = supportedFormatsField
        , label = gettext "Supported Formats" appState.locale
        , addLabel = gettext "Add format" appState.locale
        , rowLabel = always ""
        , row = supportedFormatFormView appState templates form supportedFormatsField
        }
    , Field.listWithHintMarkdown (gettext "You can create properties that can be set by each user in their profile. Then, you can use these properties in the request settings. For example, if you want each user to use their own authorization token, you can create a property called `Token` and use it in request headers as `${Token}`. Users will be able to set the token in their profile settings." appState.locale)
        appState
        form
        { field = propsField
        , label = gettext "User Properties" appState.locale
        , addLabel = gettext "Add property" appState.locale
        , rowLabel = \propIndex -> stringValue form (Field.listItemPath propsField propIndex "")
        , row =
            \propIndex ->
                [ Field.input appState form (Field.listItemPath propsField propIndex "") (gettext "Name" appState.locale)
                    |> Field.toHtml
                ]
        }
    , requestFormView appState form (field "request")
    ]


{-| A format a service takes: a document template, one of its versions, and one
of that version's formats by name. Each is chosen from what the one before it
leaves.
-}
supportedFormatFormView : AppState -> List DocumentTemplateAllSuggestion -> Form FormError SubmissionSettings -> String -> Int -> List (Html Form.Msg)
supportedFormatFormView appState templates form prefix index =
    let
        field =
            Field.listItemPath prefix index

        value name =
            Maybe.withDefault "" (Form.getFieldAsString (field name) form).value

        defaultOption =
            ( "", "--" )

        sorted =
            List.sortWith DocumentTemplateAllSuggestion.compare templates

        templateOptions =
            sorted
                |> List.uniqueBy .id
                |> List.map (\template -> ( template.id, template.name ++ " (" ++ template.id ++ ")" ))
                |> (::) defaultOption

        versions =
            List.filter ((==) (value "id") << .id) sorted

        versionOptions =
            versions
                |> List.map (\template -> ( Version.toString template.version, Version.toString template.version ))
                |> List.reverse
                |> (::) defaultOption

        formatOptions =
            versions
                |> List.find ((==) (value "version") << Version.toString << .version)
                |> Maybe.map (.formats >> List.map (\f -> ( f.name, f.name )) >> (::) defaultOption)
                |> Maybe.withDefault [ defaultOption ]
    in
    [ Field.select appState form (field "id") (gettext "Document Template" appState.locale) templateOptions
        |> Field.toHtml
    , Layout.grid
        [ Field.select appState form (field "version") (gettext "Version" appState.locale) versionOptions
            |> Field.toHtml
        , Field.select appState form (field "formatName") (gettext "Format" appState.locale) formatOptions
            |> Field.toHtml
        ]
    ]


requestFormView : AppState -> Form FormError SubmissionSettings -> String -> Html Form.Msg
requestFormView appState form prefix =
    let
        field name =
            prefix ++ "." ++ name

        headersField =
            field "headers"

        multipartEnabled =
            Maybe.withDefault False (Form.getFieldAsBool (field "multipart.enabled") form).value

        multipartFileNameInput =
            if multipartEnabled then
                [ UiForm.nested
                    [ Field.input appState form (field "multipart.fileName") (gettext "Multipart File Name" appState.locale)
                        |> Field.toHtml
                    ]
                ]

            else
                []
    in
    UiForm.section
        { id = prefix ++ "-request"
        , legend = gettext "Request" appState.locale
        , content =
            [ Field.select appState form (field "method") (gettext "Method" appState.locale) HttpMethod.options
                |> Field.toHtml
            , Field.input appState form (field "url") (gettext "URL" appState.locale)
                |> Field.toHtml
            , Field.list appState
                form
                { field = headersField
                , label = gettext "Headers" appState.locale
                , addLabel = gettext "Add header" appState.locale
                , rowLabel = \headerIndex -> stringValue form (Field.listItemPath headersField headerIndex "key")
                , row =
                    \headerIndex ->
                        [ Layout.grid
                            [ Field.input appState form (Field.listItemPath headersField headerIndex "key") (gettext "Header" appState.locale)
                                |> Field.toHtml
                            , Field.input appState form (Field.listItemPath headersField headerIndex "value") (gettext "Value" appState.locale)
                                |> Field.toHtml
                            ]
                        ]
                }
            , Field.toggle appState form (field "multipart.enabled") (gettext "Multipart" appState.locale)
                |> Field.withHint (gettext "If enabled, file will be sent using multipart request. Otherwise, it will be directly in the request body." appState.locale)
                |> Field.toHtml
            ]
                ++ multipartFileNameInput
        }


stringValue : Form FormError SubmissionSettings -> String -> String
stringValue form path =
    Maybe.withDefault "" (Form.getFieldAsString path form).value


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model.genericModel)
