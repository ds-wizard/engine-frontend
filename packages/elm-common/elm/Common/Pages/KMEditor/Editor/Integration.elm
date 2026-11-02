module Common.Pages.KMEditor.Editor.Integration exposing (Config, fields)

{-| The integration form: the largest single editor in the knowledge model.

An integration is either **an API** — a request the wizard makes to somebody
else's service to offer answers — or **a plugin**, which is a name and some
settings the plugin itself understands. An integration the client has built in
(`Common.Data.BuiltInIntegration`) is stored as a plugin's but is a type of
its own here, with the client's own fields.

The API half is written in Jinja over three things: `{{ q }}`, the search term;
`{{ variables.name }}`, filled in on each question that uses the integration;
and `{{ secrets.name }}`, which the tenant holds and the knowledge model never
sees. What can go wrong with those names is what the warnings panel reports.

**It is four sections, not one long form.** Name and what the integration is;
then the Request, the Test, and the Response — which is the order the work is
done in. Configuring the response is impossible without having seen one, so
that section says so until a test has been run and then builds itself out of
what came back: the paths to every list in the body, and the properties of the
first item in the chosen one, as values to be inserted.

**What is folded away is what is rarely changed**, and it folds with a native
`details`, so nothing here holds open/closed state.

**The help is markdown**, because most of it is about Jinja and reads as code.

-}

import ActionResult exposing (ActionResult)
import Common.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Common.Api.Models.Event.EditEventSetters exposing (setAllowCustomReply, setAnnotations, setName, setPluginIntegrationSettings, setRequestAllowEmptySearch, setRequestBody, setRequestHeaders, setRequestMethod, setRequestUrl, setResponseItemTemplate, setResponseItemTemplateForSelection, setResponseListField, setTestQ, setTestVariables, setVariables)
import Common.Api.Models.Event.EditIntegrationApiEventData as EditIntegrationApiEventData exposing (EditIntegrationApiEventData)
import Common.Api.Models.Event.EditIntegrationEventData exposing (EditIntegrationEventData(..))
import Common.Api.Models.Event.EditIntegrationPluginEventData as EditIntegrationPluginEventData
import Common.Api.Models.Event.EventContent exposing (EventContent(..))
import Common.Api.Models.Event.EventField as EventField
import Common.Api.Models.KnowledgeModel.Annotation exposing (Annotation)
import Common.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration(..))
import Common.Api.Models.KnowledgeModel.Integration.ApiIntegrationData as ApiIntegrationData exposing (ApiIntegrationData)
import Common.Api.Models.KnowledgeModel.Integration.KeyValuePair exposing (KeyValuePair)
import Common.Api.Models.KnowledgeModel.Integration.PluginIntegrationData exposing (PluginIntegrationData)
import Common.Api.Models.TypeHint exposing (TypeHint)
import Common.Api.Models.TypeHintTestResponse as TypeHintTestResponse exposing (TypeHintTestResponse)
import Common.Components.Copyable as Copyable
import Common.Components.Markdown as Markdown
import Common.Components.TemplateEditor as TemplateEditor
import Common.Data.BuiltInIntegration as BuiltInIntegration exposing (BuiltInIntegration)
import Common.Pages.KMEditor.Editor.Widgets as Widgets
import Common.Plugins.Plugin as Plugin
import Common.Plugins.PluginElement as PluginElement exposing (PluginElement)
import Common.Utils.HttpStatus as HttpStatus
import Common.Utils.JsonUtils as JsonUtils
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (class)
import Json.Encode as E
import List.Extra as List
import Maybe.Extra as Maybe
import Ui.Alert as Alert
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.CodeBlock as CodeBlock
import Ui.Copy as Copy
import Ui.Field as Field
import Ui.Form as UiForm
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.ListField as ListField
import Ui.Text as Text
import Uuid exposing (Uuid)


type alias Config msg =
    { locale : Gettext.Locale
    , fieldId : String -> String
    , uuid : String

    -- Typing is debounced; anything structural is not.
    , edit : EventContent -> msg
    , atOnce : EventContent -> msg

    -- The tenant's secrets, named so the request can use them, and where they
    -- are configured.
    , secrets : List String
    , secretsUrl : String

    -- Integrations somebody can start from rather than fill in, offered only
    -- while this one is still empty.
    , prefabs : List Integration
    , isEmpty : Bool

    -- Running a test, and what came back. The response itself is kept on the
    -- integration, so it survives a reload; this is only the request in flight.
    , testing : Bool
    , testError : Maybe String
    , onTest : msg

    -- What the templates render to, fetched when a preview is asked for.
    , previewOpen : String -> Bool
    , preview : String -> Maybe (Result String (List TypeHint))
    , onPreview : String -> msg
    , onTemplate : String -> msg

    -- Where the caret was, so a suggestion lands where it was left, and how to
    -- put it back after one has been inserted.
    , cursor : String -> Maybe ( Int, Int )
    , onCursor : String -> ( Int, Int ) -> msg
    , onInsert : { fieldId : String, caret : Int } -> msg -> msg

    -- Copying a Jinja reference, and the response body.
    , copy : Copy.Model
    , copyMsg : Copy.Msg -> msg

    -- Importing the request from a cURL command.
    , onCurlImport : msg

    -- The questions that use this integration, as links.
    , questions : List { uuid : String, name : String, url : String }

    -- The plugins that offer a knowledge model integration, each with the
    -- element it draws its own settings with.
    , plugins : List ( Uuid, Plugin.KnowledgeModelIntegrationConnector )
    , pluginView : Uuid -> PluginElement -> List (Html.Attribute msg) -> Html msg

    -- The editor, as the plugin's element is told it: what it lists can be
    -- the editor's workspace's.
    , editor : Maybe { uuid : Uuid, name : String, workspaceUuid : Maybe Uuid }

    -- The integrations the client has built in, what each of them loaded
    -- with the editor, and the configuration in force their fields are drawn
    -- for.
    , builtIns : List BuiltInIntegration
    , builtInLoaded : BuiltInIntegration -> ActionResult E.Value
    , bootstrapConfig : BootstrapConfig
    }


fields : Config msg -> Integration -> List (Html msg)
fields config integration =
    quickSetup config
        ++ typeField config integration
        :: (case integration of
                ApiIntegration data ->
                    apiFields config data

                PluginIntegration data ->
                    case BuiltInIntegration.find config.builtIns data of
                        Just builtIn ->
                            builtInFields config builtIn data

                        Nothing ->
                            pluginFields config data
           )
        ++ [ annotationsOf config (Integration.getAnnotations integration) integration
           , questionsUsing config
           ]


{-| An integration somebody else has already written, as a starting point. Only
while this one is untouched: it replaces every field at once, so offering it
over work already done would be a trap.
-}
quickSetup : Config msg -> List (Html msg)
quickSetup config =
    if List.isEmpty config.prefabs || not config.isEmpty then
        []

    else
        [ UiForm.section
            { id = config.fieldId "prefabs"
            , legend = gettext "Quick setup" config.locale
            , content =
                [ Text.muted (gettext "Start from an integration that is already written. It fills in everything below." config.locale)
                , div [ class "ui-inline" ]
                    (List.map (prefabButton config) (List.sortBy Integration.getVisibleName config.prefabs))
                ]
            }
        ]


{-| Every field of the prefab at once, marked changed, which is what makes it a
starting point rather than a merge.
-}
fromPrefab : Integration -> EditIntegrationEventData
fromPrefab prefab =
    case prefab of
        ApiIntegration data ->
            EditIntegrationApiEvent
                { allowCustomReply = EventField.create data.allowCustomReply True
                , annotations = EventField.create data.annotations True
                , name = EventField.create data.name True
                , requestAllowEmptySearch = EventField.create data.requestAllowEmptySearch True
                , requestBody = EventField.create data.requestBody True
                , requestHeaders = EventField.create data.requestHeaders True
                , requestMethod = EventField.create data.requestMethod True
                , requestUrl = EventField.create data.requestUrl True
                , responseItemTemplate = EventField.create data.responseItemTemplate True
                , responseItemTemplateForSelection = EventField.create data.responseItemTemplateForSelection True
                , responseListField = EventField.create data.responseListField True
                , testQ = EventField.create data.testQ True
                , testResponse = EventField.create data.testResponse True
                , testVariables = EventField.create data.testVariables True
                , variables = EventField.create data.variables True
                }

        PluginIntegration data ->
            EditIntegrationPluginEvent
                { annotations = EventField.create data.annotations True
                , name = EventField.create data.name True
                , pluginIntegrationId = EventField.create data.pluginIntegrationId True
                , pluginIntegrationSettings = EventField.create data.pluginIntegrationSettings True
                , pluginUuid = EventField.create data.pluginUuid True
                }


prefabButton : Config msg -> Integration -> Html msg
prefabButton config prefab =
    Button.toHtml
        (Button.secondary (Integration.getVisibleName prefab) (config.atOnce (EditIntegrationEvent (fromPrefab prefab)))
            |> Button.withIcon (Icon.solid "fa-right-left")
            |> Button.small
        )


{-| A built-in integration is a choice of its own, by its key, though it is a
plugin integration underneath: choosing it or leaving it for Plugin sets the
plugin and its settings, which an edit of a plugin integration would otherwise
keep.
-}
typeField : Config msg -> Integration -> Html msg
typeField config integration =
    let
        pluginEvent pluginUuid integrationId =
            let
                event =
                    EditIntegrationPluginEventData.init
            in
            EditIntegrationPluginEvent
                { event
                    | pluginUuid = EventField.create pluginUuid True
                    , pluginIntegrationId = EventField.create integrationId True
                    , pluginIntegrationSettings = EventField.create "" True
                }
    in
    Widgets.select
        { id = config.fieldId "type"
        , label = gettext "Type" config.locale
        , value =
            case integration of
                PluginIntegration data ->
                    Maybe.unwrap "Plugin" BuiltInIntegration.key (BuiltInIntegration.find config.builtIns data)

                _ ->
                    Integration.getTypeString integration
        , options =
            [ ( "Api", gettext "API" config.locale )
            , ( "Plugin", gettext "Plugin" config.locale )
            ]
                ++ List.map (\builtIn -> ( BuiltInIntegration.key builtIn, builtIn.name )) config.builtIns
        , onInput =
            \value ->
                config.atOnce
                    (EditIntegrationEvent
                        (case ( value, BuiltInIntegration.findByKey config.builtIns value ) of
                            ( _, Just builtIn ) ->
                                pluginEvent (Uuid.toString builtIn.pluginUuid) builtIn.integrationId

                            ( "Plugin", Nothing ) ->
                                pluginEvent "" ""

                            _ ->
                                EditIntegrationApiEvent EditIntegrationApiEventData.init
                        )
                    )
        }



-- AN API


apiFields : Config msg -> ApiIntegrationData -> List (Html msg)
apiFields config data =
    [ nameField config (change config setName) data.name
    , UiForm.collapsible
        { label = gettext "Advanced Integration Configuration" config.locale
        , content =
            [ Field.toHtml
                (Field.toggle
                    { id = config.fieldId "allowCustomReply"
                    , label = gettext "Allow Custom Reply" config.locale
                    , checked = data.allowCustomReply
                    , onCheck = change config setAllowCustomReply
                    }
                    |> Field.withHintHtml (help (gettext "If enabled, users can type their own custom reply. If disabled, they must select a reply from the integration." config.locale))
                )
            , variablesField config data
            , secretsField config
            ]
        }
    , requestSection config data
    , testSection config data
    , responseSection config data
    ]


{-| The name, which both kinds of integration have — and which is edited with the
kind's own event, because an edit event says what kind of integration it is
editing. Sending an API edit for a plugin integration's name turns the
integration back into an API one as it is typed.
-}
nameField : Config msg -> (String -> msg) -> String -> Html msg
nameField config onInput name =
    Field.toHtml
        (Field.input
            { id = config.fieldId "name"
            , label = gettext "Name" config.locale
            , value = name
            , onInput = onInput
            }
            |> Field.withHintHtml (help (gettext "A name visible everywhere else in the knowledge model editor, such as when choosing the integration for a question." config.locale))
        )



-- THE REQUEST


requestSection : Config msg -> ApiIntegrationData -> Html msg
requestSection config data =
    Field.group
        { id = config.fieldId "request"
        , legend = gettext "Request" config.locale
        , fields =
            [ Widgets.select
                { id = config.fieldId "requestMethod"
                , label = gettext "Method" config.locale
                , value = data.requestMethod
                , options = List.map (\method -> ( method, method )) [ "GET", "POST" ]
                , onInput = change config setRequestMethod
                }
            , Field.toHtml
                (Field.input
                    { id = config.fieldId "requestUrl"
                    , label = gettext "URL" config.locale
                    , value = data.requestUrl
                    , onInput = change config setRequestUrl
                    }
                    |> Field.withHintHtml (help (gettext "The full API endpoint used for search. Use `{{ q }}` for the search term, for example *https://example.com/search?q={{ q }}*, and `{{ variables.name }}` and `{{ secrets.name }}` for variables and secrets." config.locale))
                )
            , UiForm.collapsible
                { label = gettext "Advanced Request Configuration" config.locale
                , content =
                    [ headers config data
                    , Field.toHtml
                        (Field.textarea
                            { id = config.fieldId "requestBody"
                            , label = gettext "Request HTTP Body" config.locale
                            , value = Maybe.withDefault "" data.requestBody
                            , onInput = change config setRequestBody << toMaybe
                            }
                            |> Field.withHintHtml (help (gettext "An optional body for the request, with the same references available again." config.locale))
                        )
                    , Field.toHtml
                        (Field.toggle
                            { id = config.fieldId "requestAllowEmptySearch"
                            , label = gettext "Allow Empty Search" config.locale
                            , checked = data.requestAllowEmptySearch
                            , onCheck = change config setRequestAllowEmptySearch
                            }
                            |> Field.withHintHtml (help (gettext "With this on, the request is sent even before anything has been typed, which suits a service that returns a default set of results." config.locale))
                        )
                    ]
                }
            ]
        }
        |> Field.withGroupStacked
        |> Field.withGroupHeaderAction
            (Button.toHtml
                (Button.ghost (gettext "Import from cURL" config.locale) config.onCurlImport
                    |> Button.withIcon (Icon.solid "fa-download")
                    |> Button.small
                    |> Button.withTestId "km-editor_integration_curl-import"
                )
            )
        |> Field.groupToHtml


headers : Config msg -> ApiIntegrationData -> Html msg
headers config data =
    let
        row index pair =
            ListField.row
                { label =
                    if String.isEmpty pair.key then
                        gettext "Header" config.locale

                    else
                        pair.key
                , content =
                    [ Widgets.string
                        { id = config.fieldId ("header-key-" ++ String.fromInt index)
                        , label = gettext "Name" config.locale
                        , value = pair.key
                        , onInput = \value -> setHeaders config (setAt index { pair | key = value } data.requestHeaders)
                        }
                    , Widgets.string
                        { id = config.fieldId ("header-value-" ++ String.fromInt index)
                        , label = gettext "Value" config.locale
                        , value = pair.value
                        , onInput = \value -> setHeaders config (setAt index { pair | value = value } data.requestHeaders)
                        }
                    ]
                }
                -- Positional, as its fields' ids are: the rows have no key of their own.
                |> ListField.withRowTestId (config.fieldId "requestHeaders" ++ "_row_" ++ String.fromInt index)
    in
    ListField.toHtml
        { id = config.fieldId "requestHeaders"
        , label = gettext "Request HTTP Headers" config.locale
        , rows = List.indexedMap row data.requestHeaders
        , addLabel = gettext "Add header" config.locale
        , onAdd = setHeaders config (data.requestHeaders ++ [ { key = "", value = "" } ])
        , onRemove = Just (\index -> setHeaders config (removeAt index data.requestHeaders))
        , hint = Just (help (gettext "HTTP headers to include in the request. The same `{{ q }}`, `{{ variables.name }}` and `{{ secrets.name }}` can be used here." config.locale))
        , error = Nothing
        }


setHeaders : Config msg -> List KeyValuePair -> msg
setHeaders config value =
    change config setRequestHeaders value



-- VARIABLES AND SECRETS


{-| The names a question can fill in, each with the Jinja that refers to it
ready to copy: the reference is what goes in the request, and typing it out is
where the spelling mistakes come from.
-}
variablesField : Config msg -> ApiIntegrationData -> Html msg
variablesField config data =
    let
        row index name =
            ListField.row
                { label =
                    if String.isEmpty name then
                        gettext "Variable" config.locale

                    else
                        name
                , content =
                    [ Widgets.string
                        { id = config.fieldId ("variable-" ++ String.fromInt index)
                        , label = gettext "Name" config.locale
                        , value = name
                        , onInput = \value -> change config setVariables (setAt index value data.variables)
                        }
                    ]
                }
                -- Positional, as its fields' ids are: the rows have no key of their own.
                |> ListField.withRowTestId (config.fieldId "variables" ++ "_row_" ++ String.fromInt index)
                |> ListField.withRowExtras [ jinjaReference config ("variable-" ++ String.fromInt index) "variables" name ]
    in
    ListField.toHtml
        { id = config.fieldId "variables"
        , label = gettext "Variables" config.locale
        , rows = List.indexedMap row data.variables
        , addLabel = gettext "Add variable" config.locale
        , onAdd = change config setVariables (data.variables ++ [ "" ])
        , onRemove = Just (\index -> change config setVariables (removeAt index data.variables))
        , hint = Just (help (gettext "Variables parametrize the integration for each question. Define them here, fill them in on the questions that use the integration, and refer to them in the request: a variable named *type* is `{{ variables.type }}`." config.locale))
        , error = Nothing
        }


secretsField : Config msg -> Html msg
secretsField config =
    Field.toHtml
        (Field.readOnlyHtml
            { id = config.fieldId "secrets"
            , label = gettext "Secrets" config.locale
            , value =
                [ if List.isEmpty config.secrets then
                    Text.muted (gettext "There are no secrets configured." config.locale)

                  else
                    div [ class "c-km-editor-secrets" ]
                        (List.indexedMap (secretRow config) config.secrets)
                , Link.toHtml
                    (Link.external (Link.link { url = config.secretsUrl, label = gettext "Configure secrets" config.locale }))
                ]
            }
            |> Field.withHintHtml (help (gettext "Secrets hold what the knowledge model should not carry, such as a token. Use one as `{{ secrets.name }}` anywhere in the request." config.locale))
        )


secretRow : Config msg -> Int -> String -> Html msg
secretRow config index name =
    div [ class "c-km-editor-secret" ]
        [ Text.body name
        , jinjaReference config ("secret-" ++ String.fromInt index) "secrets" name
        ]


{-| The Jinja that refers to a variable or a secret, on the clipboard. A name
that is not a plain identifier is subscripted rather than dotted, which is the
only form Jinja accepts for it.
-}
jinjaReference : Config msg -> String -> String -> String -> Html msg
jinjaReference config key domain name =
    if String.isEmpty name then
        text ""

    else
        Copy.buttonWithWords
            { prompt = gettext "Copy code" config.locale
            , done = gettext "Copied!" config.locale
            }
            { key = config.fieldId key
            , label = String.replace "%s" (TemplateEditor.reference domain name) (gettext "Copy %s" config.locale)
            , value = TemplateEditor.reference domain name
            , model = config.copy
            , toMsg = config.copyMsg
            }



-- THE TEST


testSection : Config msg -> ApiIntegrationData -> Html msg
testSection config data =
    UiForm.section
        { id = config.fieldId "test"
        , legend = gettext "Test" config.locale
        , content =
            Field.toHtml
                (Field.input
                    { id = config.fieldId "testQ"
                    , label = gettext "Test Search Query" config.locale
                    , value = data.testQ
                    , onInput = change config setTestQ
                    }
                    |> Field.withHintHtml (help (gettext "What somebody would type in the questionnaire when answering a question that uses this integration." config.locale))
                )
                :: testVariables config data
                ++ [ div [ class "ui-inline" ]
                        [ Button.toHtml
                            (Button.secondary (gettext "Load" config.locale) config.onTest
                                |> Button.withLoading config.testing
                                |> Button.withTestId "km-editor_integration_test"
                            )
                        ]
                   ]
                ++ testOutcome config data
        }


testVariables : Config msg -> ApiIntegrationData -> List (Html msg)
testVariables config data =
    let
        named =
            List.filterNot String.isEmpty data.variables

        row name =
            Field.toHtml
                (Field.input
                    { id = config.fieldId ("testVariable-" ++ name)
                    , label = name
                    , value = Maybe.withDefault "" (ApiIntegrationData.getTestVariableValue name data)
                    , onInput = \value -> change config setTestVariables (Dict.insert name value data.testVariables)
                    }
                    -- The name in front of the box rather than over it: these
                    -- are one row each and the name is what the value is for.
                    |> Field.withHiddenLabel
                    |> Field.withPrefix name
                )
    in
    if List.isEmpty named then
        []

    else
        [ Field.groupToHtml
            (Field.group
                { id = config.fieldId "testVariables"
                , legend = gettext "Test Variables" config.locale
                , fields = List.map row named
                }
                |> Field.withGroupStacked
            )
        ]


testOutcome : Config msg -> ApiIntegrationData -> List (Html msg)
testOutcome config data =
    case ( config.testError, data.testResponse ) of
        ( Just message, _ ) ->
            [ Alert.toHtml (Alert.danger message) ]

        ( Nothing, Just response ) ->
            [ UiForm.collapsible
                { label = gettext "Request Details" config.locale
                , content = [ requestDetails config response ]
                }
            , responseOutcome config response
            ]

        ( Nothing, Nothing ) ->
            []


requestDetails : Config msg -> TypeHintTestResponse -> Html msg
requestDetails config response =
    CodeBlock.toHtml
        { label = gettext "The request that was sent" config.locale
        , language = ""
        , value =
            -- The parts that are there, one blank line between them: a request
            -- with no headers and no body is one line, not one line and four
            -- empty ones.
            [ Just (response.request.method ++ " " ++ response.request.url)
            , case response.request.headers of
                [] ->
                    Nothing

                headerList ->
                    Just (String.join "\n" (List.map (\pair -> pair.key ++ ": " ++ pair.value) headerList))
            , Maybe.andThen toMaybe response.request.body
            ]
                |> Maybe.values
                |> String.join "\n\n"
        }


responseOutcome : Config msg -> TypeHintTestResponse -> Html msg
responseOutcome config response =
    case response.response of
        TypeHintTestResponse.RequestFailedTypeHintResponse error ->
            Alert.toHtml (Alert.danger error.message)

        TypeHintTestResponse.SuccessTypeHintResponse data ->
            responseBody config data

        TypeHintTestResponse.RemoteErrorTypeHintResponse data ->
            responseBody config data


responseBody : Config msg -> TypeHintTestResponse.ResponseData -> Html msg
responseBody config data =
    let
        status =
            if HttpStatus.isSuccessful data.status then
                Badge.success (String.fromInt data.status ++ " " ++ HttpStatus.statusName data.status)

            else
                Badge.danger (String.fromInt data.status ++ " " ++ HttpStatus.statusName data.status)

        contentType =
            if TypeHintTestResponse.supportedContentType data then
                Badge.neutral data.contentType

            else
                Badge.warning data.contentType
                    |> Badge.withDescription (gettext "This content type is not supported." config.locale)

        ( body, language ) =
            case JsonUtils.prettyString 4 data.body of
                Ok pretty ->
                    ( pretty, "json" )

                Err _ ->
                    ( data.body, "" )
    in
    Layout.stack
        [ div [ class "ui-inline" ]
            [ Text.label (gettext "Response" config.locale)
            , Badge.toHtml status
            , Badge.toHtml contentType
            ]
        , CodeBlock.withActions
            [ Copyable.button { locale = config.locale }
                { key = config.fieldId "response"
                , label = gettext "Copy the response" config.locale
                , value = data.body
                , model = config.copy
                , toMsg = config.copyMsg
                }
            ]
            { label = gettext "The response" config.locale
            , language = language
            , value = body
            }
        ]



-- THE RESPONSE


responseSection : Config msg -> ApiIntegrationData -> Html msg
responseSection config data =
    UiForm.section
        { id = config.fieldId "response"
        , legend = gettext "Response" config.locale
        , content =
            case Maybe.map .response data.testResponse of
                Just (TypeHintTestResponse.SuccessTypeHintResponse responseData) ->
                    case responseData.bodyJson of
                        Just _ ->
                            responseFields config data responseData

                        Nothing ->
                            [ Alert.toHtml (Alert.warning (gettext "The response is not valid JSON, so there is nothing here to map." config.locale)) ]

                _ ->
                    [ Alert.toHtml (Alert.info (gettext "Run a test above, and what came back is what this section is built from." config.locale)) ]
        }


responseFields : Config msg -> ApiIntegrationData -> TypeHintTestResponse.ResponseData -> List (Html msg)
responseFields config data responseData =
    let
        listField =
            Maybe.withDefault "" data.responseListField
    in
    [ Field.toHtml
        (Field.input
            { id = config.fieldId "responseListField"
            , label = gettext "Response List Field" config.locale
            , value = listField
            , onInput = change config setResponseListField << toMaybe
            }
            |> Field.withHintHtml
                (suggestions
                    (TypeHintTestResponse.getSuggestedListFieldProperties responseData)
                    (\value -> change config setResponseListField (toMaybe value))
                    ++ help (gettext "Where the items are in the response. Leave it empty when the body is the list itself; otherwise give the path to the field that holds it." config.locale)
                )
        )
    , listFieldOutcome config listField responseData
    , templateField config
        { name = "responseItemTemplate"
        , label = gettext "Response Item Template" config.locale
        , value = data.responseItemTemplate
        , onInput = change config setResponseItemTemplate
        , toPreview = .value
        , properties = TypeHintTestResponse.getSuggestedItemProperties listField responseData
        }
    , UiForm.collapsible
        { label = gettext "Advanced Response Configuration" config.locale
        , content =
            [ templateField config
                { name = "responseItemTemplateForSelection"
                , label = gettext "Response Item Template for Selection" config.locale
                , value = Maybe.withDefault "" data.responseItemTemplateForSelection
                , onInput = change config setResponseItemTemplateForSelection << toMaybe
                , toPreview = Maybe.withDefault "" << .valueForSelection
                , properties = TypeHintTestResponse.getSuggestedItemProperties listField responseData
                }
            ]
        }
    ]


{-| Whether the path names a list in the response that came back. Said here
rather than left to the preview, because a path that names nothing produces an
empty preview and no reason for it.
-}
listFieldOutcome : Config msg -> String -> TypeHintTestResponse.ResponseData -> Html msg
listFieldOutcome config listField responseData =
    case TypeHintTestResponse.checkListField listField responseData of
        TypeHintTestResponse.CheckListFieldResultOk ->
            text ""

        TypeHintTestResponse.CheckListFieldResultNotFound ->
            Alert.toHtml (Alert.warning (gettext "There is no such field in the response." config.locale))

        TypeHintTestResponse.CheckListFieldResultNoList ->
            Alert.toHtml (Alert.warning (gettext "That field is in the response, but it does not hold a list." config.locale))


{-| A template, with what it renders to beside it and the properties of an item
underneath, each of which inserts its reference where the caret was
(`Common.Components.TemplateEditor`).
-}
templateField :
    Config msg
    ->
        { name : String
        , label : String
        , value : String
        , onInput : String -> msg
        , toPreview : TypeHint -> String
        , properties : List String
        }
    -> Html msg
templateField config cfg =
    TemplateEditor.view config.locale
        { id = config.fieldId cfg.name
        , label = cfg.label
        , value = cfg.value
        , onInput = cfg.onInput
        , hint = Just (help (gettext "Jinja and Markdown, over `{{ item }}`. The properties below are what the response offers." config.locale))
        , warning = Nothing
        , preview =
            if config.previewOpen cfg.name then
                Just (previewOf config cfg.name cfg.toPreview)

            else
                Nothing
        , onTemplate = config.onTemplate cfg.name
        , onPreview = config.onPreview cfg.name
        , properties = cfg.properties
        , cursor = config.cursor cfg.name
        , onCursor = config.onCursor cfg.name
        , onInsert = config.onInsert
        }


previewOf : Config msg -> String -> (TypeHint -> String) -> Html msg
previewOf config name toPreview =
    case config.preview name of
        Just (Ok hints) ->
            case List.head hints of
                Just hint ->
                    Markdown.view { headingOffset = 3 } (toPreview hint)

                Nothing ->
                    Text.muted (gettext "The request returned nothing to show." config.locale)

        Just (Err message) ->
            Alert.toHtml (Alert.danger message)

        Nothing ->
            Text.muted (gettext "Loading..." config.locale)


{-| Values worth one press, under the field they go into.
-}
suggestions : List String -> (String -> msg) -> List (Html msg)
suggestions properties onChoose =
    if List.isEmpty properties then
        []

    else
        [ div [ class "ui-inline" ]
            (List.map
                (\property ->
                    Button.toHtml
                        (Button.ghost property (onChoose property)
                            |> Button.small
                        )
                )
                properties
            )
        ]



-- A PLUGIN


pluginFields : Config msg -> PluginIntegrationData -> List (Html msg)
pluginFields config data =
    let
        plugins =
            pluginsOffered config
    in
    nameField config (changePlugin config setName) data.name
        :: (if List.isEmpty plugins then
                [ Alert.toHtml (Alert.info (gettext "There are no plugins that offer an integration." config.locale)) ]

            else
                pluginChoiceFields config plugins data
           )


{-| The plugins to choose from, without one that offers what is built in: that
one is a type of its own.
-}
pluginsOffered : Config msg -> List ( Uuid, Plugin.KnowledgeModelIntegrationConnector )
pluginsOffered config =
    List.filter
        (\( pluginUuid, connector ) ->
            Maybe.isNothing (BuiltInIntegration.find config.builtIns { pluginUuid = Uuid.toString pluginUuid, pluginIntegrationId = connector.integrationId })
        )
        config.plugins


pluginChoiceFields : Config msg -> List ( Uuid, Plugin.KnowledgeModelIntegrationConnector ) -> PluginIntegrationData -> List (Html msg)
pluginChoiceFields config plugins data =
    let
        chosen =
            List.filter
                (\( pluginUuid, connector ) ->
                    Uuid.toString pluginUuid == data.pluginUuid && connector.integrationId == data.pluginIntegrationId
                )
                plugins
                |> List.head
    in
    [ Widgets.select
        { id = config.fieldId "plugin"
        , label = gettext "Plugin" config.locale
        , value = data.pluginUuid ++ "/" ++ data.pluginIntegrationId
        , options =
            ( "", gettext "Choose a plugin" config.locale )
                :: List.map
                    (\( pluginUuid, connector ) ->
                        ( Uuid.toString pluginUuid ++ "/" ++ connector.integrationId, connector.name )
                    )
                    plugins
        , onInput = choosePlugin config
        }

    -- The settings are the plugin's own element: what an integration needs
    -- to know is the plugin's business, not the editor's.
    , case chosen of
        Just ( pluginUuid, connector ) ->
            config.pluginView pluginUuid
                connector.editorElement
                (PluginElement.pluginIntegrationSettingsValue data.pluginIntegrationSettings
                    :: PluginElement.onPluginIntegrationSettingsChange (changePlugin config setPluginIntegrationSettings)
                    :: (Maybe.map (List.singleton << PluginElement.knowledgeModelEditorValue) config.editor |> Maybe.withDefault [])
                )

        Nothing ->
            text ""
    ]


{-| One choice sets two fields, because a plugin and the integration it offers
are one thing to choose and two things to store.
-}
choosePlugin : Config msg -> String -> msg
choosePlugin config value =
    let
        ( pluginUuid, integrationId ) =
            case String.split "/" value of
                [ chosenUuid, chosenId ] ->
                    ( chosenUuid, chosenId )

                _ ->
                    ( Uuid.toString Uuid.nil, "" )

        event =
            EditIntegrationPluginEventData.init
    in
    config.atOnce
        (EditIntegrationEvent
            (EditIntegrationPluginEvent
                { event
                    | pluginUuid = EventField.create pluginUuid True
                    , pluginIntegrationId = EventField.create integrationId True
                }
            )
        )



-- BUILT IN


{-| The name, and the client's own fields, which say what the integration
answers from.
-}
builtInFields : Config msg -> BuiltInIntegration -> PluginIntegrationData -> List (Html msg)
builtInFields config builtIn data =
    nameField config (changePlugin config setName) data.name
        :: List.map (Html.map (changePlugin config setPluginIntegrationSettings))
            (builtIn.fields
                { locale = config.locale
                , fieldId = config.fieldId
                , settings = data.pluginIntegrationSettings
                , loaded = config.builtInLoaded builtIn
                , workspaceUuid = Maybe.andThen .workspaceUuid config.editor
                , config = config.bootstrapConfig
                }
            )



-- THE REST


annotationsOf : Config msg -> List Annotation -> Integration -> Html msg
annotationsOf config annotations integration =
    Widgets.annotations config
        { id = config.fieldId "annotations"
        , value = annotations
        , onChange =
            \value ->
                config.edit
                    (EditIntegrationEvent
                        (case integration of
                            ApiIntegration _ ->
                                EditIntegrationApiEvent (setAnnotations value EditIntegrationApiEventData.init)

                            PluginIntegration _ ->
                                EditIntegrationPluginEvent (setAnnotations value EditIntegrationPluginEventData.init)
                        )
                    )
        }


{-| Where this integration is already in use. A read-only list, because the
answer to "can I change this" is usually "who is using it".
-}
questionsUsing : Config msg -> Html msg
questionsUsing config =
    Field.toHtml
        (Field.readOnlyHtml
            { id = config.fieldId "questions"
            , label = gettext "Questions using this integration" config.locale
            , value =
                if List.isEmpty config.questions then
                    [ Text.muted (gettext "No questions use it yet." config.locale) ]

                else
                    [ ul [ class "c-km-editor-uses" ]
                        (List.map
                            (\question ->
                                li []
                                    [ Link.toHtml (Link.link { url = question.url, label = question.name }) ]
                            )
                            config.questions
                        )
                    ]
            }
        )



-- ODDS AND ENDS


change : Config msg -> (value -> EditIntegrationApiEventData -> EditIntegrationApiEventData) -> value -> msg
change config setter value =
    config.edit (EditIntegrationEvent (EditIntegrationApiEvent (setter value EditIntegrationApiEventData.init)))


changePlugin : Config msg -> (value -> EditIntegrationPluginEventData.EditIntegrationPluginEventData -> EditIntegrationPluginEventData.EditIntegrationPluginEventData) -> value -> msg
changePlugin config setter value =
    config.edit (EditIntegrationEvent (EditIntegrationPluginEvent (setter value EditIntegrationPluginEventData.init)))


{-| A field's description, as markdown: most of it is about Jinja and reads as
code.

It is the field's own hint rather than something drawn under it, so it is
inside the field's `aria-describedby` and is read with the field rather than
found after it — and `Markdown.inline` rather than the block renderer, because
a hint is a paragraph and a paragraph cannot hold one.

-}
help : String -> List (Html msg)
help =
    Markdown.inline


toMaybe : String -> Maybe String
toMaybe value =
    if String.isEmpty value then
        Nothing

    else
        Just value


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
