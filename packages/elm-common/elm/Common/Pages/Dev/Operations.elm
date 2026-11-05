module Common.Pages.Dev.Operations exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

{-| The operations a developer can run on the server, a section at a time: the
sections are the menu on the side (`SettingsPage.layout`), as the settings'
sections are, and the one shown is part of the address.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.DevOperations as DevOperationsApi
import Common.Api.Models.DevOperation exposing (DevOperation)
import Common.Api.Models.DevOperation.DevOperationParameter exposing (DevOperationParameter)
import Common.Api.Models.DevOperation.DevOperationParameterType as DevOperationParameterType
import Common.Api.Models.DevOperationExecutionResult exposing (DevOperationExecutionResult)
import Common.Api.Models.DevOperationSection as DevOperationSection exposing (DevOperationSection)
import Common.Api.Models.TenantSuggestion exposing (TenantSuggestion)
import Common.Api.Tenants as TenantsApi
import Common.Components.Form as FormLayout
import Common.Components.Markdown as Markdown
import Common.Components.Page as Page
import Common.Components.SettingsPage as SettingsPage
import Common.Components.TypeHint as TypeHint
import Common.Data.AppState exposing (AppState)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Dev.Routes as DevRoutes
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.Card as Card
import Ui.CodeBlock as CodeBlock
import Ui.Field as Field
import Ui.Form
import Ui.Layout as Layout
import Ui.Text as Text
import Uuid


type alias Model =
    { adminOperationSections : ActionResult (List DevOperationSection)
    , openedSection : Maybe String
    , fieldValues : Dict String String
    , typeHints : Dict String (TypeHint.Model TenantSuggestion)
    , operationResults : Dict String (ActionResult DevOperationExecutionResult)
    }


type Msg
    = GetDevOperationsComplete (Result ApiError (List DevOperationSection))
    | FieldInput String String
    | FieldInputBool String Bool
    | TypeHintMsg String (TypeHint.Msg TenantSuggestion)
    | ExecuteOperation String String
    | ExecuteOperationComplete String String (Result ApiError DevOperationExecutionResult)


initialModel : Model
initialModel =
    { adminOperationSections = Loading
    , openedSection = Nothing
    , fieldValues = Dict.empty
    , typeHints = Dict.empty
    , operationResults = Dict.empty
    }


{-| Moving between the sections keeps what was read and what was typed: the
operations are asked for once, and after that only the section changes.
-}
init : AppState -> Maybe String -> Model -> ( Model, Cmd Msg )
init appState mbSection model =
    if ActionResult.isSuccess model.adminOperationSections then
        ( { model | openedSection = mbSection }, Cmd.none )

    else
        ( { initialModel | openedSection = mbSection }
        , DevOperationsApi.getOperations appState GetDevOperationsComplete
        )


fieldPath : String -> String -> String -> String
fieldPath sectionName operationName parameterName =
    sectionName ++ "__" ++ operationName ++ "__" ++ parameterName


operationPath : String -> String -> String
operationPath sectionName operationName =
    sectionName ++ "__" ++ operationName


{-| Section, operation and parameter names are free text; an id is not.
-}
domId : String -> String
domId path =
    path
        |> String.map
            (\char ->
                if Char.isAlphaNum char || char == '-' || char == '_' then
                    char

                else
                    '-'
            )
        |> (++) "dev-operation-"


getSection : String -> Model -> Maybe DevOperationSection
getSection sectionName model =
    ActionResult.unwrap Nothing (List.find (.name >> (==) sectionName)) model.adminOperationSections


getTypeHint : String -> Model -> TypeHint.Model TenantSuggestion
getTypeHint path model =
    Maybe.withDefault TypeHint.init (Dict.get path model.typeHints)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetDevOperationsComplete result ->
            case result of
                Ok sections ->
                    ( { model | adminOperationSections = Success sections }, Cmd.none )

                Err error ->
                    ( { model | adminOperationSections = ApiError.toActionResult appState "Unable to get dev operations." error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        FieldInput path value ->
            ( { model | fieldValues = Dict.insert path value model.fieldValues }, Cmd.none )

        FieldInputBool path value ->
            let
                stringValue =
                    if value then
                        "True"

                    else
                        "False"
            in
            ( { model | fieldValues = Dict.insert path stringValue model.fieldValues }, Cmd.none )

        TypeHintMsg path typeHintMsg ->
            let
                updateConfig =
                    { wrapMsg = cfg.wrapMsg << TypeHintMsg path
                    , getTypeHints = TenantsApi.getTenantSuggestions appState
                    , getError = "Unable to get tenants."
                    , setReply = cfg.wrapMsg << FieldInput path << Uuid.toString << .uuid
                    , clearReply = Just (cfg.wrapMsg (FieldInput path ""))
                    , filterResults = Nothing
                    , logoutMsg = cfg.logoutMsg
                    , id = domId path
                    , toKey = Uuid.toString << .uuid
                    }

                ( typeHint, cmd ) =
                    TypeHint.update appState updateConfig typeHintMsg (getTypeHint path model)
            in
            ( { model | typeHints = Dict.insert path typeHint model.typeHints }, cmd )

        ExecuteOperation sectionName operationName ->
            case Maybe.andThen (DevOperationSection.getOperation operationName) (getSection sectionName model) of
                Just operation ->
                    if ActionResult.isLoading (operationResult sectionName operationName model) then
                        ( model, Cmd.none )

                    else
                        let
                            getParameterValue parameter =
                                Maybe.withDefault "" (Dict.get (fieldPath sectionName operationName parameter.name) model.fieldValues)

                            execution =
                                { sectionName = sectionName
                                , operationName = operationName
                                , parameters = List.map getParameterValue operation.parameters
                                }
                        in
                        ( { model | operationResults = Dict.insert (operationPath sectionName operationName) Loading model.operationResults }
                        , DevOperationsApi.executeOperation appState execution (cfg.wrapMsg << ExecuteOperationComplete sectionName operationName)
                        )

                Nothing ->
                    ( model, Cmd.none )

        ExecuteOperationComplete sectionName operationName result ->
            let
                operationResultValue =
                    case result of
                        Ok executionResult ->
                            Success executionResult

                        Err error ->
                            ApiError.toActionResult appState "Execution failed." error
            in
            ( { model | operationResults = Dict.insert (operationPath sectionName operationName) operationResultValue model.operationResults }
            , RequestHelpers.getResultCmd cfg.logoutMsg result
            )


operationResult : String -> String -> Model -> ActionResult DevOperationExecutionResult
operationResult sectionName operationName model =
    Maybe.withDefault Unset (Dict.get (operationPath sectionName operationName) model.operationResults)



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResult appState (viewContent appState model) model.adminOperationSections


viewContent : AppState -> Model -> List DevOperationSection -> Html Msg
viewContent appState model sections =
    let
        opened =
            sections
                |> List.find (.name >> Just >> (==) model.openedSection)
                |> Maybe.orElse (List.head sections)

        entry section =
            { label = section.name
            , url = Routes.toUrl appState (Routes.DevRoute (DevRoutes.OperationsRoute (Just section.name)))
            , active = Maybe.map .name opened == Just section.name
            }
    in
    SettingsPage.layout
        { title = "Dev Operations"
        , groups = [ ( Nothing, List.map entry sections ) ]
        , body =
            case opened of
                Just section ->
                    viewSection appState model section

                Nothing ->
                    [ Text.pageTitle "Dev Operations" ]
        , footer = Nothing
        }


{-| The section's name is the page's heading, as a settings section's is.
-}
viewSection : AppState -> Model -> DevOperationSection -> List (Html Msg)
viewSection appState model section =
    [ Layout.stack
        (Text.pageTitle section.name
            :: viewDescription 1 section.description
            ++ List.map (viewOperation appState model section.name) section.operations
        )
    ]


{-| `headingOffset` is the level of the heading the description sits under: the
section's `h1`, or an operation card's `h2`.
-}
viewDescription : Int -> Maybe String -> List (Html msg)
viewDescription headingOffset =
    Maybe.map (List.singleton << Markdown.view { headingOffset = headingOffset }) >> Maybe.withDefault []


viewOperation : AppState -> Model -> String -> DevOperation -> Html Msg
viewOperation appState model sectionName operation =
    let
        result =
            operationResult sectionName operation.name model

        resultView =
            case result of
                Success executionResult ->
                    CodeBlock.toHtml
                        { label = "Result"
                        , language = ""
                        , value = executionResult.output
                        }

                Error error ->
                    Alert.toHtml (Alert.danger error)

                _ ->
                    Html.text ""
    in
    Card.viewWithTitleLevel 2
        operation.name
        (viewDescription 2 operation.description
            ++ [ FormLayout.view
                    { onSubmit = ExecuteOperation sectionName operation.name
                    , content =
                        List.map (viewParameter appState model sectionName operation.name) operation.parameters
                            ++ [ Ui.Form.actions
                                    [ Button.submit "Execute"
                                        |> Button.withLoading (ActionResult.isLoading result)
                                    ]
                               ]
                    }
               , resultView
               ]
        )


viewParameter : AppState -> Model -> String -> String -> DevOperationParameter -> Html Msg
viewParameter appState model sectionName operationName parameter =
    let
        path =
            fieldPath sectionName operationName parameter.name

        fieldValue =
            Maybe.withDefault "" (Dict.get path model.fieldValues)

        simpleField typeLabel placeholder =
            Field.input
                { id = domId path
                , label = parameter.name
                , value = fieldValue
                , onInput = FieldInput path
                }
                |> Field.withPrefix typeLabel
                |> Field.withPlaceholder placeholder
                |> Field.toHtml
    in
    case parameter.type_ of
        DevOperationParameterType.String ->
            simpleField "string" "abc"

        DevOperationParameterType.Int ->
            simpleField "int" "1"

        DevOperationParameterType.Double ->
            simpleField "double" "2.3"

        DevOperationParameterType.Bool ->
            Field.checkbox
                { id = domId path
                , label = parameter.name
                , checked = fieldValue == "True"
                , onCheck = FieldInputBool path
                }
                |> Field.toHtml

        DevOperationParameterType.Json ->
            simpleField "Json" "{ \"prop\": 1 }"

        DevOperationParameterType.Tenant ->
            TypeHint.view appState
                { id = domId path
                , label = parameter.name
                , placeholder = "Select tenant"
                , required = False
                , error = Nothing
                , hint = Nothing
                , clearable = True
                , viewItem = \tenant -> { label = tenant.name, code = Nothing, description = Just tenant.clientUrl, icon = Nothing, badges = [] }
                , toKey = Uuid.toString << .uuid
                , wrapMsg = TypeHintMsg path
                }
                (getTypeHint path model)
