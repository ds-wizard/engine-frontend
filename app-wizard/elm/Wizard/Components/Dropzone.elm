module Wizard.Components.Dropzone exposing
    ( DropzoneConfig
    , Msg
    , State
    , UpdateConfig
    , dropzone
    , getFile
    , getFileContent
    , initialState
    , update
    )

import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, p, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Events.Extensions exposing (alwaysPreventDefaultOn, alwaysPreventDefaultOnWithDecoder)
import Json.Decode as D
import Registry.Components.FontAwesome exposing (fas)
import Task


type State
    = State StateData


type alias StateData =
    { hover : Bool
    , file : Maybe File
    , fileContent : Maybe String
    }


initialState : State
initialState =
    State
        { hover = False
        , file = Nothing
        , fileContent = Nothing
        }


getFile : State -> Maybe File
getFile (State state) =
    state.file


getFileContent : State -> Maybe String
getFileContent (State state) =
    state.fileContent


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFile File
    | GotFileContent String


type alias UpdateConfig =
    { mimes : List String
    , readFile : Bool
    }


update : UpdateConfig -> Msg -> State -> ( State, Cmd Msg )
update cfg msg (State state) =
    case msg of
        Pick ->
            ( State state
            , Select.file cfg.mimes GotFile
            )

        DragEnter ->
            ( State { state | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( State { state | hover = False }
            , Cmd.none
            )

        GotFile file ->
            ( State { state | hover = False, file = Just file }
            , if cfg.readFile then
                Task.perform GotFileContent (File.toString file)

              else
                Cmd.none
            )

        GotFileContent fileContent ->
            ( State { state | fileContent = Just fileContent }
            , Cmd.none
            )


type alias DropzoneConfig msg =
    { wrapMsg : Msg -> msg
    , buttonText : String
    , dropzoneText : String
    , fileIcon : Maybe (Html msg)
    }


dropzone : DropzoneConfig msg -> State -> Html msg
dropzone cfg (State state) =
    case state.file of
        Just file ->
            viewFile cfg file

        Nothing ->
            viewDropzone cfg state


viewDropzone : DropzoneConfig msg -> StateData -> Html msg
viewDropzone cfg state =
    div
        [ class "dropzone rounded-3"
        , classList [ ( "active", state.hover ) ]
        , alwaysPreventDefaultOn "dragenter" (cfg.wrapMsg DragEnter)
        , alwaysPreventDefaultOn "dragover" (cfg.wrapMsg DragEnter)
        , alwaysPreventDefaultOn "dragleave" (cfg.wrapMsg DragLeave)
        , alwaysPreventDefaultOnWithDecoder "drop" (dropDecoder cfg)
        ]
        [ button [ onClick (cfg.wrapMsg Pick), class "btn btn-secondary" ]
            [ text cfg.buttonText ]
        , p [] [ text cfg.dropzoneText ]
        ]


viewFile : DropzoneConfig msg -> File -> Html msg
viewFile cfg file =
    div
        [ class "bg-light rounded-3 p-4 fs-4 text-center" ]
        [ span [ class "me-2" ] [ Maybe.withDefault (fas "fa-file") cfg.fileIcon ]
        , text (File.name file)
        ]


dropDecoder : DropzoneConfig msg -> D.Decoder msg
dropDecoder cfg =
    D.at [ "dataTransfer", "files", "0" ] (D.map (cfg.wrapMsg << GotFile) File.decoder)
