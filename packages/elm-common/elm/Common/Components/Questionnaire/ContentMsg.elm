module Common.Components.Questionnaire.ContentMsg exposing
    ( FileConfig
    , ItemEventData
    , Msg(..)
    , PluginQuestionActionData
    , QuestionActions
    , TypeHints
    )

{-| What the questionnaire's content says back, and the few records its messages
carry. Its own module so that the question and item views and the content that
dispatches to them can all speak it.
-}

import ActionResult exposing (ActionResult)
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.KnowledgeModel.Question exposing (Question)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue exposing (ReplyValue)
import Common.Api.Models.TypeHint exposing (TypeHint)
import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Plugins.Plugin exposing (Plugin, ProjectQuestionActionConnector)
import Common.Plugins.PluginElement exposing (PluginElement)
import Debounce
import Ui.Menu as Menu
import Uuid exposing (Uuid)


type Msg
    = OpenChapter String
    | CollapsePaths (List String)
    | ExpandPaths (List String)
    | ScrollToPath String
    | ScrollToQuestion String
    | SetReply String ReplyValue
    | ClearReply String
    | AddItem String (List String)
    | RemoveItemOpen ItemEventData
    | RemoveItemConfirm
    | RemoveItemCancel
    | MoveItemUp ItemEventData
    | MoveItemDown ItemEventData
      -- The menu of the item select questions an item is selected in, by the
      -- item's uuid.
    | ItemUsagesMenuMsg String Menu.Msg
    | OpenFileUpload String FileConfig
    | DeleteFile Uuid String String
    | DownloadFile Uuid
    | GotFileUrl (Result ApiError UrlResponse)
    | ShowTypeHints String Bool String String
    | HideTypeHints
    | TypeHintInput String Bool Bool String ReplyValue
    | TypeHintDebounceMsg Debounce.Msg
    | TypeHintsLoaded String String (Result ApiError (List TypeHint))
      -- The hint the arrow keys or the pointer are on, by its index.
    | HighlightTypeHint String (Maybe Int)
    | ChooseTypeHint String ReplyValue
    | SetPluginReply String String
    | CopyLinkToQuestion String
    | ClearCopiedLink String
    | AddTodo String
    | RemoveTodo String
    | OpenComments String
    | CloseRightPanel
    | OpenPluginQuestionActionModal Uuid PluginElement Question String
    | OpenPluginQuestionActionRightPanel PluginQuestionActionData
      -- Keeps focus in a type-hint input while a hint is pressed; the choice is
      -- made on the click (WCAG 2.5.2).
    | Noop


{-| An item of a list, for the messages that move or remove it: the list's path,
its items as they are, and the one acted on.
-}
type alias ItemEventData =
    { path : String
    , itemUuids : List String
    , itemUuid : String
    }


type alias TypeHints =
    { path : String
    , searchValue : String
    , hints : ActionResult (List TypeHint)
    , highlighted : Maybe Int
    }


{-| What a file question accepts, for the upload.
-}
type alias FileConfig =
    { fileTypes : Maybe String
    , maxSize : Maybe Int
    }


type alias PluginQuestionActionData =
    { plugin : Plugin
    , connector : ProjectQuestionActionConnector
    , question : Question
    , questionPath : String
    }


{-| The plugin actions a question offers, and which of them has its panel open.

Every question is given the same value — held in the model, so the same reference
render after render — except the one whose panel is open, which is given another
held value. Opening a panel changes the argument of that one node, and no other:
the wizard marked the open panel on every node instead, and re-rendered them all.

-}
type alias QuestionActions =
    { plugins : List ( Plugin, ProjectQuestionActionConnector )
    , open : Maybe ( Uuid, ProjectQuestionActionConnector )
    }
