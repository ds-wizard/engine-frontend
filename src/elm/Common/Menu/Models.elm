module Common.Menu.Models exposing (..)


type alias Model =
    { reportIssueOpen : Bool
    }


initialModel : Model
initialModel =
    { reportIssueOpen = False
    }
