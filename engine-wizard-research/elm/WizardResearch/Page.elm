module WizardResearch.Page exposing
    ( Page(..)
    , view
    )

import Browser exposing (Document)
import Html exposing (Html)
import WizardResearch.Common.AppState exposing (AppState)


type Page
    = Other
    | Login
    | Dashboard


view : AppState -> Page -> { title : String, content : Html msg } -> Document msg
view appState page { title, content } =
    { title = title ++ " | Data Stewardship Wizard"
    , body = [ content ]
    }
