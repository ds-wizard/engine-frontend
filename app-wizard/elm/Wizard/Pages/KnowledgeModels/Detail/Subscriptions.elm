module Wizard.Pages.KnowledgeModels.Detail.Subscriptions exposing (subscriptions)

import Bootstrap.Dropdown as Dropdown
import Wizard.Pages.KnowledgeModels.Detail.ImportLocaleModal as ImportLocaleModal
import Wizard.Pages.KnowledgeModels.Detail.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Detail.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.dropdownState DropdownMsg
        , Sub.map ImportLocaleModalMsg (ImportLocaleModal.subscriptions model.importLocaleModal)
        ]
