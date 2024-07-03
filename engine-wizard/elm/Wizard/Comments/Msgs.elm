module Wizard.Comments.Msgs exposing (Msg(..))

import Shared.Data.QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg QuestionnaireCommentThreadAssigned)
