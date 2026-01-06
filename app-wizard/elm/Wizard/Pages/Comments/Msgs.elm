module Wizard.Pages.Comments.Msgs exposing (Msg(..))

import Wizard.Api.Models.ProjectCommentThreadAssigned exposing (ProjectCommentThreadAssigned)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg ProjectCommentThreadAssigned)
