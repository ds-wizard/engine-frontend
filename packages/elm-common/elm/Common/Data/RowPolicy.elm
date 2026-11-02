module Common.Data.RowPolicy exposing
    ( Notice
    , RowBadge
    , RowPolicy
    , default
    )

{-| What a page that lists things — knowledge models, document templates,
secrets — offers for each of them, where the application mounting the page has
a say.

A page does not decide these. The application passes a policy when it draws the
page, and may draw the same page in two places with two policies. `default` is
a page with everything on offer and nothing added.

Each function is given the row's `workspaceUuid`, which is all a shared model
says about where a row belongs.

-}

import Uuid exposing (Uuid)


type alias RowPolicy =
    { -- Whether other things can be made from a row: a project or an editor
      -- from a knowledge model, an editor from a template.
      derive : Bool

    -- Whether a row can be changed: deprecated, published, deleted, given a
    -- locale; a secret edited or deleted.
    , manage : Maybe Uuid -> Bool

    -- A badge to add to a row.
    , badge : Maybe Uuid -> Maybe RowBadge

    -- Something to say at the top of a row's own page, with the page's path
    -- (`Common.Routes.toPath`) for a link to the same page elsewhere.
    , notice : Maybe Uuid -> String -> Maybe Notice
    }


{-| The icon is a Font Awesome class name, such as `"fa-building-columns"`; the
test id is the badge's `data-cy`.
-}
type alias RowBadge =
    { label : String
    , icon : Maybe String
    , testId : String
    }


{-| The icon is a Font Awesome class name, as a `RowBadge`'s is.
-}
type alias Notice =
    { text : String
    , icon : Maybe String
    , link : Maybe { label : String, url : String }
    }


default : RowPolicy
default =
    { derive = True
    , manage = always True
    , badge = always Nothing
    , notice = \_ _ -> Nothing
    }
