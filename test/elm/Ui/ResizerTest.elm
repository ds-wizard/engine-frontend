module Ui.ResizerTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Ui.Resizer as Resizer


{-| The width arithmetic, which is the part with somewhere to go wrong.

What these cannot cover is the drag itself — the separator's press is a DOM
event and the moves arrive through a subscription — so the messages are applied
directly, in the order a pointer would produce them.

-}
suite : Test
suite =
    describe "Ui.Resizer"
        [ describe "a panel resized from its trailing edge"
            [ test "follows the pointer by its delta, not to its position" <|
                \_ ->
                    -- Grabbed at 260 while the edge is at 240: the edge is 20
                    -- behind the pointer and stays 20 behind it. Jumping the
                    -- edge to the pointer on the first move is the thing that
                    -- makes a resize handle feel cheap.
                    navigation
                        |> apply [ Resizer.dragFrom 260, Resizer.dragTo 300, Resizer.dragDone ]
                        |> Resizer.widthPx
                        |> Expect.within (Expect.Absolute 0.01) 280
            , test "stops at the maximum" <|
                \_ ->
                    navigation
                        |> apply [ Resizer.dragFrom 240, Resizer.dragTo 2000 ]
                        |> Resizer.widthPx
                        |> Expect.within (Expect.Absolute 0.01) 420
            , test "collapses when dragged past the minimum" <|
                \_ ->
                    navigation
                        |> apply [ Resizer.dragFrom 240, Resizer.dragTo 20, Resizer.dragDone ]
                        |> Resizer.isCollapsed
                        |> Expect.equal True
            , test "dragging back out uncollapses it" <|
                \_ ->
                    navigation
                        |> apply [ Resizer.dragFrom 240, Resizer.dragTo 20, Resizer.dragTo 300 ]
                        |> Expect.all
                            [ Resizer.isCollapsed >> Expect.equal False
                            , Resizer.widthPx >> Expect.within (Expect.Absolute 0.01) 300
                            ]
            , test "the width survives a collapse and comes back with it" <|
                \_ ->
                    -- Collapsing is not a resize. Someone who set 380 and then
                    -- collapsed to get the room back wants 380 again, not the
                    -- default.
                    navigation
                        |> apply
                            [ Resizer.setWidth 380
                            , Resizer.setCollapsed True
                            , Resizer.setCollapsed False
                            ]
                        |> Resizer.widthPx
                        |> Expect.within (Expect.Absolute 0.01) 380
            , test "is only dragging while the pointer is down" <|
                \_ ->
                    navigation
                        |> apply [ Resizer.dragFrom 240, Resizer.dragTo 300 ]
                        |> Expect.all
                            [ Resizer.isDragging >> Expect.equal True
                            , Resizer.update Resizer.dragDone >> Resizer.isDragging >> Expect.equal False
                            ]
            , test "Escape puts back the width the drag started from" <|
                \_ ->
                    navigation
                        |> apply [ Resizer.dragFrom 240, Resizer.dragTo 400 ]
                        |> Resizer.update (Resizer.setWidth 240)
                        |> Resizer.widthPx
                        |> Expect.within (Expect.Absolute 0.01) 240
            ]
        , describe "a panel resized from its leading edge"
            [ test "grows as the pointer moves towards the start of the page" <|
                \_ ->
                    -- The mirror image, and the one that is easy to get
                    -- backwards: a panel at the end of the page gets *wider*
                    -- when its edge moves left.
                    panel
                        |> apply [ Resizer.dragFrom 800, Resizer.dragTo 740 ]
                        |> Resizer.widthPx
                        |> Expect.within (Expect.Absolute 0.01) 380
            , test "shrinks as the pointer moves towards the end of it" <|
                \_ ->
                    panel
                        |> apply [ Resizer.dragFrom 800, Resizer.dragTo 850 ]
                        |> Resizer.widthPx
                        |> Expect.within (Expect.Absolute 0.01) 270
            , test "never collapses, having nothing to collapse to" <|
                \_ ->
                    panel
                        |> apply [ Resizer.dragFrom 800, Resizer.dragTo 200, Resizer.dragDone ]
                        |> Expect.all
                            [ Resizer.isCollapsed >> Expect.equal False
                            , Resizer.widthPx >> Expect.within (Expect.Absolute 0.01) 520
                            ]
            , test "ignores a collapse it was never configured for" <|
                \_ ->
                    panel
                        |> Resizer.update (Resizer.setCollapsed True)
                        |> Resizer.isCollapsed
                        |> Expect.equal False
            ]
        , describe "the keyboard"
            [ test "nudges within the bounds" <|
                \_ ->
                    navigation
                        |> apply [ Resizer.nudgeBy 16, Resizer.nudgeBy 16 ]
                        |> Resizer.widthPx
                        |> Expect.within (Expect.Absolute 0.01) 272
            , test "widening a collapsed panel expands it rather than resizing it" <|
                \_ ->
                    navigation
                        |> apply [ Resizer.setCollapsed True, Resizer.nudgeBy 16 ]
                        |> Expect.all
                            [ Resizer.isCollapsed >> Expect.equal False
                            , Resizer.widthPx >> Expect.within (Expect.Absolute 0.01) 240
                            ]
            , test "narrowing a collapsed panel has nowhere to go" <|
                \_ ->
                    navigation
                        |> apply [ Resizer.setCollapsed True, Resizer.nudgeBy -16 ]
                        |> Resizer.isCollapsed
                        |> Expect.equal True
            ]
        , describe "restoring what the app stored"
            [ test "clamps a width from a release with different bounds" <|
                \_ ->
                    navigation
                        |> Resizer.restore { collapsed = False, width = Just 9000 }
                        |> Resizer.widthPx
                        |> Expect.within (Expect.Absolute 0.01) 420
            , test "falls back to the default when nothing was stored" <|
                \_ ->
                    navigation
                        |> Resizer.restore { collapsed = True, width = Nothing }
                        |> Expect.all
                            [ Resizer.widthPx >> Expect.within (Expect.Absolute 0.01) 240
                            , Resizer.isCollapsed >> Expect.equal True
                            ]
            , test "never restores mid-drag" <|
                \_ ->
                    navigation
                        |> apply [ Resizer.dragFrom 240, Resizer.dragTo 300 ]
                        |> Resizer.restore { collapsed = False, width = Just 300 }
                        |> Resizer.isDragging
                        |> Expect.equal False
            ]
        ]


{-| The navigation's own configuration, which is the one with a rail.
-}
navigation : Resizer.Model
navigation =
    Resizer.init
        { edge = Resizer.Trailing
        , label = "Resize navigation"
        , min = 180
        , max = 420
        , default = 240
        , collapse = Just { at = 132, rail = 56 }
        }


{-| The detail page's side panel: the other edge, and no collapsing.
-}
panel : Resizer.Model
panel =
    Resizer.init
        { edge = Resizer.Leading
        , label = "Resize panel"
        , min = 240
        , max = 520
        , default = 320
        , collapse = Nothing
        }


apply : List Resizer.Msg -> Resizer.Model -> Resizer.Model
apply messages model =
    List.foldl Resizer.update model messages
