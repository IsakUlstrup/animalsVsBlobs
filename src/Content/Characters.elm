module Content.Characters exposing (..)

import Component exposing (Component)
import Components.Character
import Components.Range


panda : ( Float, Float ) -> List Component
panda ( x, y ) =
    -- [ Component.characterComponent (Components.Character.newCharacter ( x, y ) True 20 0.7 (Just "🐼")) ]
    [ Component.characterComponent
        (Components.Character.init ( x, y )
            |> Components.Character.withPlayerFlag True
            |> Components.Character.withAppearance (Just "🐼")
            |> Components.Character.withPreferredDistance 200
            |> Components.Character.withVisionRange 500
            |> Components.Character.withSkill (Just Components.Character.testSkill)
        )
    ]


dog : ( Float, Float ) -> List Component
dog ( x, y ) =
    -- [ Component.characterComponent (Components.Character.newCharacter ( x, y ) True 17 0.9 (Just "🐶")) ]
    [ Component.characterComponent
        (Components.Character.init ( x, y )
            |> Components.Character.withPlayerFlag True
            |> Components.Character.withAppearance (Just "🐶")
            |> Components.Character.withPreferredDistance 200
            |> Components.Character.withVisionRange 500
        )
    ]


mouse : ( Float, Float ) -> List Component
mouse ( x, y ) =
    -- [ Component.characterComponent (Components.Character.newCharacter ( x, y ) True 15 1.5 (Just "🐭")) ]
    [ Component.characterComponent
        (Components.Character.init ( x, y )
            |> Components.Character.withPlayerFlag True
            |> Components.Character.withAppearance (Just "🐭")
            |> Components.Character.withPreferredDistance 200
            |> Components.Character.withVisionRange 500
        )
    ]


rocket : ( Float, Float ) -> List Component
rocket ( x, y ) =
    -- [ Component.characterComponent (Components.Character.newCharacter ( x, y ) True 15 1.5 (Just "🐭")) ]
    [ Component.characterComponent
        (Components.Character.init ( x, y )
            |> Components.Character.withPlayerFlag True
            |> Components.Character.withAppearance (Just "🚀")
            |> Components.Character.withPreferredDistance 0
            |> Components.Character.withVisionRange 500
            |> Components.Character.withDamage 500
            |> Components.Character.withMoveSpeed 2
            |> Components.Character.withHealth (Components.Range.newRange 0 1 1)
        )
    ]


blob : ( Float, Float ) -> List Component
blob ( x, y ) =
    [ Component.characterComponent
        (Components.Character.init ( x, y )
            |> Components.Character.withPreferredDistance 0
            |> Components.Character.withVisionRange 9999
            |> Components.Character.withMoveSpeed 0.2
        )
    ]
