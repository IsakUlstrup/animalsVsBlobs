module Content.Characters exposing (..)

import Component exposing (Component)
import Components.Character


panda : ( Float, Float ) -> List Component
panda ( x, y ) =
    -- [ Component.characterComponent (Components.Character.newCharacter ( x, y ) True 20 0.7 (Just "🐼")) ]
    [ Component.characterComponent
        (Components.Character.init ( x, y )
            |> Components.Character.withPlayerFlag True
            |> Components.Character.withAppearance (Just "🐼")
            |> Components.Character.withPreferredDistance 200
            |> Components.Character.withVisionRange 500
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



-- elephant : ( Float, Float ) -> Character
-- elephant ( x, y ) =
--     Components.Character.newCharacter ( x, y ) True 6 0.005 (Just "🐘")
-- fox : ( Float, Float ) -> Character
-- fox ( x, y ) =
--     Components.Character.newPassiveCharacter ( x, y ) True 4 0.06 (Just "🦊")


blob : ( Float, Float ) -> List Component
blob ( x, y ) =
    [ Component.characterComponent
        (Components.Character.init ( x, y )
            |> Components.Character.withPreferredDistance 0
            |> Components.Character.withVisionRange 9999
            |> Components.Character.withMoveSpeed 0.2
        )
    ]
