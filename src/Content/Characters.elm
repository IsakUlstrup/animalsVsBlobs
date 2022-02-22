module Content.Characters exposing (..)

import Component exposing (Component)
import Components.Character


panda : ( Float, Float ) -> List Component
panda ( x, y ) =
    [ Component.characterComponent (Components.Character.newPassiveCharacter ( x, y ) True 20 2 (Just "🐼")) ]


dog : ( Float, Float ) -> List Component
dog ( x, y ) =
    [ Component.characterComponent (Components.Character.newPassiveCharacter ( x, y ) True 17 2.6 (Just "🐶")) ]


mouse : ( Float, Float ) -> List Component
mouse ( x, y ) =
    [ Component.characterComponent (Components.Character.newPassiveCharacter ( x, y ) True 15 3 (Just "🐭")) ]



-- elephant : ( Float, Float ) -> Character
-- elephant ( x, y ) =
--     Components.Character.newCharacter ( x, y ) True 6 0.005 (Just "🐘")
-- fox : ( Float, Float ) -> Character
-- fox ( x, y ) =
--     Components.Character.newPassiveCharacter ( x, y ) True 4 0.06 (Just "🦊")


blob : ( Float, Float ) -> List Component
blob ( x, y ) =
    [ Component.characterComponent (Components.Character.newCharacter ( x, y ) False 20 1 Nothing) ]
