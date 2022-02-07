module Content.Characters exposing (..)

import Components.Character exposing (Character)


panda : ( Float, Float ) -> Character
panda ( x, y ) =
    Components.Character.newCharacter ( x, y ) True 4 0.009 (Just "🐼")


dog : ( Float, Float ) -> Character
dog ( x, y ) =
    Components.Character.newCharacter ( x, y ) True 3 0.01 (Just "🐶")


mouse : ( Float, Float ) -> Character
mouse ( x, y ) =
    Components.Character.newCharacter ( x, y ) True 3 0.05 (Just "🐭")


elephant : ( Float, Float ) -> Character
elephant ( x, y ) =
    Components.Character.newCharacter ( x, y ) True 5 0.005 (Just "🐘")


blob : ( Float, Float ) -> Character
blob ( x, y ) =
    Components.Character.newCharacter ( x, y ) False 3 0.007 Nothing
