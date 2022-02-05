module GameData exposing (GameMsg(..), GameScene, GameSystem)

import Component exposing (Component)
import Components.Skill exposing (SkillEffect)
import Ecs exposing (EcsId)


type alias GameSystem =
    Ecs.System Component GameMsg


type alias GameScene =
    Ecs.Scene Component GameMsg


type GameMsg
    = GameTick Float
      -- (skill parent part id, skill index) (target component id, skill effect)
    | UseSkill ( EcsId, Int ) ( EcsId, SkillEffect )
    | SetSkillTarget EcsId
