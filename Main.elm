import Color exposing (..)
import Keyboard 
import Time exposing (..)
import Platform exposing (..)
import Window
import Task
import Html exposing (Html)
import Svg exposing (..)
import Svg.Events exposing (onClick)
import Svg.Attributes exposing (..)

main = Html.program
  { init          = init
  , update        = update
  , subscriptions = subscriptions
  , view          = view
  } 

subscriptions : Model -> Sub Msg
subscriptions model = Time.every  (180 * millisecond) Tick


view : Model -> Html Msg
view model = 
  let w = toString model.gameWidth
      h = toString model.gameHeight
  in svg
      [ width w, height h, viewBox "0 0 100 100" ]
      [ text_ [x "1", y "50", fill "red"] [ text (toString model.time)] ]

-- --Input
-- type alias Input =
--     { up : Bool
--     , down : Bool
--     , left : Bool
--     , right : Bool
--     , mouse : (Int, Int)
--     , delta : Time
--     , obiY : Float
--     , obiX : Float
--     , obiVY : Float
--     , obiVX : Float
--     }
-- 
-- delta : Signal Time
-- delta =
--     Signal.map inSeconds (fps 35)
-- 
-- input : Signal Input
-- input = 
--     Signal.sampleOn delta <|
--         Signal.map10 Input
--             Keyboard.up
--             Keyboard.down
--             Keyboard.left
--             Keyboard.right
--             (if Mouse.isDown
--                 Mouse.position)
--             delta
-- 
--Model

type alias Character a = 
    { a |
      x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , exist : Existance
    , life : Int
    , association : Association
    }  

type alias Player =
    Character { score : Int }

type alias Level =  Int

type Association = Good | Bad

type Existance = NonExist | Exist

type State = Menu | Play | Over

obi : Character {}
obi = 
    { x = 0.00
    , y = 0.00
    , vx = 0.00
    , vy = 0.00
    , exist = NonExist
    , life = 5
    , association = Good
    }

type alias Model = 
    { state : State
    , level : Level
    , time : Time
    , gameWidth : Int
    , gameHeight : Int
    , obi : Character {}
    }

init : ( Model, Cmd Msg)
init = (
    { state = Menu
    , level = 0
    , time = 0
    , gameWidth = 0
    , gameHeight = 0
    , obi   = obi
    }, Task.perform WindowSize Window.size)

type Msg =
    Tick Time
    | WindowSize Window.Size

--Update 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time -> ( {model | time = time }, Cmd.none)
        WindowSize size -> ( { model | gameWidth = size.width
                                     , gameHeight = size.height }, Cmd.none)


-- inRange : Character a -> Character a -> Bool
-- inRange ({x,y,vx,vy,exist,life,association} as plr) ({x,y,vx,vy,exist,life,association} as enemy) =
--     let
--         yRange = 
--             abs(plr.y - enemy.y) < 50    
-- 
--         xRange =
--             abs(plr.x - enemy.x) < 50
--     in
--         yRange && xRange  
-- 
-- updateCharacterPosition : Time -> Character a -> Character a
-- updateCharacterPosition t ({x,y,vx,vy,exist,life,association} as cha) =
--     { cha | 
--         x = x + vx * t,
--         x = x + vx * t
--     }
-- 
-- updateCharacterLife : Bool -> Time -> Character a -> Character a
-- updateCharacterLife t obi =
--     if inRange
--         { obi | life = life - 1 }
-- 
