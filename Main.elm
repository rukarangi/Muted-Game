import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard 
import Time exposing (..)
import Signal 
import Window
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)

--Input

type alias Input =
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    , mouse : (Int, Int)
    , delta : Time
    , obiY : Float
    , obiX : Float
    , obiVY : Float
    , obiVX : Float
    }

delta : Signal Time
delta =
    Signal.map inSeconds (fps 35)

input : Signal Input
input = 
    Signal.sampleOn delta <|
        Signal.map10 Input
            Keyboard.up
            Keyboard.down
            Keyboard.left
            Keyboard.right
            (if Mouse.isDown
                Mouse.position)
            delta

--Model

(gameWidth,gameHeight) = (Window.width,Window.height)

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

type alias Level = 
    { levelNo : Int } 

type Association = Good | Bad

type Existance = NonExist | Exist

type State = Menu | Play | Over

obi : Character a
obi = 
    { x = 0.00
    , y = 0.00
    , vx = 0.00
    , vy = 0.00
    , exist = NonExist
    , life = 5
    , association = Good
    }

type alias Game = 
    { state : State
    , level : Level
    , obi : Character
    }

initalGame : Game
initalGame =
    { state : Menu
    , level : 0
    , obi : Obi 
    }

--Update 

inRange : Character a -> Character a -> Bool
inRange ({x,y,vx,vy,exist,life,association} as plr) ({x,y,vx,vy,exist,life,association} as enemy) =
    let
        yRange = 
            abs(plr.y - enemy.y) < 50    

        xRange =
            abs(plr.x - enemy.x) < 50
    in
        yRange && xRange  

updateCharacterPosition : Time -> Character a -> Character a
updateCharacterPosition t ({x,y,vx,vy,exist,life,association} as cha) =
    { cha | 
        x = x + vx * t,
        x = x + vx * t
    }

updateCharacterLife : Bool -> Time -> Character a -> Character a
updateCharacterLife t obi =
    if inRange
        { obi | life = life - 1 }

