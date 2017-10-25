import Color exposing (..)
import Keyboard exposing (downs)
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

--Subscrptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch 
    [ Time.every  (200 * millisecond) Tick
    , Keyboard.downs DownsInfo
    ]

--Veiw

view : Model -> Html Msg
view model = 
  case model.state of
    Menu -> menuView
    Play -> playView
    Over -> overView

menuView : Model -> Msg
menuView model =
    let w = toString model.gameWidth
        h = toString model.gameHeight
    in svg
      [ width w, height h, viewBox ("0 0 " ++ w ++ " " ++ h ++ "")]
      [ image [ xlinkHref "/static/buttonPlaceholder.jpg", width "386", height "131", x 300, y 300][]
      ]

playView : Model -> Html Msg
playView model = 
    let w = toString model.gameWidth
        h = toString model.gameHeight
    in svg
      [ width w, height h, viewBox ("0 0 " ++ w ++ " " ++ h ++ "")]
      [ image [ xlinkHref "/static/Final Muted hero.png", width "386", height "131", x (toString model.obi.x), y (toString model.obi.y)][]
      ]

overView : Model -> Html Msg
overView model =
    let w = toString model.gameWidth
        h = toString model.gameHeight
    in svg
      [ width w, height h, viewBox ("0 0 " ++ w ++ " " ++ h ++ "")]
      [ image [ xlinkHref "/static/buttonPlaceholder.jpg", width "386", height "131", x 300, y 300][]
      ]

--viewms : Model -> Html Msg

walkLeft : Int -> Model -> Model
walkLeft kc model =
    let obi = model.obi    
    in  if kc == 65 then     
            let nobi = { obi | vx = obi.vx - 10 }
            in  { model | obi = nobi }
        else
            model

walkRight : Int -> Model -> Model
walkRight kc model =
    let obi = model.obi    
    in  if kc == 68 then     
            let nobi = { obi | vx = obi.vx + 10 }
            in  { model | obi = nobi }
        else
            model

jump : Int -> Model -> Model
jump kc model = 
    let obi = model.obi    
    in  if kc == 87 then
            if obi.vy == 0 then     
                let nobi = { obi | vx = obi.vy + 10 }
                in  { model | obi = nobi }
            else
                model
        else
            model



--Update 

characterUpdate : Bool -> Model -> Character -> Character
characterUpdate standingUpdate model obi =
    { obi | x = obi.x + obi.vx
            , y = obi.y + obi.vy
            , vy = if standingUpdate == True then
                        0
                   else
                        obi.vy + model.gravity
            , vx = obi.vx }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time -> 
            let obi = model.obi
                standingUpdate = if obi.y > 400 then
                                    True
                                 else
                                    False
                nobi = characterUpdate standingUpdate model obi
            in  ( { model | time = time, obi = nobi }, Cmd.none)
        WindowSize size -> ( { model | gameWidth = size.width
                                     , gameHeight = size.height }, Cmd.none)
        DownsInfo kc -> 
            (model |> jump kc |> walkLeft kc |> walkRight kc, Cmd.none)

{-          
actionCheck : Msg -> Model -> Model
actionCheck msg model = 
    msg
        |> jump msg model 
        |> walkRight msg model 
        |> walkLeft msg model 

-}
--Model

type alias Character = 
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , exist : Existance
    , life : Int
    , standing : Bool
    , association : Association
    }  


type alias Level =  Int

type Association = Good | Bad

type Existance = NonExist | Exist

type State = Menu | Play | Over

obi : Character
obi = 
    { x = 0.00
    , y = 0.00
    , vx = 0.00
    , vy = 0.00
    , exist = NonExist
    , life = 5
    , standing = True
    , association = Good
    }

type alias Model = 
    { state : State
    , level : Level
    , time : Time
    , gameWidth : Int
    , gameHeight : Int
    , obi : Character 
    , gravity : Float
    }

init : ( Model, Cmd Msg)
init = (
    { state = Menu
    , level = 0
    , time = 0
    , gameWidth = 0
    , gameHeight = 0
    , obi   = obi
    , gravity = 0.2
    }, Task.perform WindowSize Window.size)

type Msg 
  = Tick Time
  | WindowSize Window.Size
  | DownsInfo Keyboard.KeyCode




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
