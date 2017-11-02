import Color exposing (..)
import Keyboard exposing (downs)
import Time exposing (..)
import Platform exposing (..)
import Window
import Task
import Random
import Array exposing (..)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Events exposing (onClick)
import Svg.Attributes exposing (..)
import Keyboard.Extra as KB

main = Html.program
  { init          = init
  , update        = update
  , subscriptions = subscriptions
  , view          = view
  } 

--Subscrptions

subscriptions : Model -> Sub Msg
subscriptions model =  
    let
        obi = model.obi   
    in  Sub.batch
        [ Time.every (1/5 * millisecond) Tick
        , Sub.map DownsInfo KB.subscriptions
        ]

--Veiw

view : Model -> Html Msg
view model = 
  case model.state of
    Menu -> menuView model
    Play -> playView model
    Over -> overView model

menuView : Model -> Html Msg
menuView model =
    let w = model.gameWidth
        h = model.gameHeight
        buttonX = w / 2 - 193
        buttonY = h / 2 - 65
    in svg
      [ width (toString (w*0.9)), height (toString (h*0.9))
      , viewBox ("0 0 " ++ (toString w) ++ " " ++ (toString h) ++ "")]
      [ image [ xlinkHref "static/buttonPlaceholder.jpg"
      , width "386", height "131"
      , x (toString buttonX)
      , y (toString (buttonY + 200))
      , Svg.Events.onClick StatePlay]
      []
      ]

enemieRender : Character -> Svg Msg
enemieRender character = svg [] [
    image [ xlinkHref "static/OBI-running.png"
              , width "100", height "100"
              , x (toString (if character.exist == Exist then character.x else -500))
              , y (toString (if character.exist == Exist then character.y else -500))][]]

platformRender : Platform -> Svg Msg
platformRender pl =
    image [ xlinkHref "static/placeholderplatform.png"
          , width  (toString  pl.width)
          , height (toString (96 * pl.width / 426)) 
          , x (toString (pl.x - (pl.width/2)))
          , y (toString (pl.y + (96 * pl.width / 426))) ][]

playView : Model -> Html Msg
playView model = 
    let enemie = List.map enemieRender (Array.toList model.ai)
        plts = List.map platformRender model.platforms
        w = model.gameWidth
        h = model.gameHeight
    in svg
      [ width (toString (w*0.9)), height (toString (h*0.9))
      , viewBox ("0 0 " ++ (toString w) ++ " " ++ (toString h) ++ "")]
      (enemie ++ plts ++
      [ image [ xlinkHref "static/OBI-running.gif"
              , width "100", height "100"
              , x (toString (if model.obi.exist == Exist then model.obi.x else -500))
              , y (toString (if model.obi.exist == Exist then model.obi.y else -500))][]
      ])

overView : Model -> Html Msg
overView model =
    let w = toString model.gameWidth
        h = toString model.gameHeight
    in svg
      [ width (toString w), height (toString h)
      , viewBox ("0 0 " ++ (toString w) ++ " " ++ (toString h) ++ "")]
      [ image [ xlinkHref "static/buttonPlaceholder.jpg"
              , width "386", height "131", x "300", y "300"
              , Svg.Events.onClick StateMenu][]
      , image [ xlinkHref "static/buttonPlaceholder.jpg"
              , width "386", height "131", x "300", y "300"
              , Svg.Events.onClick StatePlay][]
      ]

--Update 

hitCheck : Array Bool -> Bool
hitCheck comparisons =
    List.member True (Array.toList comparisons)

checkRange : Model -> Character -> Bool
checkRange model enemy = 
    let
        obi = model.obi   
    in
        if abs (obi.y - enemy.y) <= 50 && abs (obi.x - enemy.x) <= 50 then
            True
        else
            model.obiInRange

max_x_speed = 3
base_gravity = 0.05

characterUpdate : Model -> Character -> Character
characterUpdate model character =
    let obi = model.obi
        {-
        timeSLowY = if character.good then
                        abs character.vy - (abs character.vy + (abs character.vy / 5))
                    else
                        obi.timeSLowY
        timeSLowX = if character.good then
                        abs character.vx - (abs character.vx + (abs character.vx / 5))
                    else
                        obi.timeSLowX
        -}
        move = if character.good then
                    KB.wasd model.keys
               else
                    character.aiMove
        checkPlatform ch pl =  abs ((ch.x + 50) - pl.x) < pl.width/2  -- not further than half width away from centre
                            && abs(ch.y + 30 - pl.y) < 5              -- within one pixel of the height
                            && ch.vy >= 0                             -- moving down or not moving up or down (not up)
        onPlatform = List.any (checkPlatform character) model.platforms
        onGround = character.y >= model.ground
        dvy =   if onGround || onPlatform then
                    toFloat move.y * model.jump_speed
                else 
                    0.0
        nvy =   if onGround || onPlatform then
                    if character.vy > 0 then   -- If going down, and you hit the ground or a platform, then stop
                            0.0
                        else 
                            character.vy + dvy
                else
                    character.vy + dvy + character.gravity
        nvxd = character.vx + (toFloat move.x) * 0.1
        nvx = if abs nvxd > max_x_speed 
                then character.vx 
                else nvxd

    in  if character.exist == Exist then 
            { character 
            | x = Basics.min (model.gameWidth) (Basics.max 0.0 (character.x + character.vx))
            , y = Basics.min model.ground (character.y + nvy)
            , vy = nvy
            , action = if character.vy /= 0 then
                            Jump
                       else StandingStill
            , gravity = base_gravity
            , exist =   if character.life == 0 then
                            NonExist
                        else 
                            Exist
            , life = if character.good then
                        if model.obiInRange then
                            Basics.max (character.life - 1) 0
                        else
                            character.life
                     else
                        character.life
            , vx = if abs nvx <= 0.01 || abs ((model.gameWidth - 100) - nvx) <= 0.01  then
                        0
                    else
                        if onGround || onPlatform then
                            if abs nvx > 0.01 then
                                nvx * 0.95
                            else if abs nvx <= 0.01 then
                                0
                            else
                                nvx
                        else
                        nvx 
            , area = if character.x < (model.gameWidth / 2) && character.y < (model.gameHeight / 2) then
                        TopLeft
                     else if character.x < (model.gameWidth / 2) && character.y > (model.gameHeight / 2) then
                        BottomLeft
                     else if character.x > (model.gameWidth / 2) && character.y < (model.gameHeight / 2) then
                        TopRight
                     else if character.x > (model.gameWidth / 2) && character.y > (model.gameHeight / 2) then
                        BottomRight
                     else
                        TopLeft}
        else
            character

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        EnemyNo enemies ->
            let ai = model.ai    
            in ({ model | ai = Array.map (initiateEnemy enemies) ai}, Cmd.none)
        StateOver ->   
            ({ model | state = Over}, Cmd.none)
        StateMenu ->
            ({ model | state = Menu}, Cmd.none)
        StatePlay ->
            let
                obi = model.obi
                nobi = { obi | exist = Exist}
                nai = model.ai
            in
                ({ model | state = Play
                 , level = 1
                 , ai = nai
                 , obi = nobi}
                 , Cmd.none)
        Tick time -> 
            let obi = model.obi
                ai = model.ai
                nai = (Array.map (characterUpdate model << pathfind model obi) ai) 
                nobi = characterUpdate model obi
            in  ( { model | obiInRange = (hitCheck (Array.map (checkRange model) ai))  
                          , time = time
                          , obi = nobi
                          , ai = nai }
                          , Random.generate EnemyNo (Random.int 1 4))
        WindowSize size -> 
            let w = toFloat size.width
                h = toFloat size.height
            in { model | gameWidth = w
                       , gameHeight = h
                       , ground = 0.85 * h
                       , jump_speed = -0.006 * h 
                       , platforms = makePlatforms w h } ! []
        DownsInfo kbmsg -> 
            let keys = model.keys
            in ({ model | keys = KB.update kbmsg keys }, Cmd.none ) 

pathfind : Model -> Character -> Character -> Character
pathfind model obi character =
    let
        sameArea = obi.area == character.area
        areaFind = if obi.x > character.x then
                        { y = 0, x = 1}
                   else if obi.x < character.x then
                        { y = 0, x = -1}
                   else if obi.x == character.x then
                        { y = 1, x = 0}
                   else
                        { y = 0, x = 0}
        findArea = if obi.area == TopLeft && character.area == BottomLeft then
                        if character.x > model.gameWidth - 50 then
                            { y = 1, x = 1}
                        else
                            { y = 0, x = 1}
                   else if obi.area == BottomRight && character.area == BottomLeft then
                        { y = 0, x = 1}
                   else if obi.area == TopRight && character.area == BottomLeft then
                        if character.x > model.gameWidth - 50 then
                            { y = 1, x = 1}
                        else
                            { y = 0, x = 1}
                   else if obi.area == TopLeft && character.area == BottomRight then
                        if character.x < model.gameWidth + 50 then
                            { y = 1, x = -1}
                        else
                            { y = 0, x = -1}
                   else if obi.area == BottomLeft && character.area == BottomRight then
                        { y = 0, x = -1}
                   else if obi.area == TopRight && character.area == BottomRight then
                        if character.x < model.gameWidth + 50 then
                            { y = 1, x = -1}
                        else
                            { y = 0, x = -1}
                   else if obi.area == BottomLeft && character.area == TopLeft then
                        if character.x > model.gameWidth - 50 then
                            { y = -1, x = 0}
                        else
                            { y = -1, x = 1}
                   else if obi.area == BottomRight && character.area == TopLeft then
                        if character.x < model.gameWidth + 50 then
                            { y = 1, x = -1}
                        else
                            { y = 0, x = -1}
                   else if obi.area == TopRight && character.area == TopLeft then
                        if character.x > model.gameWidth - 50 then
                            { y = -1, x = 1}
                        else
                            { y = 0, x = 1}
                   else if obi.area == TopLeft && character.area == TopRight then
                        if character.x < model.gameWidth - 50 then
                            { y = 1, x = 1}
                        else
                            { y = 1, x = 1}
                   else if obi.area == BottomRight && character.area == TopRight then
                        { y = 1, x = -1}
                   else if obi.area == BottomLeft && character.area == TopRight then
                        if character.x < model.gameWidth - 50 then
                            { y = -1, x = -1}
                        else
                            { y = 0, x = -1}
                   else
                        { y = 0, x = 0}
    in
        { character | aiMove = if sameArea then
                                    areaFind
                               else
                                    findArea
        }
            

initiateEnemy : Int -> Character -> Character
initiateEnemy areaNo character =
    { character | area = if character.areaSelect then
                            if areaNo == 1 then
                                TopLeft
                            else if areaNo == 2 then
                                TopRight
                            else if areaNo == 3 then
                                BottomRight
                             else if areaNo == 4 then
                                BottomLeft
                            else
                                TopLeft
                         else
                            TopLeft
                , areaSelect = False
                , x = if character.exist == NonExist then
                        if character.area == TopLeft || character.area == BottomLeft then
                                0
                        else if character.area == TopRight || character.area == BottomRight then
                                1500
                        else 
                                0
                      else
                        character.x
                , y = if character.exist == NonExist then 
                        if character.area == TopRight || character.area == TopLeft then
                                0
                        else if character.area == BottomRight || character.area == BottomLeft then
                                575
                        else
                                0
                      else
                        character.y
                , exist = Exist }
                
type alias Character = 
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , acceleration : Float
    , exist : Existance
    , life : Int
    , standing : Bool
    , action : Action
    , gravity : Float
    , area : Area
    , good : Bool
    , areaSelect : Bool
    , aiMove : AiMove
    , aiHit : Bool
    }  

type alias Platform =
    { x : Float 
    , y : Float
    , width : Float 
    }

-- Make the platforms after we already have the width and height of game
-- The width is to check if we have landed
makePlatforms : Float -> Float -> List Platform
makePlatforms w h = 
    [ { x = w * (9/10),  y = h * (1/4) - 100,  width = 300 } 
    , { x = w * (1/10),  y = h * (1/4) - 100,  width = 300 }
    , { x = w * (1/2),  y = h * (3/4) - 100, width = 300}
    , { x = w * (1/3), y = h * (7/16) - 50,   width = 300}
    , { x = w * (2/3), y = h * (7/16) - 50,   width = 300}
    ]

type alias AiMove =
    { y : Int, x : Int}

type alias Model = 
    { state : State
    , level : Level
    , time : Time
    , platforms : List Platform
    , gameWidth : Float
    , gameHeight : Float
    , ground : Float
    , jump_speed : Float
    , ai : Array Character
    , obi : Character
    , keys : List KB.Key
    , obiInRange : Bool
    }

type alias Level =  Int

type Area = TopLeft | TopRight | BottomLeft | BottomRight

type Action = Jump | Walk | StandingStill

type Association = Good | Bad

type Existance = NonExist | Exist

type State = Menu | Play | Over

standardAi : Character
standardAi = 
    { x = 0.00
    , y = 0.00
    , vx = 0.00
    , vy = 0.00
    , acceleration = 0.00
    , exist = NonExist
    , life = 5
    , standing = True
    , action = StandingStill
    , gravity = 0.1
    , area = BottomLeft
    , good = False
    , areaSelect = True
    , aiMove = { y = 0, x = 0}
    , aiHit = False
    }

aiMake : Array Character
aiMake = 
    Array.repeat 1 standardAi

obi : Character
obi = 
    { x = 850
    , y = 200
    , vx = 0.00
    , vy = 0.00
    , acceleration = 0.00
    , exist = NonExist
    , life = 5
    , standing = True
    , action = StandingStill
    , gravity = 0.1
    , area = TopLeft
    , good = True
    , areaSelect = False
    , aiMove = { y = 0, x = 0}
    , aiHit = False
    }

init : (Model, Cmd Msg)
init = (
    { state = Menu
    , level = 10
    , time = 0
    , platforms = []
    , gameWidth = 0
    , gameHeight = 0
    , jump_speed = -90
    , ground = 0
    , ai = aiMake 
    , obi = obi
    , keys = []
    , obiInRange = False
    }, Task.perform WindowSize Window.size)

type Msg 
  = Tick Time
  | WindowSize Window.Size
  | DownsInfo KB.Msg
  | StatePlay
  | StateMenu
  | StateOver
  | EnemyNo Int




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
