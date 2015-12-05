module ColourPickerM  where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (..)
import Signal exposing (..)
import Color exposing (..)
import String exposing (slice)
import Text exposing (..)
import Mouse
import Time exposing (..)

------------------------------------------------------------------------------------------
{--MODEL--}

type alias State = { h:Float -- hue
                   , s:Float -- saturation
                   , l:Float -- lightness
                   , sx:Float -- slider x pos
                   , sy:Float -- slider y pos
                   , cx:Float -- selectorCircle x pos
                   , cy:Float -- selectorCircle y pos
                   }

init : State
init = { h = 0, s = 0, l = 0.5, sx = 0, sy = 0, cx = 0, cy = 0}--, counter = 0, timeLimit = 10}
--       , task = Incomplete}

------------------------------------------------------------------------------------------
{--UPDATE--}

changeColor x' y' m = let x = toFloat x' - 170
                          y =  ((toFloat y') - 525) * (-1)
                      in  if | radius (x,y) <= 100 -> {m | h <- hue x y, s <- sat x y, cx <- x, cy <- y}
                             | (x >= 178 && x <= 198) && (y<=101 && y>=(-101)) -> {m | l <- light x y, sy <- y}
                             | otherwise -> m

--resetTimer m = {m | counter <- 0}

--incrementTimer m = {m | counter <- m.counter + 1}

--finishTask m  = {m| task <- Done}

update : Update -> State -> State
update lastPos m = 
    case lastPos of
        Mouse (x,y) -> changeColor x y m
        _ -> m
--        Click bool -> resetTimer m
--        Run time -> incrementTimer m
--        Task Done -> finishTask m

radius (x,y) = sqrt ((( x)^2) + (( y)^2))
hue x y = atan2 ( y) ( x)
sat x y = radius (x,y)/100
light x y = (( y)+100)/200  

------------------------------------------------------------------------------------------
{--DISPLAY--}

--display : State -> Element
--display m input = 
view m = let
                lightnessBar = 
                    container 75 212 middle
                        ( flow outward
                              [ collage 30 212  (List.map shape2 lst2) 
                                  |> clickable (Signal.message clickArea.address Bar)
                              , container 30 212 middle (sliderShape m) 
                              ]  
                        ) 
                lst2 = [0..100]
                shape2 l = (filled (hsl (m.h) m.s (l/100))) (rect 20 2) |> move(0,-100+(2*l))
            in
            flow right
                   -- colour wheel + lightness
                   [ container 300 314 middle (colourWheelDisplay m 300 314)
                     -- |> Graphics.Element.color lightGrey
                      |> clickable (Signal.message clickArea.address ColourWheel)
                   -- ((List.map shape lst)
                   , lightnessDisplay lightnessBar
                   , infoDisplay m --|> Graphics.Element.color lightGrey
                   ]

infoDisplay m = flow down
                        [ hslDisplay m
                        , hslCode 270 20 m --|> Graphics.Element.color green
                        , flow outward
                            [ finalColour_image
                                |> container (220) (147) midRight
                            ]    
                        ]

displayInput input m = case input of
                              Mouse (x,y) -> show (x,y)
                              Clickable object -> ( case object of
                                                        ColourWheel -> show "ColourWheel"
                                                        Bar -> show "Bar"
                                                  )
                              Run time -> show m.counter
                              _ -> show "Hi"                        

colourWheelDisplay m w h = flow outward 
                        [ container w h midTop colourWheel_image
                        , container w h bottomRight select_image
                        , container w h midTop (selectorCircle m)
                        , container w h bottomLeft hslLegend 
                        ] 

lightnessDisplay lightnessBar 
                  = flow right
                      [ lightnessBar
                      , adjust_image
                      ]

-- additional text 
hslLegend = flow down 
                [ centered (fromString "Saturation = % of radius\nHue = angle (in degrees)") --|> move (0,-125)
                ]

instruction = text <| fromString <| "Copy and paste the following code:"

adjust_image = image 101 143 "http://i.imgur.com/o0uqK9O.png"
select_image = image 121 110 "http://i.imgur.com/l8u28ip.png"
finalColour_image = image 127 147 "http://i.imgur.com/7E3HoKw.png"

colourWheel_image = image 204 204 "http://i.imgur.com/AyFNQiG.png"
                        

x h s = s * (cos (degrees h))
y h s = s * (sin (degrees h))

-- Slider and Circle
sliderShape m = collage 30 212 
                  [outlined (solid black) (rect 30 10)
                      --|> move (-235,-145)   
                      |> move (m.sx,m.sy)
                  ]
selectorCircle m = collage 204 204 -- (hsl (m.h+pi) m.s 0.75)
                    [ outlined (solid black) (circle 5) |> move (m.cx,m.cy)
                    , outlined (solid black) <| segment (0,0) (m.cx,m.cy)
                    ]

hslDisplay m = container 200 100 middle
                              ( flow right 
                                    [ colourDisplayHSL m-- |> move (-250,-65)         
                                        , container 100 65 midTop 
                                            (flow down 
                                                [ hueInfo m-- |> move (-195,-50)
                                                , satInfo m--|> move (-195,-65)                    
                                                , lightInfo m
                                                ]
                                            )
                                    
                                    ]
                              )

colourDisplayHSL m = collage 50 50 [square 50 |> filled (hsl (m.h) (m.s) (m.l))]
                  




-- format values that are to be displayed
hue' m = toString <| round <| if | m.h < 0 -> ((m.h+(2*pi))*180/pi) 
                                 | otherwise -> (m.h*180/pi) 
sat' m =  (slice 0 4 (toString m.s))
satPercent m = (toString (round (m.s*100))) ++ "%"
light' m =  (slice 0 4 (toString m.l))
lightPercent m = (toString (round (m.l*100))) ++ "%"
                    
-- display hue, sat, light values                                    
hueInfo m = centered <| Text.height 14 <| fromString <| "HUE: " ++ (hue' m) -- |>  scale 0.9
satInfo m = centered <| Text.height 14 <| fromString <| "SAT: " ++ (satPercent m)-- |>  scale 0.9
lightInfo m = centered <| Text.height 14 <| fromString <| "LIGHT: " ++ (lightPercent m) -- |>  scale 0.9
hslCode w h m = container w h middle (centered <| Text.height 16 <|monospace <| fromString <| hslCodeString m)

hslCodeString m = "(hsl (degrees "  ++ (hue' m) ++ ")" ++ " " ++ (sat' m) ++ " " ++ (light' m) ++ ")"

actualCode m = hsl (actualHue m) (actualSat m) (actualLight m)
actualHue m = m.h
actualSat m = m.s
actualLight m = m.l

------------------------------------------------------------------------------------------
{--MAIN--}

type Update = Mouse (Int,Int) | Click Bool | Clickable Object | Run Time --| Task TaskStatus

type Object = ColourWheel | Bar --| None

clickArea : Signal.Mailbox Object
clickArea = Signal.mailbox ColourWheel

--type TaskStatus = Done | Incomplete

lastPos : Signal Update
lastPos = Signal.sampleOn (Signal.map Clickable clickArea.signal) (Signal.map Mouse Mouse.position)

input : Signal Update
input = Signal.mergeMany [lastPos
--                         , (Signal.map Click Mouse.isDown)
--                         , (Signal.map Task clickDone.signal)
--                         , (Signal.map Run (every second))
                         ]

