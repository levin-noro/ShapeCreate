module Transformer where
 
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input.Field exposing (..)
import Graphics.Input exposing (..)
import Text exposing (fromString, monospace)
import String exposing (toInt)
import Color exposing (..)
import Signal exposing (..)
import Result --exposing (Result)

type alias State = { width : Int, height : Int--width =  269, height = 346
                   , chooseMove : Bool, chooseRotate : Bool
                   , chooseScale : Bool, transformer : Bool
                   , horShift : Float, vertShift : Float, degrees : Float, scaleFactor : Float
                   , currTextbox : String, currTrans : String, transformCode : String
                   }

init : State 
init =  { width =  133, height = 130--width =  269, height = 346
        , chooseMove = False, chooseRotate = False
        , chooseScale = False, transformer = True
        , horShift = 0, vertShift = 0, degrees = 0, scaleFactor = 1
        , currTextbox = "", currTrans = "", transformCode = ""
        }

------------------------------------------------------------------------------------------
{--UPDATE--}

update input m = 
    case input of
      Button string -> changeState2 string m
      Typing content -> checkValue (String.toFloat (checkBox content.string m)) m

checkBox string m = if string == "" then "0" else string

changeState2 string m = if | string == "move" -> {m | transformCode <- " |> move (" ++ (toString m.horShift) ++ "," ++ (toString m.vertShift) ++ ")", chooseMove <-True, chooseRotate <- False, chooseScale <- False}
                           | string == "rotate" -> {m | transformCode <- " |> rotate (degrees " ++ (toString m.degrees) ++ ")", chooseMove <-False, chooseRotate <- True, chooseScale <- False}
                           | string == "scale" -> {m | transformCode <- " |> scale " ++ (toString m.scaleFactor),chooseMove <-False, chooseRotate <- False, chooseScale <- True}
                           | string == "horShift" -> {m | currTextbox <- "horShift"}
                           | string == "vertShift" -> {m | currTextbox <- "vertShift"}
                           | string == "degrees" -> {m | currTextbox <- "degrees"}
                           | string == "scaleFactor" -> {m | currTextbox <- "scaleFactor"}
                           | otherwise -> m

checkValue result m =
    case result of
      Ok value -> updateValue value m
      _ -> m

updateValue value m = if
       | m.currTextbox == "horShift" -> {m | horShift <- value, transformCode <- " |> move (" ++ (toString value) ++ "," ++ (toString m.vertShift) ++ ")"}
       | m.currTextbox == "vertShift" -> {m | vertShift <- value, transformCode <- " |> move (" ++ (toString m.horShift) ++ "," ++ (toString value) ++ ")"}
       | m.currTextbox == "degrees" -> {m | degrees <- value, transformCode <- " |> rotate (degrees " ++ (toString value) ++ ")"}
       | m.currTextbox == "scaleFactor" -> {m | scaleFactor <- value, transformCode <- " |> scale " ++ (toString value)}
       | otherwise -> m

------------------------------------------------------------------------------------------
{--VIEW--}

view m nf9 nf10 nf11 nf12 transSelect =
        container m.width m.height topLeft (transMenu m.width m.height nf9 nf10 nf11 nf12 m transSelect)
            

{--
transCollage w h nf9 nf10 nf11 nf12 m =         
        flow down
            [ flow right
                [ transMenu w h nf9 nf10 nf11 nf12 m
                , transMenu w h nf9 nf10 nf11 nf12 m
                ]
            , flow right
                [ transMenu w h nf9 nf10 nf11 nf12 m
                , transMenu w h nf9 nf10 nf11 nf12 m
                ]     
            ]
--}

transMenu w h nf9 nf10 nf11 nf12 m transSelect =
        let 
            collageWidth = 133
            buttonWidth = 120
            menuWidth = 130
        in
        -- 
        flow outward
            [ transMenuBackDrop collageWidth menuWidth
            , container collageWidth 130 (midTopAt (absolute (collageWidth//2)) (absolute 25)) 
                <| (transButtons buttonWidth nf9 nf10 nf11 nf12 m transSelect)
            ]
        --}
        
transMenuBackDrop collageWidth menuWidth = 
        collage collageWidth 130
            [ outlined (solid black) (rect menuWidth 100) 
            , outlined (solid black) (oval menuWidth 20) |> move (0,50)
            , outlined (solid black) (oval menuWidth 20) |> move (0,-50)
            , filled white (rect menuWidth 100)
            , filled white (oval menuWidth 20) |> move (0,50)
            , filled white (oval menuWidth 20) |> move (0,-50)
            , text (Text.color black (fromString "PICK ONE")) |> move (0,50)
            ]

transButtons buttonWidth nf9 nf10 nf11 nf12 m transSelect =
        flow down 
            [ moveButton buttonWidth nf9 nf10 m transSelect
                |> (if | transSelect == "transformer 1" -> clickable (Signal.message buttonMailbox.address "move")
                       | transSelect == "transformer 2" -> clickable (Signal.message buttonMailbox2.address "move")
                       | transSelect == "transformer 3" -> clickable (Signal.message buttonMailbox3.address "move")  
                       | otherwise -> clickable (Signal.message buttonMailbox4.address "move")
                   )    
            --, spacer 155 20    
            , rotateButton buttonWidth nf11 m transSelect
                |> (if | transSelect == "transformer 1" -> clickable (Signal.message buttonMailbox.address "rotate")
                       | transSelect == "transformer 2" -> clickable (Signal.message buttonMailbox2.address "rotate")
                       | transSelect == "transformer 3" -> clickable (Signal.message buttonMailbox3.address "rotate")  
                       | otherwise -> clickable (Signal.message buttonMailbox4.address "rotate")
                   )   
            , scaleButton buttonWidth nf12 m transSelect
                |> (if | transSelect == "transformer 1" -> clickable (Signal.message buttonMailbox.address "scale")
                       | transSelect == "transformer 2" -> clickable (Signal.message buttonMailbox2.address "scale") 
                       | transSelect == "transformer 3" -> clickable (Signal.message buttonMailbox3.address "scale")  
                       | otherwise -> clickable (Signal.message buttonMailbox4.address "scale")
                   )   
            ]
              
moveButton buttonWidth nf9 nf10 m transSelect =
        collage 150 30
            [ text <| Text.color black <| fromString "move (\t\t\t\t\t\t\t\t\t\t,\t\t\t\t\t\t\t\t\t\t)" 
            , if m.chooseMove then emptyForm else moveXtextbox nf9 m transSelect
            , if m.chooseMove then emptyForm else moveYtextbox nf10 m transSelect
            , toForm (transButOutline buttonWidth m.chooseMove)
            , if m.chooseMove then moveXtextbox nf9 m transSelect else emptyForm
            , if m.chooseMove then moveYtextbox nf10 m transSelect else emptyForm
            ]

moveXtextbox nf9 m transSelect = size 28 18 nf9
                   |> (if | transSelect == "transformer 1" -> clickable (Signal.message buttonMailbox.address "horShift")
                          | transSelect == "transformer 2" -> clickable (Signal.message buttonMailbox2.address "horShift")
                          | transSelect == "transformer 3" -> clickable (Signal.message buttonMailbox3.address "horShift")
                          | otherwise -> clickable (Signal.message buttonMailbox4.address "horShift")
                      )
                   |> toForm
                   |> move (0,0)
                   
moveYtextbox nf10 m transSelect = size 28 18 nf10
                   |> (if | transSelect == "transformer 1" -> clickable (Signal.message buttonMailbox.address "vertShift")
                          | transSelect == "transformer 2" -> clickable (Signal.message buttonMailbox2.address "vertShift")
                          | transSelect == "transformer 3" -> clickable (Signal.message buttonMailbox3.address "vertShift")
                          | otherwise -> clickable (Signal.message buttonMailbox4.address "vertShift")
                      )
                   |> toForm
                   |> move (33,0)
            
rotateButton buttonWidth nf11 m transSelect =
        collage 150 30
            [ text <| Text.color black <| fromString "rotate (degrees \t\t\t\t\t\t\t\t)" 
            , if m.chooseRotate then emptyForm else degreeTextbox nf11 m transSelect
            , toForm (transButOutline buttonWidth m.chooseRotate)  
            , if m.chooseRotate then degreeTextbox nf11 m transSelect else emptyForm
            ]

degreeTextbox nf11 m transSelect = size 25 18 nf11 
                        |> (if | transSelect == "transformer 1" -> clickable (Signal.message buttonMailbox.address "degrees")
                               | transSelect == "transformer 2" -> clickable (Signal.message buttonMailbox2.address "degrees")
                               | transSelect == "transformer 3" -> clickable (Signal.message buttonMailbox3.address "degrees")
                               | otherwise -> clickable (Signal.message buttonMailbox4.address "degrees")
                           )
                        |> toForm 
                        |> move (40,0)

scaleButton buttonWidth nf12 m transSelect =
        collage 150 30
            [ (text <| Text.color black <| fromString "scale")  |> move (-10,0)
            , if m.chooseScale then emptyForm else scaleTextbox nf12 m transSelect
            , toForm (transButOutline buttonWidth m.chooseScale)
            , if m.chooseScale then scaleTextbox nf12 m transSelect else emptyForm
            ]

scaleTextbox nf12 m transSelect = size 22 18 nf12
                      |> (if | transSelect == "transformer 1" -> clickable (Signal.message buttonMailbox.address "scaleFactor")
                             | transSelect == "transformer 2" -> clickable (Signal.message buttonMailbox2.address "scaleFactor")
                             | transSelect == "transformer 3" -> clickable (Signal.message buttonMailbox3.address "scaleFactor")
                             | otherwise -> clickable (Signal.message buttonMailbox4.address "scaleFactor")
                         )
                      |> toForm
                      |> move (20,0)

transButOutline buttonWidth select = if | select == True -> collage 175 40 [outlined (solid black) <| rect buttonWidth 25] -- |> clickable (Signal.message buttonMailbox.address "circle")
                                        | otherwise -> collage 175 40 [filled (hsla (degrees 0) 0 1 0.5) <| rect buttonWidth 25]  -- |> clickable (Signal.message buttonMailbox.address "circle")

emptyForm = filled white (circle 0)

-- TRANSFORMER 1

nf9 : Signal Element
nf9 = (field {textBoxStyle | style <- {textStyle | height <- Just 11}} (Signal.message name9.address) "x" <~ name9.signal)
nf10 : Signal Element
nf10 = (field textBoxStyle (Signal.message name10.address) "y" <~ name10.signal)
nf11 : Signal Element
nf11 = (field textBoxStyle (Signal.message name11.address) "" <~ name11.signal)
nf12 : Signal Element
nf12 = (field textBoxStyle (Signal.message name12.address) "" <~ name12.signal)

name9 : Signal.Mailbox Content
name9 = Signal.mailbox (Content (toString -195) (Selection 0 0 Forward))

name10 : Signal.Mailbox Content
name10 = Signal.mailbox (Content (toString 65) (Selection 0 0 Forward))

name11 : Signal.Mailbox Content
name11 = Signal.mailbox (Content (toString 0) (Selection 0 0 Forward))

name12 : Signal.Mailbox Content
name12 = Signal.mailbox (Content (toString 1) (Selection 0 0 Forward))

buttonMailbox : Signal.Mailbox String
buttonMailbox = Signal.mailbox "filled"

textboxMailbox  = Signal.mergeMany [ name9.signal
                                   , name10.signal
                                   , name11.signal
                                   , name12.signal
                                   ]

-- TRANSFORMER 2

nf13 : Signal Element
nf13 = (field {textBoxStyle | style <- {textStyle | height <- Just 11}} (Signal.message name13.address) "x" <~ name13.signal)
nf14 : Signal Element
nf14 = (field textBoxStyle (Signal.message name14.address) "y" <~ name14.signal)
nf15 : Signal Element
nf15 = (field textBoxStyle (Signal.message name15.address) "" <~ name15.signal)
nf16 : Signal Element
nf16 = (field textBoxStyle (Signal.message name16.address) "" <~ name16.signal)

name13 : Signal.Mailbox Content
name13 = Signal.mailbox (Content (toString 0) (Selection 0 0 Forward))

name14 : Signal.Mailbox Content
name14 = Signal.mailbox (Content (toString 0) (Selection 0 0 Forward))

name15 : Signal.Mailbox Content
name15 = Signal.mailbox (Content (toString 45) (Selection 0 0 Forward))

name16 : Signal.Mailbox Content
name16 = Signal.mailbox (Content (toString 2) (Selection 0 0 Forward))

textboxMailbox2  = Signal.mergeMany [ name13.signal
                                   , name14.signal
                                   , name15.signal
                                   , name16.signal
                                   ]                                   

buttonMailbox2 : Signal.Mailbox String
buttonMailbox2 = Signal.mailbox "filled"

-- TRANSFORMER 3

nf17 : Signal Element
nf17 = (field {textBoxStyle | style <- {textStyle | height <- Just 11}} (Signal.message name17.address) "x" <~ name17.signal)
nf18 : Signal Element
nf18 = (field textBoxStyle (Signal.message name18.address) "y" <~ name18.signal)
nf19 : Signal Element
nf19 = (field textBoxStyle (Signal.message name19.address) "" <~ name19.signal)
nf20 : Signal Element
nf20 = (field textBoxStyle (Signal.message name20.address) "" <~ name20.signal)

name17 : Signal.Mailbox Content
name17 = Signal.mailbox (Content (toString 0) (Selection 0 0 Forward))

name18 : Signal.Mailbox Content
name18 = Signal.mailbox (Content (toString 0) (Selection 0 0 Forward))

name19 : Signal.Mailbox Content
name19 = Signal.mailbox (Content (toString 0) (Selection 0 0 Forward))

name20 : Signal.Mailbox Content
name20 = Signal.mailbox (Content (toString 0) (Selection 0 0 Forward))

textboxMailbox3 = Signal.mergeMany [ name17.signal
                                   , name18.signal
                                   , name19.signal
                                   , name20.signal
                                   ]                                   

buttonMailbox3 : Signal.Mailbox String
buttonMailbox3 = Signal.mailbox "filled"

-- TRANSFORMER 4

nf21 : Signal Element
nf21 = (field {textBoxStyle | style <- {textStyle | height <- Just 11}} (Signal.message name21.address) "x" <~ name21.signal)
nf22 : Signal Element
nf22 = (field textBoxStyle (Signal.message name22.address) "y" <~ name22.signal)
nf23 : Signal Element
nf23 = (field textBoxStyle (Signal.message name23.address) "" <~ name23.signal)
nf24 : Signal Element
nf24 = (field textBoxStyle (Signal.message name24.address) "" <~ name24.signal)

name21 : Signal.Mailbox Content
name21 = Signal.mailbox (Content (toString 0) (Selection 0 0 Forward))

name22 : Signal.Mailbox Content
name22 = Signal.mailbox (Content (toString 0) (Selection 0 0 Forward))

name23 : Signal.Mailbox Content
name23 = Signal.mailbox (Content (toString 0) (Selection 0 0 Forward))

name24 : Signal.Mailbox Content
name24 = Signal.mailbox (Content (toString 0) (Selection 0 0 Forward))

textboxMailbox4 = Signal.mergeMany [ name21.signal
                                   , name22.signal
                                   , name23.signal
                                   , name24.signal
                                   ]                                   

buttonMailbox4 : Signal.Mailbox String
buttonMailbox4 = Signal.mailbox "filled"

textBoxStyle : Style
textBoxStyle = 
        { padding = (uniformly 1)
        , outline = { color = grey, width = uniformly 1, radius = 0 }--noOutline
        , highlight = { color = blue, width = 1 }
        , style = textStyle
        }
        
textStyle = 
        { typeface = []
        , height = Just 11
        , color = black
        , bold = False
        , italic = False
        , line = Nothing
        }                                   

type Update = Button String | Typing Content

combinedInput : Signal Update
combinedInput = Signal.mergeMany [ (Signal.map Button buttonMailbox.signal)
                                 , (Signal.map Typing textboxMailbox)
                                 ]

combinedInput2 : Signal Update
combinedInput2 = Signal.mergeMany [ (Signal.map Button buttonMailbox2.signal)
                                  , (Signal.map Typing textboxMailbox2)
                                  ]

combinedInput3 : Signal Update
combinedInput3 = Signal.mergeMany [ (Signal.map Button buttonMailbox3.signal)
                                  , (Signal.map Typing textboxMailbox3)
                                  ]

combinedInput4 : Signal Update
combinedInput4 = Signal.mergeMany [ (Signal.map Button buttonMailbox4.signal)
                                  , (Signal.map Typing textboxMailbox4)
                                  ]

--main = view <~ (Signal.foldp update init combinedInput) ~ nf9 ~ nf10 ~ nf11 ~ nf12 -- ~ buttonMailbox.signal
