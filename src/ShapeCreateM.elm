
module ShapeCreateM where

--Basic imports
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)
import Text exposing (fromString, monospace)
import String exposing (toInt, slice, contains)
import Color exposing (..)

-- Input Libaries
import Graphics.Input.Field exposing (..)
import Graphics.Input exposing (..)
import Mouse
import Result
import Time exposing (..)

-- Other Module Imports
import Transformer exposing (..)
import ColourPickerM exposing (..)
import TimeOut exposing (..)
--import QuizMode exposing (..)
 
------------------------------------------------------------------------------------------------
{--MODEL--}

type alias State = 
        { -- Dimensions
          width : Int, height : Int
        -- ShapeCreate
        , pickFilled : Bool, pickOutlined : Bool, filledRed : Bool, filledGreen : Bool
        , filledBlue : Bool, filledCustom : Bool, outlinedSolid : Bool
        , outlinedDashed : Bool, outlinedDotted : Bool, outlinedBlack : Bool
        , outlinedOrange : Bool, outlinedBlue : Bool, outlinedCustom : Bool
        , pickCircle : Bool, pickSquare : Bool, pickRect : Bool, pickOval : Bool
        , pickNgon : Bool, chooseMove : Bool, chooseRotate : Bool
        , chooseScale : Bool, transformer : Bool
        , currTextbox : String
        , circRadius : Float, sqLength : Float, rectLength : Float, rectWidth : Float
        , ovalLength : Float, ovalWidth : Float, ngonSize : Int , ngonRadius : Float
        , cpState : ColourPickerM.State, t1State : Transformer.State, t2State : Transformer.State 
        , t3State : Transformer.State, t4State : Transformer.State
        , formStyleCode : String, formColourCode : String, formCode : String
        , outStyleCode : String, outColourCode : String
        , shapeCode : String, totalCode : String
        -- Timer
        , counter : Float, timer : Float, timeLimit : Float, task : TaskStatus
        --, demoState : TimeOut.State
        -- Transformer
        , currTrans : String
        -- Quiz Mode
        , quizMode : Bool, q1Answered : Bool, q2Answered : Bool, q3Answered : Bool, q4Answered : Bool, q5Answered : Bool, q6Answered : Bool
        , q1Text : String, q2Text : String, q3Text : String, q4Text : String, q5Text : String, q6Text : String
        , q1Code : String, q2Code : String, q3Code : String, q4Code : String, q5Code1 : String, q5Code2 : String, q5Code3 : String, q6Code : String        
        , quizOver : Bool
        , currQuestion : String, message : String
        --, quizState : QuizMode.State
        , x : Int, y: Int
        }

init : State 
init = let
        t1 = Transformer.init
        t2 = Transformer.init
       in
        { width = 768, height = 768 
        -- ShapeCreate
        , pickFilled = True, pickOutlined = False, filledRed = True, filledGreen = False
        , filledBlue = False, filledCustom = False, outlinedSolid = True
        , outlinedDashed = False, outlinedDotted = False, outlinedBlack = True
        , outlinedOrange = False, outlinedBlue = False, outlinedCustom = False 
        , pickCircle = True, pickSquare = False, pickRect = False, pickOval = False
        , pickNgon = False, chooseMove = True, chooseRotate = False
        , chooseScale = False, transformer = True
        , currTextbox = "degrees"
        , circRadius = 45, sqLength = 75, rectLength = 40, rectWidth = 60
        , ovalLength = 70, ovalWidth = 90, ngonSize =5 , ngonRadius = 50
        , cpState = ColourPickerM.init
        , t1State = {t1 |  horShift <- (-195), vertShift <- 65, degrees <- 0}
        , t2State = {t2 | degrees <- 45, scaleFactor <- 2}
        , t3State = Transformer.init, t4State = Transformer.init
        , formStyleCode = "filled", formColourCode = "red" , formCode = "filled red (circle 45)"
        , outStyleCode = " (solid", outColourCode = "black)"
        , shapeCode = "(circle 45)", totalCode = "filled red (circle 45)"
        -- Timer
        , counter = 60, timer = 0, timeLimit = 60, task = Done--, demoState = TimeOut.init
        -- Transformer
        , currTrans = ""
        -- Quiz Mode
        , quizMode = False
        , q1Answered = False, q2Answered = True, q3Answered = True, q4Answered = True, q5Answered = True
        , q6Answered = True
        --
        , q1Text = "Make a square with side length 45, and fill it blue."
        , q1Code = "filled blue (square 45)"
        --
        , q2Text = "Rotate your shape by 135 degrees."
        , q2Code = " |> rotate (degrees 135)"
        --
        , q3Text = "Fill your shape with a custom colour (and click DONE)."
        , q3Code = "filled (hsl (degrees "
        --
        , q4Text = "Give your shape a dotted blue outline."
        , q4Code = "outlined (dotted blue)"
        --
        , q5Text = "Create an 8 sided polygon (aka ngon) with radius 137. Give it a solid outline with a custom colour."
        , q5Code1 = "(ngon 8 137)", q5Code2 = "solid", q5Code3 = "hsl"   
        --
        , q6Text = ""
        , q6Code = ""
        -- 
        , quizOver = False 
        , currQuestion = "", message = ""
        --, quizState = QuizMode.init
        , x = 0, y = 0 
        }


------------------------------------------------------------------------------------------------
{--UPDATE--}

update input m = 
    case input of
      Mouse (x,y) -> clickUpdate x y m
      Movement (x,y) -> checkRegion x y m--{m | formCode <- appendFormCode m, totalCode <- appendTotalCode m}--resetTimer m
      Click bool -> m
      Run time -> {--timeoutUpdate--} {m| counter <- incrementTimer m}
      --DemoMode state -> {m | demoState <- state}
      Refresh time -> {m | formCode <- appendFormCode m, totalCode <- appendTotalCode m}
      Transformer1 t1Step -> {m | t1State <- t1Step}
      Transformer2 t2Step -> {m | t2State <- t2Step} 
      Task Done -> finishTask m
      Quiz Start -> startQuiz m 
      Quiz Quit -> {m | quizMode <- False}
      Quiz Verify -> checkEntry m
      Button string -> changeState string m
      Typing content -> checkValue (String.toFloat (checkBox content.string m)) m

checkRegion x y m = if | ((x > 438) && (x < 568)) && ((y > 100) && (y < 222)) -> {m | currTrans <- "transformer 1" }
                       | ((x > 603) && (x < 733)) && ((y > 100) && (y < 222)) -> {m | currTrans <- "transformer 2" }
                       | otherwise -> m

clickUpdate x y m = {m | cpState <- (if m.task == Incomplete then (updateColourPicker x y m) else m.cpState)
                , counter <- resetCounter
                , message <- updateMessage m
                , x <- x
                , y <- y
                } 

updateColourPicker x y m = ColourPickerM.changeColor x y m.cpState

quizComplete m = if m.q1Answered && m.q2Answered && m.q3Answered && m.q4Answered && m.q5Answered then True else False      

updateMessage m = if (quizComplete m) then m.message else ""

resetCounter = 0

incrementTimer m = m.counter + 1

finishTask m  = { m | task <- Done
                , formColourCode <- ColourPickerM.hslCodeString m.cpState
                --, updateColourPicker m.x m.y m
                }

startQuiz m = { m | quizMode <- True, q1Answered <- False, q2Answered <- True, q3Answered <- True
              , currQuestion <- m.q1Text, message <- "", quizOver <- False
              }

checkEntry m = let
                  t1 = m.t1State
                  t2 = m.t2State
               in
                  if | m.q1Answered == False && m.formCode == m.q1Code -> {m | q1Answered <- True
                                                                          , q2Answered <- False      
                                                                          , currQuestion <- m.q2Text
                                                                          , message <- "Good Job!"
                                                                          }
                     | m.q2Answered == False && (verifyTransform (||) t1 t2 m.q2Code) -> { m | q2Answered <- True
                                                                                       , q3Answered <- False
                                                                                       , currQuestion <- m.q3Text
                                                                                       , message <- "Good Job!"
                                                                                       }
                     | m.q3Answered == False && (slice 0 21 m.formCode) == m.q3Code -> { m | q3Answered <- True
                                                                                       , q4Answered <- False
                                                                                       , currQuestion <- m.q4Text
                                                                                       , message <- "Good Job!"
                                                                                       }
                     | m.q4Answered == False && (slice 0 22 m.formCode) == m.q4Code -> { m | q4Answered <- True
                                                                                       , q5Answered <- False
                                                                                       , currQuestion <- m.q5Text
                                                                                       , message <- "Good Job!"
                                                                                       }
                     | m.q5Answered == False && contains m.q5Code1 m.formCode&& contains m.q5Code2 m.formCode && contains m.q5Code3 m.formCode -> { m | q5Answered <- True
                                                                                       , q6Answered <- False
                                                                                       , currQuestion <- m.q6Text
                                                                                       , message <- victoryMessage
                                                                                       , quizOver <- True
                                                                                       }                                                                                                                                                                                                        
                     | m.q1Answered && m.q2Answered && m.q3Answered && m.q4Answered && m.q5Answered -> {m | message <- victoryMessage}
                     | otherwise -> {m | message <- "Try Again!"}

victoryMessage = "You've reached the end of the quiz. Congratulations!"

nextTutorial = Text.link "http://www.google.ca" (fromString "Click here to go the next tutorial.")

verifyTransform boolOp t1 t2 ans = boolOp (t1.transformCode == ans) (t2.transformCode == ans)
      
checkValue result m =
    case result of
      Ok value -> updateValue value m
      _ -> m
      
updateValue value m =
    if | m.currTextbox == "circRadius" -> {m | circRadius <- value, shapeCode <- "(circle " ++ (toString value) ++ ")"}
       | m.currTextbox == "sqLength" -> {m | sqLength <- value, shapeCode <- "(square " ++ (toString value) ++ ")"}
       | m.currTextbox == "rectLength" -> {m | rectLength <- value, shapeCode <- "(rect " ++ (toString value) ++ " " ++ (toString m.rectLength) ++ ")"}
       | m.currTextbox == "rectWidth" -> {m | rectWidth <- value, shapeCode <- "(rect " ++ (toString m.rectWidth) ++ " " ++ (toString value) ++ ")"}
       | m.currTextbox == "ovalLength" -> {m | ovalLength <- value, shapeCode <- "(oval " ++ (toString value) ++ " " ++ (toString m.ovalLength) ++ ")"}
       | m.currTextbox == "ovalWidth" -> {m | ovalWidth <- value, shapeCode <- "(oval " ++ (toString m.ovalWidth) ++ " " ++ (toString value) ++ ")"}
       | m.currTextbox == "ngonSize" -> {m | ngonSize <- round value, shapeCode <- "(ngon " ++ (toString value) ++ " " ++ (toString m.ngonRadius) ++ ")"}
       | m.currTextbox == "ngonRadius" -> {m | ngonRadius <- value, shapeCode <- "(ngon " ++ (toString m.ngonSize) ++ " " ++ (toString value) ++ ")"}
       | otherwise -> m

appendFormCode m = m.formStyleCode ++ " " ++ m.formColourCode ++ " " ++ m.shapeCode

appendTotalCode m = 
    let
      t1 = m.t1State
      t2 = m.t2State
    in
      m.formCode ++ t1.transformCode ++ t2.transformCode

changeState string m = if -- filled
                           | string == "filled" -> {m | formStyleCode <- "filled", pickFilled <- True, pickOutlined <- False, currTrans <- ""}
                           | string == "filled red" -> {m | formStyleCode <- "filled", formColourCode <- "red", pickFilled <- True, pickOutlined <- False, filledRed <- True, filledGreen <- False, filledBlue <- False, filledCustom <- False, currTrans <- ""}
                           | string == "filled green" -> {m | formStyleCode <- "filled", formColourCode <- "green", pickFilled <- True, pickOutlined <- False, filledRed <- False, filledGreen <- True, filledBlue <- False, filledCustom <- False, currTrans <- ""}
                           | string == "filled blue" -> {m | formStyleCode <- "filled", formColourCode <- "blue", pickFilled <- True, pickOutlined <- False, filledRed <- False, filledGreen <- False, filledBlue <- True, filledCustom <- False, currTrans <- ""}
                           | string == "filled custom" -> {m | formStyleCode <- "filled", {-formColourCode <- ColourPickerM.hslCodeString m.cpState,-} pickFilled <- True, pickOutlined <- False, filledRed <- False, filledGreen <- False, filledBlue <- False, filledCustom <- True, task <- Incomplete, currTrans <- ""}
                           -- outlined
                           | string == "outlined" -> {m | formStyleCode <- "outlined" ++ m.outStyleCode, formColourCode <- m.outColourCode, pickFilled <- False, pickOutlined <- True, currTrans <- ""}
                           | string == "solid" -> {m | formStyleCode <- "outlined (solid", outStyleCode <- " (solid", pickOutlined <- True, pickFilled <- False, outlinedSolid <- True, outlinedDashed <- False, outlinedDotted <- False, currTrans <- ""}
                           | string == "dashed" -> {m | formStyleCode <- "outlined (dashed", outStyleCode <- " (dashed", pickOutlined <- True, pickFilled <- False, outlinedSolid <- False, outlinedDashed <- True, outlinedDotted <- False, currTrans <- ""}
                           | string == "dotted" -> {m | formStyleCode <- "outlined (dotted", outStyleCode <- " (dotted", pickOutlined <- True, pickFilled <- False, outlinedSolid <- False, outlinedDashed <- False, outlinedDotted <- True, currTrans <- ""}
                           | string == "outlined black" -> {m | formStyleCode <- "outlined" ++ m.outStyleCode, formColourCode <- "black)", outColourCode <- "black)", pickOutlined <- True, pickFilled <- False, outlinedBlack <- True, outlinedOrange <- False, outlinedBlue <- False, outlinedCustom <- False, currTrans <- ""}
                           | string == "outlined orange" -> {m | formStyleCode <- "outlined" ++ m.outStyleCode, formColourCode <- "orange)", outColourCode <- "orange)", pickOutlined <- True, pickFilled <- False, outlinedBlack <- False, outlinedOrange <- True, outlinedBlue <- False, outlinedCustom <- False, currTrans <- ""}
                           | string == "outlined blue" -> {m | formStyleCode <- "outlined" ++ m.outStyleCode, formColourCode <- "blue)", outColourCode <- "blue)", pickOutlined <- True, pickFilled <- False, outlinedBlack <- False, outlinedOrange <- False, outlinedBlue <- True, outlinedCustom <- False, currTrans <- ""}
                           | string == "outlined custom" -> {m | {-formColourCode <- ColourPickerM.hslCodeString m.cpState ++ ")",-} pickOutlined <- True, pickFilled <- False, outlinedBlack <- False, outlinedOrange <- False, outlinedBlue <- False, outlinedCustom <- True, task <- Incomplete, currTrans <- ""}
                           -- shapes
                           | string == "circle" -> {m | shapeCode <- "(circle " ++ (toString m.circRadius) ++ ")", pickCircle <- True, pickSquare <- False, pickRect <- False, pickOval <- False, pickNgon <- False, currTrans <- ""}
                           | string == "square" -> {m | shapeCode <- "(square " ++ (toString m.sqLength) ++ ")", pickCircle <- False, pickSquare <- True, pickRect <- False, pickOval <- False, pickNgon <- False, currTrans <- ""}
                           | string == "rect" -> {m | shapeCode <- "(rect " ++ (toString m.rectWidth) ++ " " ++ (toString m.rectLength) ++ ")", pickCircle <- False, pickSquare <- False, pickRect <- True, pickOval <- False, pickNgon <- False, currTrans <- ""}
                           | string == "oval" -> {m | shapeCode <- "(oval " ++ (toString m.ovalWidth) ++ " " ++ (toString m.ovalLength) ++ ")", pickCircle <- False, pickSquare <- False, pickRect <- False, pickOval <- True, pickNgon <- False, currTrans <- ""}
                           | string == "ngon" -> {m | shapeCode <- "(ngon " ++ (toString m.ngonSize) ++ " " ++ (toString m.ngonRadius) ++ ")", pickCircle <- False, pickSquare <- False, pickRect <- False, pickOval <- False, pickNgon <- True, currTrans <- ""}
                           -- shape properties
                           | string == "circRadius" -> {m | currTextbox <- "circRadius", currTrans <- ""}
                           | string == "sqLength" -> {m | currTextbox <- "sqLength", currTrans <- ""}
                           | string == "rectLength" -> {m | currTextbox <- "rectLength", currTrans <- ""}
                           | string == "rectWidth" -> {m | currTextbox <- "rectWidth", currTrans <- ""}
                           | string == "ovalLength" -> {m | currTextbox <- "ovalLength", currTrans <- ""}
                           | string == "ovalWidth" -> {m | currTextbox <- "ovalWidth", currTrans <- ""}
                           | string == "ngonSize" -> {m | currTextbox <- "ngonSize", currTrans <- ""}
                           | string == "ngonRadius" -> {m | currTextbox <- "ngonRadius", currTrans <- ""}
                           -- transformers 
                           | string == "transformer 1" -> {m | currTrans <- "transformer 1"}
                           | string == "transformer 2" -> {m | currTrans <- "transformer 2"}
                           | string == "transformer 3" -> {m | currTrans <- "transformer 3"}
                           | string == "transformer 4" -> {m | currTrans <- "transformer 4"}
                           -- else                           
                           | otherwise -> m

------------------------------------------------------------------------------------------------
{--VIEW--}

view m colourPicker cpState (x,y) nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 nf9 nf10 nf11 nf12 nf13 nf14 nf15 nf16 buttonSelect timeoutStep =
        let
            trans1 state = (Transformer.view state nf9 nf10 nf11 nf12 m.currTrans)
            trans2 state = (Transformer.view state nf13 nf14 nf15 nf16 m.currTrans)
            cpView = ColourPickerM.view cpState
        in
        --normalView m colourPicker csState trans1 trans1State tr2Display tr2State t3Display t3State t4Display t4State (x,y) nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 nf9 nf10 nf11 nf12 buttonSelect
        if | m.counter >= m.timeLimit && m.quizMode == False -> normalView timeoutStep cpView m.cpState (trans1 timeoutStep.t1State) timeoutStep.t1State (trans2 timeoutStep.t2State) timeoutStep.t2State (x,y) nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 nf9 nf10 nf11 nf12 buttonSelect timeoutStep 
           | otherwise -> normalView m cpView m.cpState (trans1 m.t1State) m.t1State (trans2 m.t2State) m.t2State (x,y) nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 nf9 nf10 nf11 nf12 buttonSelect timeoutStep


normalView m colourPicker csState trans1 trans1State tr2Display tr2State (x,y) nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 nf9 nf10 nf11 nf12 buttonSelect timeoutStep = 
  flow down
    [ --show m.formCode,
      flow outward 
        [ --color green <| spacer 768 768,
         canvas m.width m.height m colourPicker csState trans1 trans1State tr2Display tr2State nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 nf9 nf10 nf11 nf12
        --, show (m.counter,m.timer)
        --, show (x,y)
        , if | m.counter >= m.timeLimit && m.quizMode == False-> (collage m.width m.height 
                                    [ (text <| (Text.color (clearGrey 0.1))<| Text.height 100 <| (fromString "DEMO MODE"))
                                    , (text <| (Text.color (clearGrey 0.7))<| Text.height 20 <| (fromString "click anywhere to exit this demo"))
                                        |> move (0,-75)
                                    ] 
                                 )   
             | otherwise -> empty
        , container 768 768 (topRightAt (absolute 5) (absolute 0)) (container 110 50 middle startButton)
        , if m.quizMode then container 768 768 middle (quizDisplay m) else empty
        ]
    ]    
quizDisplay m =
    container m.width m.height middle
    <| flow outward
          [ spacer m.width 80 |> color (clearGrey 0.1)
          , container m.width 50 midTop <| leftAligned <| quizMessage (m.message)--if m.counter < 3 then (fromString m.message) else (fromString "")
          , if m.q1Answered == False then container m.width 59 middle (leftAligned (fromString ("Task 1: " ++ m.currQuestion))) else empty
          , if m.q2Answered == False then container m.width 59 middle (leftAligned (fromString ("Task 2: " ++ m.currQuestion))) else empty
          , if m.q3Answered == False then container m.width 59 middle (leftAligned (fromString ("Task 3: " ++ m.currQuestion))) else empty
          , if m.q4Answered == False then container m.width 59 middle (leftAligned (fromString ("Task 4: " ++ m.currQuestion))) else empty
          , if m.q5Answered == False then container m.width 59 middle (leftAligned (fromString ("Task 5: " ++ m.currQuestion))) else empty
          , if m.quizOver == True then container m.width 59 middle (leftAligned nextTutorial) else empty
          , container m.width 80 (midBottomAt (absolute (-50 + (m.width//2))) (absolute 5)) verifyButton
          , container m.width 80 (midBottomAt (absolute (50 + (m.width//2))) (absolute 5)) quitButton
          ]

quizMessage message = if | message == "Try Again!" -> Text.color red <| fromString message
                         | message == "Good Job!" -> Text.color green <| fromString message
                         | message == victoryMessage -> Text.color orange <| fromString message
                         | otherwise -> fromString message

--timeOutView m colourPicker csState trans1 trans1State tr2Display tr2State t3Display t3State t4Display t4State (x,y) nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 nf9 nf10 nf11 nf12 buttonSelect
--    = normalView m colourPicker csState trans1 trans1State tr2Display tr2State t3Display t3State t4Display t4State (x,y) nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 nf9 nf10 nf11 nf12 buttonSelect

checkBox string m = if string == "" then "0" else string

boxOutline = 
    let
        w  = round (768*0.5325)
        w' = (768*0.5325) - 5
        h  = round (768*0.42)
        h' = (768*0.42) - 10
    in    
    collage w h [outlined (solid darkGrey) (rect w' h') |> move (2,0)]-- |> color grey

-- CANVAS

canvas w h m colourPicker csState trans1Display trans1State tr2Display tr2State nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 nf9 nf10 nf11 nf12 = 
        flow down
        [ flow outward
            [ boxOutline
            , selectionCanvas w ((h*42)//100) m trans1Display tr2Display t3Display t4Display nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 nf9 nf10 nf11 nf12
            --, color opaqueWhite (spacer w ((h*42)//100)) |> clickable (Signal.message buttonMailbox.address "nothing")
            , if | checkCpSelect m -> blockSelection w ((h*42)//100)
                 | otherwise -> empty
            ]
        , codeCanvas w (h*5//100) m trans1State tr2State
        --, if m.quizMode then spacer m.width 80 else empty
        , if | checkCpSelect m -> container w (h//2) (midLeftAt (absolute 20) (absolute (h//4+28))) ( flow outward
                                                                [ colourPicker
                                                                , buttonDisplay doneButton colourPicker
                                                                ]
                                                            ) 
             | otherwise -> displayCanvas m csState trans1State tr2State w (h//2)
        ]

blockSelection w h = color opaqueWhite (spacer w h)
                     |> clickable (Signal.message buttonMailbox.address "nothing")

buttonPosition = midLeftAt (absolute 480) (absolute 175)        
buttonDisplay button cp = container (widthOf cp) (heightOf cp) buttonPosition button

checkCpSelect m = ((m.pickFilled && m.filledCustom) || (m.pickOutlined && m.outlinedCustom)) && m.task == Incomplete        

selectionCanvas w h m trans1 trans2 trans3 trans4 nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 nf9 nf10 nf11 nf12 = 
        flow right 
        [ formCanvas (w*250//768) h m -- |> color lightGreen
        , shapeCanvas (w*155//768) h nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 m -- |> color lightBlue
        , feedSymbol (w*30//768) h-- |> color lightPurple
        , transCanvas ((w*135)//768) h m trans1 1
        , feedSymbol (w*30//768) h-- |> color lightPurple
        , transCanvas ((w*135)//768) h m trans2 2
        --, spacer (w*61//768) h |> color blue
             -- |> color lightOrange
        ] 

codeCanvas w h m trans1State tr2State =
        container w h midTop (centered <| monospace <| Text.color black <|fromString (code m trans1State tr2State)) -- |> color blue

code m trans1State tr2State = (formText m) ++ (shapeText m) ++ (transformers trans1State) ++ (transformers tr2State)-- ++ (transformers tr3State)-- ++ (transformers m tr4State)


-- CHECKS UPDATED MODEL FOR WHICH TEXT SHOULD BE DISPLAYED

formText m = if | m.pickFilled == True -> "filled " ++ (filledColour m) ++ " "
                | m.pickOutlined == True -> "outlined (" ++ (outlinedStyle m) ++ " " ++ (outlinedColour m) ++ ") "
           
filledColour m = if | m.filledRed == True -> "red"
                    | m.filledGreen == True -> "green"
                    | m.filledBlue == True -> "blue"
                    | m.filledCustom == True && m.quizMode == False -> (ColourPickerM.hslCodeString m.cpState)
                    | m.filledCustom == True && m.quizMode -> m.formColourCode
                    
outlinedStyle m = if | m.outlinedSolid == True -> "solid" 
                     | m.outlinedDashed == True ->  "dashed"
                     | m.outlinedDotted == True ->  "dotted"
                    
outlinedColour m = if | m.outlinedBlack == True -> "black"
                      | m.outlinedOrange == True -> "orange"
                      | m.outlinedBlue == True -> "blue"
                      | m.outlinedCustom == True -> (ColourPickerM.hslCodeString m.cpState)
        
shapeText m = 
              if | m.pickCircle == True -> "(circle " ++ (toString m.circRadius) ++ ") " 
                 | m.pickSquare == True -> "(square " ++ (toString m.sqLength) ++ ") "
                 | m.pickRect == True -> "(rect " ++ (toString m.rectLength)  ++ " " ++ (toString m.rectWidth) ++ ") " 
                 | m.pickOval == True -> "(oval " ++ (toString m.ovalLength) ++ " " ++ (toString m.ovalWidth) ++ ") " 
                 | m.pickNgon == True -> "(ngon " ++ (toString m.ngonSize) ++ " " ++ (toString m.ngonRadius) ++ ") " 
                 
transformers state = 
        if | state.chooseMove == True -> "|> move (" ++ (toString state.horShift) ++ "," ++ (toString state.vertShift) ++ ") " 
           | state.chooseRotate == True -> "|> rotate (degrees " ++ (toString state.degrees) ++ ") "
           | state.chooseScale == True -> "|> scale " ++ (toString state.scaleFactor) ++ " "
           | otherwise -> ""


-- CHECKS UPDATED MODEL FOR HOW THE SHAPE SHOULD BE DISPLAYED

displayCanvas m cs tr1 tr2 w h =
                  collage w h
                          [ (checkForm m cs) (checkShape m) 
                              |> (checkTransform tr1)
                              |> (checkTransform tr2)
                              --|> (checkTransform tr3)
                              --|> (checkTransform tr4)
                          ]

checkForm m cs = if | m.pickFilled -> filled (checkFilledColour m cs)
                    | m.pickOutlined -> outlined (checkOutlineStyle m cs)

checkFilledColour m cs =
                    let
                        cpState = m.cpState
                    in 
                        if | m.filledRed -> red
                           | m.filledGreen -> green
                           | m.filledBlue -> blue
                           | m.filledCustom -> hsl cpState.h cpState.s cpState.l
                   
                 
checkOutlineStyle m cs = if | m.outlinedSolid -> (solid (checkOutlinedColour m cs))
                         | m.outlinedDashed -> (dashed (checkOutlinedColour m cs))
                         | m.outlinedDotted -> (dotted (checkOutlinedColour m cs))
                    
checkOutlinedColour m cs = 
                        let
                            cpState = m.cpState
                        in
                            if | m.outlinedBlack -> black
                               | m.outlinedOrange -> orange
                               | m.outlinedBlue -> blue
                               | m.outlinedCustom -> hsl cpState.h cpState.s cpState.l

checkShape m = if | m.pickCircle -> circle m.circRadius
                  | m.pickSquare -> square m.sqLength
                  | m.pickRect -> rect m.rectLength m.rectWidth
                  | m.pickOval -> oval m.ovalLength m.ovalWidth
                  | m.pickNgon -> ngon m.ngonSize m.ngonRadius

checkTransform tr1 = if | tr1.chooseMove -> move (tr1.horShift,tr1.vertShift)
                      | tr1.chooseRotate -> rotate (degrees tr1.degrees)
                      | tr1.chooseScale -> scale tr1.scaleFactor
                      | otherwise -> rotate 0

-- FORM CANVAS

formCanvas w h m = 
    container w h middle (formMenu m)

formMenu m = flow outward 
        [ formMenuBackDrop
        , container 240 325 middle (formButtons m)
        ]

formMenuBackDrop = 
    collage 240 330
        [ outlined (solid black) (rect 230 270)
        , outlined (solid black) (oval 230 25) |> move (0,135)
        , outlined (solid black) (oval 230 25) |> move (0,-135)
        , filled white (rect 230 270)
        , filled white (oval 230 25) |> move (0,135)
        , filled white (oval 230 25) |> move (0,-135)
        , text (Text.color black (fromString "PICK ONE")) |> move (0,140)
        ]

formButtons m = flow down 
        [ container 230 130 middle (filledButton m) 
            |> clickable (Signal.message buttonMailbox.address "filled")
        , container 230 130 middle (outlinedButton m) 
            |> clickable (Signal.message buttonMailbox.address "outlined")
        ]

formButtonOutline select = 
        if | select -> collage 230 125 [outlined (solid black) (rect 225 125)]
           | otherwise -> collage 230 125 [filled (hsla (degrees 0) 0 1 0.5) (rect 225 125)]

-- FILLED BUTTON + CONTENT

filledButton m = 
        flow inward
            [ formButtonOutline m.pickFilled
            , container 230 125 middle (fbContent m)
            ]
        
fbContent m = 
        flow right
            [ container 75 125 midRight (centered (fromString "filled")) 
            , container 150 125 middle (fillColPalette m)
            ]

-- FILLED COLOURS MENU

fillColPalette m = 
        flow outward
            [ container 113 125 middle (fcPaletteBackDrop)
            , container 113 125 middle (fcPaletteButtons m)
            ]

fcPaletteBackDrop =
        collage 96 105
            [ outlined (solid black) (rect 95 83.75) 
            , outlined (solid black) (oval 95 20) |> move (0,41.875)
            , outlined (solid black) (oval 95 20) |> move (0,-41.875)
            , filled white (rect 95 83.75) 
            , filled white (oval 95 20) |> move (0,41.875)
            , filled white (oval 95 20) |> move (0,-41.875)
            , (text <| Text.color black <| Text.height 10 <| fromString "PICK ONE") |> move (0,46)
            ]
            
fcPaletteButtons m =
        flow down 
            [ spacerBlock 
            , filledColourButton m.filledRed red "red"
                |> clickable (Signal.message buttonMailbox.address "filled red")
            , spacerBlock
            , filledColourButton m.filledGreen green "green"
                |> clickable (Signal.message buttonMailbox.address "filled green")
            , spacerBlock
            , filledColourButton m.filledBlue blue "blue"
                |> clickable (Signal.message buttonMailbox.address "filled blue")
            , spacerBlock
            , filledColourButton m.filledCustom black "custom"
                |> clickable (Signal.message buttonMailbox.address "filled custom")
            ] 

--FILLED RED

spacerBlock = spacer 70 5

filledColourButton select colour string =
        flow outward
            [ container 70 15 midBottom <| centered <| Text.color colour <| fromString string 
            , colourButton select
            ] 

-- PAINTS OUTLINE AROUND SELECTED BUTTON
colourButton select = if | select -> collage 70 15 [outlined (solid black) (rect 55 15)]
                         | otherwise -> collage 70 15 [filled (hsla (degrees 0) 0 1 0.5) (rect 55 15)]

-- OUTLINED BUTTON

outlinedButton m =
        flow inward
            [ formButtonOutline m.pickOutlined
            , container 230 125 middle (outlinedContent m)
            ]

-- OUTLINED BUTTON CANVAS
-- "OUTLINED" + STYLE MENU + COLOUR 
outlinedContent m = 
        flow right
            [ container 60 125 middle (centered (fromString "outlined")) -- text
            , container 5 125 middle (centered (fromString "(")) -- text
            -- OUTLINE STYLES BUTTONS  
            , container 70 125 middle (outStylesMenu m) 
            , container 5 125 middle (centered (fromString ",")) -- text
            -- OUTLINE COLOURS BUTTONS
            , container 70 125 middle (outPaletteMenu m) 
            , container 5 125 middle (centered (fromString ")")) -- text
            ]

--OUTLINE TYPES MENU

outStylesMenu m = 
         flow outward
            [ osMenuBackDrop
            , container 70 125 middle (outStyleButtons m)
            ]

osMenuBackDrop = 
        collage 70 125
            [ outlined (solid black) (rect 65 75)
            , outlined (solid black) (oval 65 15) |> move (0,37.5)
            , outlined (solid black) (oval 65 15) |> move (0,-37.5)
            , filled white (rect 65 75)
            , filled white (oval 65 15) |> move (0,37.5)
            , filled white (oval 65 15) |> move (0,-37.5)
            , (text <| Text.color black <| Text.height 10 <| fromString "PICK ONE") |> move (0,37)
            ]

-- OUTLINE STYLES

outStyleButtons m =
        flow down
            [ styleButton m.outlinedSolid "solid"
                |> clickable (Signal.message buttonMailbox.address "solid")
            , styleButton m.outlinedDashed "dashed"
                |> clickable (Signal.message buttonMailbox.address "dashed")
            , styleButton m.outlinedDotted "dotted"
                |> clickable (Signal.message buttonMailbox.address "dotted")
            ]

styleButton select string = 
        flow outward
            [ styleButtonContent string
            , osButtonBackDrop select
            ]

styleButtonContent string =
        container 55 20 middle <| centered <| Text.color (black) <|fromString string            

osButtonBackDrop select = if | select -> collage 55 20 [outlined (solid black) (rect 55 15)]
                             | otherwise -> collage 55 20 [filled (hsla (degrees 0) 0 1 0.5) (rect 55 15)]

--OUTLINE COLOURS MENU

outPaletteMenu m = 
        flow outward
            [ opBackDrop -- |> Graphics.Element.color orange
            , container 70 125 middle (opColourButtons m)-- |> Graphics.Element.color purple
            ]--|> Graphics.Element.color green
            
opBackDrop =
        collage 70 125
            [ outlined (solid black) (rect 65 83.75)
            , outlined (solid black) (oval 65 20) |> move (0,41.875)
            , outlined (solid black) (oval 65 20) |> move (0,-41.125)
            , filled white (rect 65 83.75)
            , filled white (oval 65 20) |> move (0,41.875)
            , filled white (oval 65 20) |> move (0,-41.125)
            , (text <| Text.color black <| Text.height 10 <| fromString "PICK ONE") |> move (0,46)
            ] 

-- OUTLINE COLOUR BUTTONS

opColourButtons m = 
        flow down
            [ opColourButton m.outlinedBlack black "black" 
                |> clickable (Signal.message buttonMailbox.address "outlined black")
            , spacerBlock 
            , opColourButton m.outlinedOrange orange "orange"
                |> clickable (Signal.message buttonMailbox.address "outlined orange")
            , spacerBlock
            , opColourButton m.outlinedBlue blue "blue"
                |> clickable (Signal.message buttonMailbox.address "outlined blue")
            , spacerBlock
            , opColourButton m.outlinedCustom black "custom"
                |> clickable (Signal.message buttonMailbox.address "outlined custom")
            ]-- |> Graphics.Element.color green 


opColourButton select colour string =
        flow inward
            [ colourButton select
            , container 70 15 midBottom <| centered (Text.color colour (fromString string)) 
            ]

-- SHAPES

-- SHAPES CANVAS

shapeCanvas w h nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 m = 
        flow outward 
            [ container w h middle (shapeMenu w h nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 m)
            ]

-- SHAPES MENU

shapeMenu w h nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 m = 
        flow outward 
            [ shapeMenuBackDrop w h --|> color red
            , container w h middle (shapeButtons nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 m) 
            --, show (w,h)
            ]

shapeMenuBackDrop w h= 
        collage w h 
            [ outlined (solid black) (oval 150 25) |> move (0,110)
            , filled white (oval 150 25) |> move (0,110)
            , outlined (solid black) (oval 150 25) |> move (0,-110)
            , filled white (oval 150 25) |> move (0,-110)
            , outlined (solid black) (rect 150 220)
            , filled white (oval 150 25) |> move (0,110)
            , filled white (rect 150 220)
            , filled white (oval 150 25) |> move (0,-110)
            , text (Text.color black (fromString "PICK ONE")) |> move (0,110)
            ]

-- VARIOUS SHAPE BUTTONS

shapeButtons nf nf2 nf3 nf4 nf5 nf6 nf7 nf8 m = 
        flow down 
            [ circleButton nf m --|> color blue
            , squareButton nf2 m
            , rectButton nf3 nf4 m
            , ovalButton nf5 nf6 m
            , ngonButton nf7 nf8 m
            ]

circleButton nf m = 
        flow outward 
            [ container 150 40 (midLeftAt (absolute 15) (absolute 20)) (leftAligned <| fromString "circle")
            , if m.pickCircle then empty else (circleTextbox nf)
            , sbOutline m.pickCircle  
                |> clickable (Signal.message buttonMailbox.address "circle")
            , if m.pickCircle then (circleTextbox nf) else empty
            ]

circleTextbox nf = container 150 40 (midRightAt (absolute 15) (absolute 22)) (size 40 22 nf)
                     |> clickable (Signal.message buttonMailbox.address "circRadius")

squareButton nf2 m =
        flow outward 
            [ container 150 40 (midLeftAt (absolute 15) (absolute 20)) (leftAligned <| fromString "square")
            , if m.pickSquare then empty else (squareTextbox nf2)
            , sbOutline m.pickSquare 
                |> clickable (Signal.message buttonMailbox.address "square")
            , if m.pickSquare then (squareTextbox nf2) else empty
            ]

squareTextbox nf2 = container 150 40 (midRightAt (absolute 15) (absolute 22)) (size 40 22 nf2)
                      |> clickable (Signal.message buttonMailbox.address "sqLength")

rectButton nf3 nf4 m = 
        flow outward
            [ container 150 40 (midLeftAt (absolute 15) (absolute 20)) (leftAligned <| fromString "rect")
            , if m.pickRect then empty else rectTextbox1 nf3
            , if m.pickRect then empty else rectTextbox2 nf4
            , sbOutline m.pickRect 
                |> clickable (Signal.message buttonMailbox.address "rect")
            , if m.pickRect then rectTextbox1 nf3 else empty
            , if m.pickRect then rectTextbox2 nf4 else empty
            ]

rectTextbox1 nf3 = size 40 22 nf3
                     |> clickable (Signal.message buttonMailbox.address "rectLength")
                     |> container 150 40 (midRightAt (absolute 58) (absolute 22))
--                     |> color red
rectTextbox2 nf4 = size 40 22 nf4
                     |> clickable (Signal.message buttonMailbox.address "rectWidth")
                     |> container 150 40 (midRightAt (absolute 15) (absolute 22))
--                     |> color red

ovalButton nf5 nf6 m = 
        flow outward
            [ container 150 40 (midLeftAt (absolute 15) (absolute 20)) (leftAligned <| fromString "oval")
            , if m.pickOval then empty else ovalTextbox1 nf5
            , if m.pickOval then empty else ovalTextbox2 nf6
            , sbOutline m.pickOval 
                |> clickable (Signal.message buttonMailbox.address "oval")
            , if m.pickOval then ovalTextbox1 nf5 else empty
            , if m.pickOval then ovalTextbox2 nf6 else empty
            ]
            
ovalTextbox1 nf5 =  (size 40 22 nf5)
                     |> clickable (Signal.message buttonMailbox.address "ovalLength")
                     |> container 150 40 (midRightAt (absolute 58) (absolute 22))
ovalTextbox2 nf6 =  (size 40 22 nf6)
                     |> clickable (Signal.message buttonMailbox.address "ovalWidth")
                     |> container 150 40 (midRightAt (absolute 15) (absolute 22))

ngonButton nf7 nf8 m =
        flow outward
            [ container 150 40 (midLeftAt (absolute 15) (absolute 20)) (leftAligned <| fromString "ngon")
            , if m.pickNgon then empty else ngonTextbox1 nf7
            , if m.pickNgon then empty else ngonTextbox2 nf8
            , sbOutline m.pickNgon 
                |> clickable (Signal.message buttonMailbox.address "ngon")
            , if m.pickNgon then ngonTextbox1 nf7 else empty
            , if m.pickNgon then ngonTextbox2 nf8 else empty
            ]

ngonTextbox1 nf7 = size 40 22 nf7
                     |> clickable (Signal.message buttonMailbox.address "ngonSize")
                     |> container 150 40 (midRightAt (absolute 58) (absolute 22))
ngonTextbox2 nf8 =  size 40 22 nf8
                     |> clickable (Signal.message buttonMailbox.address "ngonRadius")
                     |> container 150 40 (midRightAt (absolute 15) (absolute 22))

--SHAPE BUTTON COVER
sbOutline select = if | select == True -> collage 150 40 [outlined sbOutlineStyle <| rect 145 30] 
                      | otherwise -> collage 150 40 [filled (hsla (degrees 0) 0 1 0.5) <| rect 145 30] 

-- CUSTOM STYLE FOR BUTTON OUTLINE
sbOutlineStyle = {defaultLine | color <- black, width <- 0.5}

-- FEED SYMBOL CANVAS

feedSymbol w h = container w h middle 
                    (centered <| Text.color black <| Text.height 25 <| monospace <| fromString "|>")

-- TRANSFORMERS CANVAS

transCanvas w h m transState num = 
    (container w h middle 
        <| flow down 
            [ flow right 
                [ transMenuDisplay transState m.currTrans (toString num)
                ]
            --, flow right   
            --    [ transMenuDisplay t3State m.currTrans (toString 3)
            --        |> container (2*widthOf t1State) (heightOf t2State) middle 
            --    ]
            ]
    )

transMenuDisplay view select num =
    collage (widthOf view) (heightOf view)
        [ toForm (if select /= ("transformer " ++ num) then view else empty)
        , toForm (if | select /= ("transformer " ++ num) -> color (hsla (degrees 0) 0 1 0.5) <| spacer (widthOf view) (heightOf view)
             | otherwise -> view)
        ] |> clickable (Signal.message buttonMailbox.address ("transformer " ++ num))    

type MouseAction = Normal | Hover | Clicked

type TaskStatus = Done | Incomplete

clickDone : Signal.Mailbox TaskStatus
clickDone = Signal.mailbox Incomplete

doneButton : Element
doneButton =
    customButton (Signal.message clickDone.address Done)
        (myButton 100 40 Normal lightRed "DONE") 
        (myButton 100 40 Hover darkRed "DONE")
        (myButton 100 40 Clicked (hsl (degrees 0) 0.38 0.15) "DONE")

myButton : Int -> Int -> MouseAction -> Color -> String -> Element        
myButton w h action xxx string = 
  let
    shape = container w h middle (flow outward
                                       [ Graphics.Element.color xxx (spacer w h)-- |> opacity 0.5
                                       , collage w h [move (0,2)<| text <| Text.color white <| Text.height 18 <| (fromString string)]
                                       ]
                                 )
  in            
    case action of
        Normal -> shape
        Hover -> shape
        Clicked -> shape

type QuizStatus = Start | Verify | Quit

quizMailbox : Signal.Mailbox QuizStatus
quizMailbox = Signal.mailbox Quit

startButton = 
    customButton (Signal.message quizMailbox.address Start)
        (myButton 120 40 Normal lightRed "QUIZ MODE") 
        (myButton 120 40 Hover darkRed "QUIZ MODE")
        (myButton 120 40 Clicked (hsl (degrees 0) 0.38 0.15) "QUIZ MODE")

verifyButton =
    customButton (Signal.message quizMailbox.address Verify)
        (myButton 80 30 Normal lightRed "VERIFY") 
        (myButton 80 30 Hover darkRed "VERIFY")
        (myButton 80 30 Clicked (hsl (degrees 0) 0.38 0.15) "VERIFY")

quitButton =
    customButton (Signal.message quizMailbox.address Quit)
        (myButton 80 30 Normal lightRed "QUIT") 
        (myButton 80 30 Hover darkRed "QUIT")
        (myButton 80 30 Clicked (hsl (degrees 0) 0.38 0.15) "QUIT")    

opaqueWhite : Color
opaqueWhite = hsla (degrees 0) 0 1 0.5

clearGrey : Float -> Color
clearGrey a = hsla (degrees 0) 0 0.435 a

------------------------------------------------------------------------------------------------
{--MAIN--}

name1 : Signal.Mailbox Content
name1 = Signal.mailbox (Content (toString init.circRadius) (Selection 0 0 Forward))

name2 : Signal.Mailbox Content
name2 = Signal.mailbox (Content (toString init.sqLength) (Selection 0 0 Forward))

name3 : Signal.Mailbox Content
name3 = Signal.mailbox (Content (toString init.rectLength) (Selection 0 0 Forward))

name4 : Signal.Mailbox Content
name4 = Signal.mailbox (Content (toString init.rectWidth) (Selection 0 0 Forward))

name5 : Signal.Mailbox Content
name5 = Signal.mailbox (Content (toString init.ovalLength) (Selection 0 0 Forward))

name6 : Signal.Mailbox Content
name6 = Signal.mailbox (Content (toString init.ovalWidth) (Selection 0 0 Forward))

name7 : Signal.Mailbox Content
name7 = Signal.mailbox (Content (toString init.ngonSize) (Selection 0 0 Forward))

name8 : Signal.Mailbox Content
name8 = Signal.mailbox (Content (toString init.ngonRadius) (Selection 0 0 Forward))

nameField : Signal Element
nameField = (field textBoxStyle (Signal.message name1.address) "radius" <~ name1.signal)
nf2 : Signal Element
nf2 = (field textBoxStyle (Signal.message name2.address) "length" <~ name2.signal)
nf3 : Signal Element
nf3 = (field textBoxStyle (Signal.message name3.address) "width" <~ name3.signal)
nf4 : Signal Element
nf4 = (field textBoxStyle (Signal.message name4.address) "height" <~ name4.signal)
nf5 : Signal Element
nf5 = (field textBoxStyle (Signal.message name5.address) "width" <~ name5.signal)
nf6 : Signal Element
nf6 = (field textBoxStyle (Signal.message name6.address) "height" <~ name6.signal)
nf7 : Signal Element
nf7 = (field textBoxStyle (Signal.message name7.address) "sides" <~ name7.signal)
nf8 : Signal Element
nf8 = (field textBoxStyle (Signal.message name8.address) "radius" <~ name8.signal)

textBoxStyle : Style
textBoxStyle = 
        { padding = (uniformly 1)
        , outline = { color = grey, width = uniformly 1, radius = 0 }--noOutline
        , highlight = { color = blue, width = 1 }
        , style = textStyle
        }
        
textStyle = 
        { typeface = []
        , height = Just 12
        , color = black
        , bold = False
        , italic = False
        , line = Nothing
        }

textboxMailbox  = Signal.mergeMany [ name1.signal
                                   , name2.signal
                                   , name3.signal
                                   , name4.signal
                                   , name5.signal
                                   , name6.signal
                                   , name7.signal
                                   , name8.signal
                                   --, name9.signal
                                   --, name10.signal
                                   --, name11.signal
                                   --, name12.signal
                                   ]

buttonMailbox : Signal.Mailbox String
buttonMailbox = Signal.mailbox "filled"

type Update = Mouse (Int,Int) | Click Bool | Button String | Typing Content | Run Time | Task TaskStatus | Movement (Int,Int) 
            | Quiz QuizStatus | Transformer1 Transformer.State | Transformer2 Transformer.State | Transformer3 Transformer.State
            | Transformer4 Transformer.State | Refresh Time-- | DemoMode TimeOut.State

mouseInput : Signal Update
mouseInput = Signal.sampleOn (Signal.map Click Mouse.isDown) (Signal.map Mouse Mouse.position) 

combinedInput : Signal Update
combinedInput = Signal.mergeMany 
                         [ mouseInput 
                         , (Signal.map Button buttonMailbox.signal) 
                         , (Signal.map Typing textboxMailbox) 
                         , (Signal.map Click Mouse.isDown)
                         , (Signal.map Task clickDone.signal)
                         , (Signal.map Run (every second))
                         , (Signal.map Movement Mouse.position)
                         , (Signal.map Quiz quizMailbox.signal)
                         , (Signal.map Transformer1 t1Step)
                         , (Signal.map Transformer2 t2Step)
                         , (Signal.map Refresh (every second))
                         --, (Signal.map DemoMode timeoutStep)
                         ]

csStep = (Signal.foldp ColourPickerM.update init.cpState ColourPickerM.input)

t1Display = (Transformer.view <~ t1Step ~ Transformer.nf9 ~ Transformer.nf10 ~ Transformer.nf11 ~ Transformer.nf12 ~ buttonMailbox.signal)
t1Step = 
  let
    t1State = init.t1State -- {t1State | horShift <- (-5), vertShift <- 10, degrees <- 0, scaleFactor <- 1}
  in
    (Signal.foldp Transformer.update t1State Transformer.combinedInput)

t2Display = (Transformer.view <~ t2Step ~ Transformer.nf13 ~ Transformer.nf14 ~ Transformer.nf15 ~ Transformer.nf16 ~ buttonMailbox.signal)
t2Step = 
  let
    t2State = init.t2State --{t2State | horShift <- 0, vertShift <- 0, degrees <- 45, scaleFactor <- 2, chooseMove <- False, chooseRotate <- True}
  in
    (Signal.foldp Transformer.update t2State Transformer.combinedInput2)

t3Display = (Transformer.view <~ t3Step ~ Transformer.nf17 ~ Transformer.nf18 ~ Transformer.nf19 ~ Transformer.nf20 ~ buttonMailbox.signal)
t3Step = (Signal.foldp Transformer.update init.t3State Transformer.combinedInput3)

t4Display = (Transformer.view <~ t4Step ~ Transformer.nf21 ~ Transformer.nf22 ~ Transformer.nf23 ~ Transformer.nf24 ~ buttonMailbox.signal)
t4Step = (Signal.foldp Transformer.update init.t4State Transformer.combinedInput4)

scStep = (Signal.foldp update init combinedInput)

timeoutStep = (Signal.foldp TimeOut.update init TimeOut.input)

main = view <~ scStep ~ (Signal.map ColourPickerM.view csStep) ~ csStep ~ Mouse.position ~nameField ~ nf2 ~ nf3 ~ nf4 ~ nf5 ~ nf6 ~ nf7 ~ nf8 ~ Transformer.nf9 ~ Transformer.nf10 ~ Transformer.nf11 ~ Transformer.nf12 ~ Transformer.nf13 ~ Transformer.nf14 ~ Transformer.nf15 ~ Transformer.nf16 ~ buttonMailbox.signal ~ timeoutStep
