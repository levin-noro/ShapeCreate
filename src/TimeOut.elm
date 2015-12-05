module TimeOut where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (..)
import Signal exposing (..)
import Color exposing (..)
import String exposing (slice)
import Text exposing (..)
import Mouse
import Time exposing (..)
import ColourPickerM exposing (..)
import Transformer exposing (..)

--type alias State = 
--        { -- Dimensions
--          width : Int, height : Int
--        -- ShapeCreate
--        , pickFilled : Bool, pickOutlined : Bool, filledRed : Bool, filledGreen : Bool
--        , filledBlue : Bool, filledCustom : Bool, outlinedSolid : Bool
--        , outlinedDashed : Bool, outlinedDotted : Bool, outlinedBlack : Bool
--        , outlinedOrange : Bool, outlinedBlue : Bool, outlinedCustom : Bool
--        , pickCircle : Bool, pickSquare : Bool, pickRect : Bool, pickOval : Bool
--        , pickNgon : Bool, chooseMove : Bool, chooseRotate : Bool
--        , chooseScale : Bool, transformer : Bool
--        , currTextbox : String
--        , circRadius : Float, sqLength : Float, rectLength : Float, rectWidth : Float
--        , ovalLength : Float, ovalWidth : Float, ngonSize : Int , ngonRadius : Float
--        , cpState : ColourPickerM.State, t1State : Transformer.State, t2State : Transformer.State 
--        , t3State : Transformer.State, t4State : Transformer.State
--        , formStyleCode : String, formColourCode : String, formCode : String
--        , outStyleCode : String, outColourCode : String
--        , shapeCode : String, totalCode : String
--        -- Timer
--        , counter : Float, timer : Float, timeLimit : Float
--        -- Transformer
--        , currTrans : String
--        }

--init : State 
--init = let
--        t1 = Transformer.init
--        t2 = Transformer.init
--       in
--        { width = 768, height = 768 
--        -- ShapeCreate
--        , pickFilled = True, pickOutlined = False, filledRed = True, filledGreen = False
--        , filledBlue = False, filledCustom = False, outlinedSolid = True
--        , outlinedDashed = False, outlinedDotted = False, outlinedBlack = True
--        , outlinedOrange = False, outlinedBlue = False, outlinedCustom = False 
--        , pickCircle = True, pickSquare = False, pickRect = False, pickOval = False
--        , pickNgon = False, chooseMove = True, chooseRotate = False
--        , chooseScale = False, transformer = True
--        , currTextbox = "degrees"
--        , circRadius = 45, sqLength = 75, rectLength = 40, rectWidth = 60
--        , ovalLength = 70, ovalWidth = 90, ngonSize =5 , ngonRadius = 50
--        , cpState = ColourPickerM.init
--        , t1State = {t1 |  horShift <- (-195), vertShift <- 65, degrees <- 0}
--        , t2State = {t2 | degrees <- 45, scaleFactor <- 2}
--        , t3State = Transformer.init, t4State = Transformer.init
--        , formStyleCode = "filled", formColourCode = "red" , formCode = "filled red (circle 45)"
--        , outStyleCode = " (solid", outColourCode = "black)"
--        , shapeCode = "(circle 45)", totalCode = "filled red (circle 45)"
--        -- Timer
--        , counter = 60, timer = 0, timeLimit = 60
--        -- Transformer
--        , currTrans = ""
        
--        }

reset m =
    let
        t1State = m.t1State
        t2State = m.t2State
    in 
        { m | -- ShapeCreate
          pickFilled <- True, pickOutlined <- False, filledRed <- True, filledGreen <- False
        , filledBlue <- False, filledCustom <- False, outlinedSolid <- True
        , outlinedDashed <- False, outlinedDotted <- False, outlinedBlack <- True
        , outlinedOrange <- False, outlinedBlue <- False, outlinedCustom <- False 
        , pickCircle <- True, pickSquare <- False, pickRect <- False, pickOval <- False
        , pickNgon <- False, chooseMove <- False, chooseRotate <- False
        , chooseScale <- False
        , t1State <- {t1State | chooseMove <- False, chooseRotate <- False, chooseScale <- False}
        , t2State <- {t2State | chooseMove <- False, chooseRotate <- False, chooseScale <- False}
        , transformer <- False
        --, currTextbox <- "degrees"
        --, circRadius <- 45, sqLength <- 75, rectLength <- 40, rectWidth <- 60
        --, ovalLength <- 70, ovalWidth <- 90, ngonSize <-5 , ngonRadius <- 50
        --, cpState <- ColourPickerM.init, t1State <- Transformer.init, t2State <- Transformer.init
        --, t3State <- Transformer.init, t4State <- Transformer.init 
        ---- Timer
        --, counter <- 0, timeLimit <- 10, timer <- 0, task <- Done
        ---- Transformer
        --, currTrans <- "" 
        }

type Update = Run Time | Click Bool

input = Signal.merge (Signal.map Click Mouse.isDown) (Signal.map Run (every second))

update input m =
    case input of
        Click bool -> resetTimer m
        Run time -> timeoutUpdate (incrementTimer m)

resetTimer m = {m | counter <- 0, timer <- 0}

incrementTimer m = if | m.counter >= m.timeLimit -> {m | counter <- m.counter + 1, timer <- m.timer + 1}
                      | otherwise -> {m | counter <- m.counter + 1, timer <- 0}

timeoutUpdate m = let
                    t1 = m.t1State
                    t2 = m.t2State
                  in
                    if  | m.timer >= 60 -> {m | counter <- 60, timer <- 0}
                        | m.timer >= 58 -> {m | t2State <- {t2 | chooseRotate <- True}, currTrans <- "transformer 2"}
                        | m.timer >= 56 -> {m | t2State <- {t2 | chooseScale <- False}, currTrans <- "transformer 2"}
                        | m.timer >= 54 -> {m | t2State <- {t2 | chooseScale <- True}, currTrans <- "transformer 2"}
                        --| m.timer >= 54 -> {m | t2State <- {t2 | chooseMove <- True}, currTrans <- "transformer 2"}
                        --| m.timer >= 54 -> {m | t1State <- {t1 | chooseMove <- False, chooseRotate <- True}, currTrans <- "transformer 1"}
                        | m.timer >= 52 -> {m | t1State <- {t1 | chooseMove <- True}, currTrans <- "transformer 1"}
                        ---------------------------------------------------------------------------------------------------------------------------------
                        | m.timer >= 50 -> {m | pickOutlined <- False, pickFilled <- True, outlinedBlue <- False, outlinedBlack <- True, outlinedDotted <- False, outlinedSolid <- True, pickNgon <- False, pickRect <- True }
                        | m.timer >= 48 -> {m | outlinedOrange <- False, outlinedBlue <- True}
                        | m.timer >= 46 -> {m | outlinedBlack <- False, outlinedOrange <- True}
                        ---------------------------------------------------------------------------------------------------------------------------------
                        | m.timer >= 44 -> {m | outlinedDashed <- False, outlinedDotted <- True, outlinedBlue <- False, outlinedBlack <- True}
                        | m.timer >= 42 -> {m | outlinedOrange <- False, outlinedBlue <- True}
                        | m.timer >= 40 -> {m | outlinedBlack <- False, outlinedOrange <- True}
                        ---------------------------------------------------------------------------------------------------------------------------------
                        | m.timer >= 38 -> {m | outlinedSolid <- False, outlinedDashed <- True, outlinedBlue <- False, outlinedBlack <- True}
                        | m.timer >= 36 -> {m | outlinedOrange <- False, outlinedBlue <- True}
                        | m.timer >= 34 -> {m | outlinedBlack <- False, outlinedOrange <- True}
                        ---------------------------------------------------------------------------------------------------------------------------------
                        | m.timer >= 32 -> {m | pickFilled <- False, pickOutlined <- True, outlinedSolid <- True, filledBlue <- False, filledRed <- True}--, counter <- m.counter}
                        | m.timer >= 30 -> {m | filledGreen <- False, filledBlue <- True}
                        | m.timer >= 28 -> {m | filledRed <- False, filledGreen <- True}
                        ---------------------------------------------------------------------------------------------------------------------------------
                        | m.timer >= 26 -> {m | pickOval <- False, pickNgon <- True, filledBlue <- False, filledRed <- True}
                        | m.timer >= 24 -> {m | filledGreen <- False, filledBlue <- True}--, counter <- m.counter}
                        | m.timer >= 22 -> {m | filledRed <- False, filledGreen <- True}
                        ---------------------------------------------------------------------------------------------------------------------------------
                        | m.timer >= 20 -> {m | pickRect <- False, pickOval <- True, filledBlue <- False, filledRed <- True}--, counter <- m.counter}
                        | m.timer >= 18 -> {m | filledGreen <- False, filledBlue <- True}--, counter <- m.counter}
                        | m.timer >= 16 -> {m | filledRed <- False, filledGreen <- True}--, counter <- m.counter}
                        ---------------------------------------------------------------------------------------------------------------------------------
                        | m.timer >= 14 -> {m | pickSquare <- False, pickRect <- True, filledBlue <- False, filledRed <- True}--, counter <- m.counter}
                        | m.timer >= 12 -> {m | filledGreen <- False, filledBlue <- True}--, counter <- m.counter}
                        | m.timer >= 10 -> {m | filledRed <- False, filledGreen <- True}--, counter <- m.counter}
                        ---------------------------------------------------------------------------------------------------------------------------------
                        | m.timer >= 8 -> {m | pickCircle <- False, pickSquare <- True, filledBlue <- False, filledRed <- True}--, counter <- m.counter}
                        | m.timer >= 6 -> {m | filledGreen <- False, filledBlue <- True}--, counter <- m.counter}
                        --| m.timer >= 4 -> {m | circRadius <- 60}
                        | m.timer >= 3 -> {m | filledRed <- False, filledGreen <- True}--, counter <- m.counter}--
                        --s| m.timer >  -> m--{m | counter <- m.counter}
                        | m.timer >= 0 -> reset m    
