module Main exposing (..)

import Browser
import Html exposing (Html, div, input, button, text, textarea, code, pre)
import Html.Attributes exposing (placeholder, style,value, cols, rows)
import Html.Events exposing (onClick, onInput, on)
import String exposing (lines)
import Json.Decode as Decode
import Browser.Navigation exposing (Key)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Keyboard exposing (subscriptions)
import Platform.Cmd as Cmd
import String exposing (slice)
import Html exposing (aside)
import Html.Attributes exposing (for)

type alias Model =
    { text : String
    , cursorPos : Int
    , lineNum : Int
    , lines : List Int
    , selectionStart : Maybe Int
    , selectionEnd : Maybe Int
    , keys : List Key
    }

init : () -> (Model, Cmd Msg)
init _ =
    ({ text = ""
    , cursorPos = 0
    , lines = []
    , lineNum = 1 
    , keys = []
    , selectionStart = Nothing
    , selectionEnd = Nothing
    }, Cmd.none)
type Msg
    = UpdateInput String
    | MoveCursor
    | Key Keyboard.Msg
    {-
    | SelectText Int
    | DeleteSelectedText
    | ReplaceSelectedText String-}

type CursorDirection
    = Left
    | Right
    | Up
    | Down
    | None

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        UpdateInput nText ->  
                ({model | text = nText}, Cmd.none)
        MoveCursor -> 
            (model, Cmd.none)

        Key keyMsg ->
            let
                 k = Keyboard.update keyMsg []
    
            in case k of
                [ArrowDown] -> (Debug.log "hi" {model | cursorPos = verifyRange 0 (String.length model.text) (model.cursorPos + 120)}, Cmd.none)
                [ArrowRight] ->  (Debug.log "hi" {model |cursorPos = verifyRange 0 (String.length model.text) (model.cursorPos + 1) }, Cmd.none)
                [ArrowLeft] ->  (Debug.log "hi" {model |cursorPos = verifyRange 0 (String.length model.text) (model.cursorPos - 1 )}, Cmd.none)
                [ArrowUp] ->  (Debug.log "hi" {model | cursorPos = verifyRange 0 (String.length model.text) (model.cursorPos - 120)}, Cmd.none)
                [Character ch] ->  (Debug.log "hi" {model | text = writeFrom model.text ch model.cursorPos, cursorPos = model.cursorPos + 1}, Cmd.none)
                [Spacebar] -> (Debug.log "hi" {model | text = writeFrom model.text " " model.cursorPos, cursorPos = model.cursorPos + 1 }, Cmd.none)
                [Backspace] -> (Debug.log "hi" {model | text = deleteText model.text model.cursorPos, cursorPos = model.cursorPos - 1 }, Cmd.none)
                [] ->  (model, Cmd.none)
                _ ->  (model, Cmd.none)
            


moveCursor : Int -> String -> String 
moveCursor index text =
    let 
        before = 
            String.left index text
        
        after = 
            String.dropLeft index text
    in 
    String.join (before ++ "|") [after] 

view : Model -> Html Msg
view model =
    div []
        [ pre
            [ style "padding" "10px"
            ]
            [text (formatText model)]
        ]

formatText : Model -> String
formatText model =
    let 
        lines = 
            String.foldl (\c (formatted,count) -> 
                if count < 120 then 
                    (formatted ++ String.fromChar c, count + 1 )
                else 
                    (formatted ++ "\n" ++ String.fromChar c, 1)
            )
            ("", 0) 
            model.text
    in case lines of 
        (formatted, _ ) -> (formatted |> insertCursor) model.cursorPos
        
insertCursor : String  -> Int -> String 
insertCursor text cursorPos = 
    String.left cursorPos text ++ "|" ++ String.dropLeft cursorPos text 

splitText : String -> List String 
splitText inputString = 
    let
        wordsList =
            String.words inputString

        (wrappedLines, currLine) =
            List.foldl (\word (lines, currentLine) ->
                let
                    newLine =
                        currentLine ++ " " ++ word
                in
                if String.length newLine <= 120 then
                    (lines, newLine)
                else
                    (lines ++ [currentLine], word)
            ) ([], "") wordsList
    in
    wrappedLines ++ [currLine]
writeFrom : String -> String ->Int -> String 
writeFrom text c index = 
    String.left index text ++ c ++ String.dropLeft index text 

deleteText : String -> Int -> String
deleteText text index = 
    String.left (index - 1) text ++ String.dropLeft index text 
    

verifyRange : comparable -> comparable -> comparable -> comparable
verifyRange min max value = 
    if value < min then 
        min 
    else if value > max then 
        max
    else 
        value    
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map Key Keyboard.subscriptions

main : Program () Model Msg 
main = 
    Browser.element {init = init, update = update, view = view, subscriptions = subscriptions}