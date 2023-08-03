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
    | MoveCursor CursorDirection
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
        MoveCursor direction -> 
            (model 
                |> (\m -> {m | cursorPos = moveCursor m.cursorPos direction m.text})
            , Cmd.none
            )

        Key keyMsg ->
            let
                 k = Keyboard.update keyMsg []
    
            in case k of
                [ArrowDown] -> ({model | cursorPos = model.cursorPos + 120 }, Cmd.none)
                [ArrowRight] ->  ({model | cursorPos = model.cursorPos - 120 }, Cmd.none)
                [ArrowLeft] ->  ({model | cursorPos = model.cursorPos - 1}, Cmd.none)
                [ArrowUp] ->  ({model | cursorPos = model.cursorPos + 1}, Cmd.none)
                [Character ch] ->  ({model | text = model.text ++ ch}, Cmd.none)
                [Spacebar] -> ({model | text = model.text ++ " " }, Cmd.none)
                [] ->  (model, Cmd.none)
                _ ->  (model, Cmd.none)
            


moveCursor : Int -> CursorDirection -> String -> Int 
moveCursor currPos direction text = 
    let
        lns = String.lines text
        (row, col) = getCursorRowAndCol text currPos 
    in case direction of 
        Left -> 
            if col > 0 then 
                currPos - 1 
            else if row > 0 then
                let 
                    prev = 
                        case List.drop (row - 1 ) lns of 
                            [] -> ""
                            line :: _ -> ""          
                in
                currPos - 1 - String.length prev
            else 
                currPos
        Right -> 
            let
                currLine = 
                    case List.drop row lns of 
                    [] -> 
                        case lns |> List.reverse |> List.head of 
                            Just last -> last 
                            Nothing -> ""
                    next :: _ -> next
            in
            if col < String.length currLine then 
                currPos + 1 
            else if row < List.length lns - 1 then 
                currPos + 1 + String.length (
                    case List.drop (row + 1) lns of 
                        [] -> ""
                        line :: _ -> line)
            else 
                currPos

        Up -> 
            if row > 0 then 
                let
                    prev = 
                        case List.drop (row - 1 ) lns of 
                            [] -> ""
                            line :: _ -> line
                            
                    newCol = min col (String.length prev)
                in
                currPos - col + newCol
            else 
                currPos
        Down -> 
            if row < List.length lns - 1 then 
                let
                    next =
                        case List.drop (row + 1) lns of 
                            [] -> ""
                            line :: _ ->  line
  
                    newCol = min col (String.length next)    
                in
                currPos +  String.length (
                        case List.drop (row + 1) lns of 
                            [] -> ""
                            line :: _ -> line)
                
                     + newCol
            else 
                currPos
        None -> 
            currPos
getCursorRowAndCol : String -> Int -> (Int, Int)
getCursorRowAndCol text cursorPos = 
    let 
        lns = String.lines text 
        (row, col) = 
            case List.drop cursorPos lns of 
                [] -> 
                    ( case lns |> List.reverse |> List.head of 
                        Just lastline -> List.length lns - 1
                        Nothing -> 0
                    , String.length (case List.head (List.drop (cursorPos - 1) lns) of 
                                Just l -> l
                                Nothing -> "")
                    )
                next :: _ -> (cursorPos + 1, 0) 
    in 
    (row, col) 


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
        (formatted, _) -> formatted 

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map Key Keyboard.subscriptions

main : Program () Model Msg 
main = 
    Browser.element {init = init, update = update, view = view, subscriptions = subscriptions}