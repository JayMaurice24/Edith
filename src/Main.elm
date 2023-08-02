module Main exposing (..)

import Browser
import Html exposing (Html, div, input, button, text, textarea, code)
import Html.Attributes exposing (placeholder, style,value, cols, rows)
import Html.Events exposing (onClick, onInput, on)
import String exposing (lines)
import Json.Decode as Decode
import Browser.Navigation exposing (Key)

type alias Model =
    { text : String
    , cursorPos : Int
    , lines : List Int
    , selectionStart : Maybe Int
    , selectionEnd : Maybe Int
    }

init : Model
init =
    { text = ""
    , cursorPos = 0
    , lines = []
    , selectionStart = Nothing
    , selectionEnd = Nothing
    }
type Msg
    = UpdateInput String
    | MoveCursor CursorDirection
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

update: Msg -> Model -> Model 
update msg model = 
    case msg of 
        UpdateInput nText -> 
            let
                updatedModel = {model | text = nText}
                updatedLines = splitLine 120 updatedModel.text
            in
            {updatedModel | lines = List.range 1 (List.length updatedLines)}
        
        MoveCursor direction -> 
            model 
                |> (\m -> {m | cursorPos = moveCursor m.cursorPos direction m.text})
                |> updateLineNum
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


splitText : Int -> String -> List String
splitText maxLineLength text =
    String.split "\n" text
        |> List.concatMap (splitLine maxLineLength)

splitLine : Int -> String -> List String
splitLine maxLineLength line =
    case String.slice 0 maxLineLength line of
        "" ->
            []

        chunk ->
            chunk :: splitLine maxLineLength (String.dropLeft (String.length chunk) line) 

lineNumber : String -> List Int 
lineNumber text = 
    String.lines text 
        |> List.indexedMap (\i _ -> i+1)

updateLineNum : Model -> Model 
updateLineNum model = 
    {model | lines = lineNumber model.text}

view : Model -> Html Msg
view model =
    div []
        [ div [ style "display" "flex", style "flex-direction" "row", style "padding" "10px" ]
            [ div [] (List.map (\num -> div [] [ text (String.fromInt num) ]) model.lines), div [] [ text (getVisibleText model)
            ]
        , input [ placeholder "Type your text", onInput UpdateInput, on "keyDown" keyDecoder ] []
        ]]

getVisibleText : Model ->   String
getVisibleText model =
    let
        lines = splitText 120 model.text
        (row, col) = getCursorRowAndCol model.text model.cursorPos
        indicator = if col >= 0 then String.repeat col " " ++ "|" else ""
    in
    String.join "\n" (List.indexedMap (\i line -> if i == row then line ++ indicator else line) lines)

keyDecoder : Decode.Decoder Msg 
keyDecoder = 
    Decode.map toKey (Decode.field "key" Decode.string)

toKey : String -> Msg
toKey keyValue = 
    case keyValue of 
        "ArrowLeft" -> MoveCursor Left
        "ArrowRight" -> MoveCursor Right 
        "ArrowDown" -> MoveCursor Down 
        "ArrowUp"   -> MoveCursor Up 
        _ -> MoveCursor None

main : Program () Model Msg 
main = 
    Browser.sandbox {init = init, update = update, view = view}