module Main exposing (..)

import Browser
import Html exposing (Html, div, button, text, pre)
import Html.Attributes exposing (style,value)
import Html.Events exposing (onClick)
import String exposing (lines)
import Browser.Navigation exposing (Key)
import Keyboard exposing (Key(..))
import Basics exposing (modBy)
import Keyboard exposing (subscriptions)
import Platform.Cmd as Cmd
import Keyboard exposing (update)
import File exposing (File)
import File.Select as Select
import Task
import String 
import File.Download as Download

type alias Model =
    { text : String
    , cursorPos : Int
    , lineNum : Int
    , lineNums: List Int
    , selectionStart : Maybe Int
    , selectionEnd : Maybe Int
    , keys : List Key
    , csv : Maybe String
    }

init : () -> (Model, Cmd Msg)
init _ =
    ({ text = ""
    , cursorPos = 0
    , lineNums = []
    , lineNum = 1 
    , keys = []
    , selectionStart = Nothing
    , selectionEnd = Nothing
    , csv = Nothing
    }, Cmd.none)
type Msg
    = 
    Key Keyboard.Msg
    | CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | Download 
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
        Key keyMsg ->
            let
                 k = Keyboard.update keyMsg []
    
            in case k of
                [ArrowDown] -> ({model | 
                                        cursorPos = moveCursor model.text (model.cursorPos + 120)}
                                        , Cmd.none)
                [ArrowRight] ->  ({model |
                                        cursorPos = moveCursor model.text (model.cursorPos + 1) }
                                        , Cmd.none)
                [ArrowLeft] ->  ({model |
                                        cursorPos = moveCursor model.text (model.cursorPos - 1 )}
                                        , Cmd.none)
                [ArrowUp] ->  ({model | 
                                        cursorPos = moveCursor model.text (model.cursorPos - 120)} 
                                        , Cmd.none)
                [Character ch] ->  (((model 
                                            |> (\m -> {m| 
                                            text = writeFrom m.text ch m.cursorPos
                                            , cursorPos = updateCursor m.text m.cursorPos
                                            })
                                            |> updateLineNum)), Cmd.none)
                [Spacebar] -> (((model 
                                        |> (\m -> {m| 
                                        text = writeFrom m.text " " m.cursorPos
                                        , cursorPos = updateCursor m.text m.cursorPos
                                        })
                                        |> updateLineNum)), Cmd.none)
                [Backspace] -> ({model | 
                                            text = deleteText model.text model.cursorPos, cursorPos = moveCursor model.text (model.cursorPos - 1)}
                                            , Cmd.none)
                [Enter] ->  (((model 
                                    |> (\m -> {m | 
                                    text = writeFrom m.text "\n" m.cursorPos, cursorPos = m.cursorPos + 1
                                    ,lineNum = m.lineNum + 1 
                                    })
                                    |> updateLineNum)), Cmd.none)
                [] ->  (model, Cmd.none)
                _ ->  (model, Cmd.none) 
                
        Download -> 
                    (model, Download.string "download.txt" "text/markdown" model.text)
        CsvRequested ->
                    ( model
                    , Select.file ["text/csv"] CsvSelected
                    )

        CsvSelected file ->
                    ( model
                    , Task.perform CsvLoaded (File.toString file)
                    )

        CsvLoaded content ->
                    ( { model | text = String.toUpper content ++ model.text}
                    , Cmd.none
                    )
                     

view : Model -> Html Msg
view model =
  case model.csv of
    Nothing ->
        div [style "padding" "20px", style "background" "black", style "color" "white"]
            [   button [ onClick CsvRequested ] [ text "Upload" ]
                ,button [ onClick Download ] [ text "Download" ]
                ,pre
            [ style "padding" "10px"
            ][
            div [style "display" "flex", style "flex-direction" "row"][
                div [style "margin-right" "10px"] (List.map (\num -> div [] [ text (String.fromInt num) ]) model.lineNums), div [] [text (formatText model)]]
            ]]


    Just content ->
        div [style "padding" "20px"]
            [   button  [onClick CsvRequested ] [ text "Upload" ]
                ,button [ onClick Download ] [ text "Download" ]
                ,pre
            [ style "padding" "10px"
            ][
            div [style "display" "flex", style "flex-direction" "row"][
                div [style "margin-right" "10px"] (List.map (\num -> div [] [ text (String.fromInt num) ]) model.lineNums), div [] [text (formatText model)]]
            ]]
formatText : Model -> String -- function to wrap text and keep line to a max of 120 characters 
formatText model =
    let 
        lines = 
            String.foldl (\c (formatted,count) -> 
                if (String.fromChar c == "\n") then 
                    (formatted ++ String.fromChar c, 1)
                else if count < 120 then 
                    (formatted ++ String.fromChar c, count + 1 )
     
                else 
                    (formatted ++"\n" ++ String.fromChar c , 1)
            )
            ("", 0) 
            model.text
    in case lines of 
        (formatted, _ ) -> (formatted |> insertCursor) (model.cursorPos) 

insertCursor : String  -> Int -> String --inserts cursor to text 
insertCursor text cursorPos = 
    String.left cursorPos text ++ "|" ++ String.dropLeft cursorPos text 

writeFrom : String -> String ->Int -> String --adds text from cursor position
writeFrom text c index = 
    String.left index text ++ c ++ String.dropLeft index text 

deleteText : String -> Int -> String --detelets text but dropping the letter/character at an index
deleteText text index =
    String.left (index - 1) text ++ String.dropLeft index text 
    
verifyRange : Int -> Int -> Int -> Int --Ensures the cursor is within bounds of the text 
verifyRange min max value = 
    if value < min then 
        min 
    else if value > max then 
        max
    else 
        value    

updateLineNum : Model -> Model --Lines are either updated base on character length for wrapped text and \n characters for new lines 
updateLineNum model = 
    let
        line = List.length (String.lines model.text) --
        l = ((String.length model.text) // 120)
        newLine =
            if ((l + line) > l) then --this is to ensure that when someone has pressed enter multiple times but the characters are still below 120, we don't have too many line numbers 
                line + l
            else
                line
    in model 
            |>(\m -> ({ m | lineNum = newLine}))
            |> updateLines

moveCursor: String -> Int -> Int
moveCursor text pos = 
    verifyRange 0 (String.length text + ((String.length text) // 120)) (pos)

updateCursor: String -> Int -> Int --the format text adds one to the count when adding a \n, so cursor position needs to be doubled to account for that)
updateCursor text cursor = 
    if (modBy 120 (String.length text) == 0) then
        cursor + 2 
    else 
        cursor + 1 


updateLines : Model -> Model --updates the list or range of lines after having updated the number of lines 
updateLines model = 
    {model | lineNums = List.range 1 model.lineNum }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map Key Keyboard.subscriptions

main : Program () Model Msg 
main = 
    Browser.element {init = init, update = update, view = view, subscriptions = subscriptions}