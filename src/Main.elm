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
    , select: Bool 
    , lineNums: List Int
    , selectionEnd : Int
    , selectionStart : Int
    , selected : String 
    , keys : List Key
    , csv : Maybe String
    }

init : () -> (Model, Cmd Msg)
init _ =
    ({ text = ""
    , cursorPos = 0
    , lineNums = []
    , lineNum = 1 
    , selected = ""
    , select = False
    , keys = []
    , selectionEnd = 0
    , selectionStart = 0 
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
                if (List.head (List.reverse model.keys)) == Just Shift then
                let
                    updatedModel =
                        let
                            k = Keyboard.update keyMsg []
                            start = model.cursorPos
                            end = model.cursorPos + 1
                            left = startSelection model.text start --splits the left and adds [
                            right = endSelection model.text (moveCursor model.text 1) -- takes the right and adds ] in front
                        in
                        case k of
                            ArrowRight :: _ ->
                                let
                                    nRight = switchCharacters (end + 1) right  -- this is supposed to switch out the characters witht the next so ] and whatever comes after it 
                                    
                                in
                                {model | text = left ++ nRight} -- however the result is that it just adds all the strings together maybe find a bettet way to implement this
                                

                            Enter :: _ ->
                                { model | cursorPos = moveCursor model.text (model.cursorPos + 120), select = False, keys = List.append model.keys k }

                            _ ->
                                model
                in
                ({ updatedModel | selected = String.slice model.selectionStart model.selectionEnd model.text }, Cmd.none)
                else 
                let 
                    k = Keyboard.update keyMsg []
                in case k of
                    [ArrowDown] -> (Debug.log "Hi"{model | 
                                            cursorPos = moveCursor model.text (model.cursorPos + 120)}
                                            , Cmd.none)
                    [ArrowRight] ->  (Debug.log "Hi"{model |
                                            cursorPos = moveCursor model.text (model.cursorPos + 1) }
                                            , Cmd.none)
                    [ArrowLeft] ->  (Debug.log "Hi"{model |
                                            cursorPos = moveCursor model.text (model.cursorPos - 1 )}
                                            , Cmd.none)
                    [ArrowUp] ->  (Debug.log "Hi"{model | 
                                            cursorPos = moveCursor model.text (model.cursorPos - 120)} 
                                            , Cmd.none)
                    [Shift] ->  (Debug.log "Hi"{model | 
                                            keys = List.append model.keys k} 
                                            , Cmd.none)
                        

                    [Character ch] ->  (Debug.log "Hi"((model 
                                                |> (\m -> {m| 
                                                text = writeFrom m.text ch m.cursorPos
                                                , cursorPos = updateCursor m.text m.cursorPos,  keys = List.append model.keys k
                                                })
                                                |> updateLineNum)), Cmd.none)
                    [Spacebar] -> (Debug.log "Hi"((model 
                                            |> (\m -> {m| 
                                            text = writeFrom m.text " " m.cursorPos
                                            , cursorPos = updateCursor m.text m.cursorPos,  keys = List.append model.keys k
                                            })
                                            |> updateLineNum)), Cmd.none)
                    [Backspace] -> (Debug.log "Hi"{model | 
                                                text = deleteText model.text model.cursorPos
                                                , cursorPos = moveCursor model.text (model.cursorPos - 1)
                                    }, Cmd.none)
                    [Enter] ->  (Debug.log "Hi"((model 
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
        div [style "padding" "20px", style "background" "black", style "color" "white"]
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

selectText: String -> Int -> Int -> String  
selectText text start end = 
    let     
        right = String.dropLeft start text

        (before, after) = String.slice 0 end right 
            |> Tuple.pair (String.dropLeft end right)
    in 
    before ++ "]" ++ after 

switchCharacters : Int -> String -> String
switchCharacters index input =
    if index >= 0 && index < String.length input - 1 then
        let
            charAtIndex =
                String.slice index (index + 1) input

            charAtNextIndex =
                String.slice (index + 1) (index + 2) input
        in
        case (charAtIndex, charAtNextIndex) of
                (char1, char2) ->
                    let
                        beforeIndex =
                            String.left index input

                        afterIndex =
                            String.dropLeft (index + 2) input
                    in
                    beforeIndex ++ char2 ++ char1 ++ afterIndex
    else
        input 



startSelection :  String -> Int -> String 
startSelection text start =
     String.left start text ++ "[" 
endSelection :  String -> Int -> String 
endSelection text end =
     "]"++ String.dropLeft end text   

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