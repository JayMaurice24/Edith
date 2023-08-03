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
import Keyboard exposing (update)
import List exposing (map)
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
    = UpdateInput String
    | MoveCursor
    | Key Keyboard.Msg
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
        UpdateInput text->  
                (updateLineNum model, Cmd.none)
        MoveCursor -> 
            (model, Cmd.none)

        Key keyMsg ->
            let
                 k = Keyboard.update keyMsg []
    
            in case k of
                [ArrowDown] -> ({model | 
                                                        cursorPos = verifyRange 0 (String.length model.text) (model.cursorPos + 120)}
                                                        , Cmd.none)
                [ArrowRight] ->  ({model |
                                                        cursorPos = verifyRange 0 (String.length model.text) (model.cursorPos + 1) }
                                                        , Cmd.none)
                [ArrowLeft] ->  ({model |
                                                        cursorPos = verifyRange 0 (String.length model.text) (model.cursorPos - 1 )}
                                                        , Cmd.none)
                [ArrowUp] ->  ({model | 
                                                        cursorPos = verifyRange 0 (String.length model.text) (model.cursorPos - 120)} 
                                                        , Cmd.none)
                [Character ch] ->  (((model 
                                                        |> (\m -> {m| 
                                                        text = writeFrom m.text ch m.cursorPos
                                                        , cursorPos = m.cursorPos + 1
                                                        })
                                                        |> updateLineNum)), Cmd.none)
                [Spacebar] -> ({model | 
                                                        text = writeFrom model.text " " model.cursorPos, cursorPos = model.cursorPos + 1}
                                                        , Cmd.none)
                [Backspace] -> ({model | 
                                                        text = deleteText model.text model.cursorPos, cursorPos = model.cursorPos - 1 }
                                                        , Cmd.none)
                [Enter] ->  ({model | 
                                                        text = writeFrom model.text "\n" model.cursorPos, cursorPos = model.cursorPos + 1
                                                        ,lineNum = model.lineNum + 1  
                                                        }
                                                        , Cmd.none)
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
                    ( { model | csv = Just content }
                    , Cmd.none
                    )
                    


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
  case model.csv of
    Nothing ->
        div []
            [   button [ onClick CsvRequested ] [ text "Open" ]
                ,button [ onClick Download ] [ text "Save" ]
                ,pre
            [ style "padding" "10px"
            ][
            div [style "display" "flex", style "flex-direction" "row"][
                div [style "margin-right" "10px"] (List.map (\num -> div [] [ text (String.fromInt num) ]) model.lineNums), div [] [text (formatText model)]]
            ]]


    Just content ->
        div []
            [   button [ onClick CsvRequested ] [ text "Upload File" ]
                ,button [ onClick Download ] [ text "Download File" ]
                ,pre
            [ style "padding" "10px"
            ][
            div [style "display" "flex", style "flex-direction" "row"][
                div [style "margin-right" "10px"] (List.map (\num -> div [] [ text (String.fromInt num) ]) model.lineNums), div [] [text (formatText model)]]
            ]]
                --[text (String.append model.text content)]
formatText : Model -> String
formatText model =
    let 
        lines = 
            String.foldl (\c (formatted,count) -> 
                if count < 120 then 
                    (formatted ++ String.fromChar c, count + 1 )
                else 
                    (formatted ++ "\n", 0)
            )
            ("", 0) 
            model.text
    in case lines of 
        (formatted, _ ) -> (formatted |> insertCursor) (model.cursorPos) 
        
insertCursor : String  -> Int -> String 
insertCursor text cursorPos = 
    String.left cursorPos text ++ "|" ++ String.dropLeft cursorPos text 

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
updateLineNum : Model ->Model
updateLineNum model = 
    { model | lineNums = List.range 1 (List.length (String.lines model.text))}

    

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map Key Keyboard.subscriptions

main : Program () Model Msg 
main = 
    Browser.element {init = init, update = update, view = view, subscriptions = subscriptions}