module Editor exposing (..)

import Browser
import Html exposing (Html, div, textarea, text, code, pre)
import Html.Attributes exposing (placeholder, value, style, cols, rows)
import Html.Events exposing (onClick, onInput, on, onMouseDown)
import String exposing (lines)
import Json.Decode as Decode
import Browser.Navigation exposing (Key)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Keyboard exposing (subscriptions)
import Platform.Cmd as Cmd
import File exposing (File)
import File.Select as Select
import Task
import File.Download as Download

type alias Model =
    { text : String
    , cursorPos : Int
    , lineNums : List Int
    , selection : Selection
    , csv : Maybe String
    }

type Selection =
    Selected { start : Int
    , end : Int
    }
    | NoSelection

init : () -> (Model, Cmd Msg)
init _ =
    ({ text = ""
     , cursorPos = 0
     , lineNums = []
     , selection = NoSelection
     , csv = Nothing
     }, Cmd.none)

type Msg
    = UpdateInput String
    | MoveCursor CursorDirection
    | Key Keyboard.Msg
    | CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | Download
    | SelectText Int
    | DeleteSelectedText
    | ReplaceSelectedText String

type CursorDirection
    = Left
    | Right
    | Up
    | Down

type LineNumMsg
    = LineNumClick Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateInput text ->
            ({ model | text = text, selection = NoSelection }, Cmd.none)

        MoveCursor direction ->
            let
                newPos =
                    case direction of
                        Left ->
                            model.cursorPos - 1

                        Right ->
                            model.cursorPos + 1

                        Up ->
                            model.cursorPos - 120

                        Down ->
                            model.cursorPos + 120
            in
            { model | cursorPos = verifyRange 0 (String.length model.text) newPos, selection = NoSelection }
                "!" []

        Key keyMsg ->
            let
                k =
                    Keyboard.update keyMsg []

                selectionAfterKey =
                    case k of
                        [ ArrowDown ] ->
                            NoSelection

                        [ ArrowRight ] ->
                            NoSelection

                        [ ArrowLeft ] ->
                            NoSelection

                        [ ArrowUp ] ->
                            NoSelection

                        [ Character ch ] ->
                            insertText model.selection.start model.selection.end ch model.text

                        [ Spacebar ] ->
                            insertText model.selection.start model.selection.end " " model.text

                        [ Backspace ] ->
                            deleteText model.selection.start model.selection.end model.text

                        [ Enter ] ->
                            insertText model.selection.start model.selection.end "\n" model.text

                        [] ->
                            NoSelection

                        _ ->
                            model.selection
            in
            ( { model | cursorPos = verifyRange 0 (String.length model.text) model.cursorPos, selection = selectionAfterKey }
            , Cmd.none
            )

        Download ->
            (model, Download.string "download.txt" "text/markdown" model.text)

        CsvRequested ->
            (model, Select.file ["text/csv"] CsvSelected)

        CsvSelected file ->
            (model, Task.perform CsvLoaded (File.toString file))

        CsvLoaded content ->
            ({ model | csv = Just content }, Cmd.none)

        SelectText start ->
            (model, { model | selection = { start = start, end = model.cursorPos } } "!" [])

        DeleteSelectedText ->
            ( { model | text = deleteText model.selection.start model.selection.end model.text, selection = NoSelection }
            , Cmd.none
            )

        ReplaceSelectedText replacement ->
            ( { model | text = replaceText model.selection.start model.selection.end replacement model.text, selection = NoSelection }
            , Cmd.none
            )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map Key Keyboard.subscriptions

view : Model -> Html Msg
view model =
    case model.csv of
        Nothing ->
            div []
                [ textarea [ placeholder "Type here", value model.text, onInput UpdateInput ] []
                , pre [ style "padding" "10px" ]
                    [ div [ style "display" "flex", style "flex-direction" "row" ]
                        [ div [ style "margin-right" "10px" ] (List.map viewLineNum model.lineNums)
                        , code [ onMouseDown (SelectText model.cursorPos) ] [ text model.text ]
                        ]
                    ]
                ]

        Just content ->
            div []
                [ textarea [ placeholder "Type here", value model.text, onInput UpdateInput ] []
                , pre [ style "padding" "10px" ]
                    [ div [ style "display" "flex", style "flex-direction" "row" ]
                        [ div [ style "margin-right" "10px" ] (List.indexedMap viewLineNum model.lineNums)
                        , code [ onMouseDown (SelectText model.cursorPos) ] [text model.text ]
                        ]
                    ]
                ]

viewLineNum : Int -> Int -> LineNumMsg
viewLineNum lineNumber _ =
    div [ onClick (LineNumClick lineNumber) ] [ text (String.fromInt lineNumber) ]

moveCursor : CursorDirection -> Msg
moveCursor direction =
    MoveCursor direction

selectText : Msg
selectText =
    SelectText 0

deleteSelectedText : Msg
deleteSelectedText =
    DeleteSelectedText

replaceSelectedText : String -> Msg
replaceSelectedText replacement =
    ReplaceSelectedText replacement

insertText : Int -> Int -> String -> String -> String
insertText start end textToInsert originalText =
    let
        (before, after) =
            String.slice start end originalText
    in
    before ++ textToInsert ++ after

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
        (formatted, _ ) -> formatted 
deleteText : Int -> Int -> String -> String
deleteText start end originalText =
    let
        (before, _) =
            String.slice 0 start originalText

        (_, after) =
            String.slice end (String.length originalText) originalText
    in
    before ++ after

replaceText : Int -> Int -> String -> String -> String
replaceText start end replacement originalText =
    let
        (before, _) =
            String.slice 0 start originalText

        (_, after) =
            String.slice end (String.length originalText) originalText
    in
    before ++ replacement ++ after

verifyRange : Int -> Int -> Int -> Int
verifyRange min max value =
    if value < min then
        min
    else if value > max then
        max
    else
        value

main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
