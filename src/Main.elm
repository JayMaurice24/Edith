module Main exposing (..)

import Browser 
import Html exposing (Html, div,text, input)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput, on, keyCode,onClick, onEnter)
import Dict exposing (update)

main = 
    Browser.sandbox {init = init, update = update, view = view}


type alias Model =
    { lines : List Line
    , userInput : String
    , lineCounter : Int
    }
type alias Line = 
    {lineNumber : Int,
    text: String}

init : Model 
init = 
    {lines = []
    ,userInput = ""
    ,lineCounter = 1}

type Msg 
    =  UpdateInput String
    |  AddLine String


update: Msg -> Model -> Model 
update msg model = 
    case msg of 
        UpdateInput text -> 
            {model | userInput = text}
        
        AddLine text -> 
            let
                newLine = 
                    {lineNumber = model.lineCounter, text = text}
                newLines = 
                    model.lines ++ [newLine]
            in
            { model | lines = newLines, userInput = "", lineCounter = model.lineCounter + 1 }

view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "...", onInput UpdateInput, on "keydown" (keyCodeIs 13 >> onEnter) ] []
        , div []
            (List.map viewLine model.lines)
        ]


viewLine : Line -> Html Msg
viewLine line =
    div []
        [ text (toString line.lineNumber ++ ": " ++ line.text) ]


onEnter : Msg
onEnter =
    AddLine

keyCodeIs : Int -> Html.Attribute msg
keyCodeIs expectedCode =
    on "keydown" (Json.Decode.andThen decodeKeyCode keyCodeDecoder)
        |> Html.Events.stopPropagationDecoder
    Where
        keyCodeDecoder : Json.Decode.Decoder Int
        keyCodeDecoder =
            Json.Decode.field "keyCode" Json.Decode.int

decodeKeyCode : Int -> msg
decodeKeyCode code =
    if code == 13 then
        onEnter

    else
        ignore

{- Explanation:

    I've defined a new Line type that holds both the line number and the text entered by the user.
    The Model now contains lines (a list of Line), userInput (the current input in the input field), and lineCounter (to keep track of the line number).
    The Msg type now includes a new message AddLine String, which will be used to add a new line when the Enter key is pressed.
    The update function has been updated to handle the new message AddLine. When the Enter key is pressed, it creates a new line with the current lineCounter and the userInput, updates the lines list, and increments the lineCounter.
    The view function is modified to display the line numbers along with the text entered by the user using the viewLine function.
    The onEnter message is created to be triggered when the Enter key is pressed.
    A helper function keyCodeIs is used to detect when the Enter key (keyCode 13) is pressed and trigger the onEnter message.

Now, each line will be numbered, and pressing Enter will create a new line with the entered text.-}