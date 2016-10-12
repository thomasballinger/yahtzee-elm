module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App


-- import List

import Dice
import Debug


-- MODEL
-- List.repeat 6 diceModel


type alias BoardModel =
    { diceModel : List Dice.Model
    }


initialModel : BoardModel
initialModel =
    { diceModel =
        [ Dice.initialModel
        , Dice.initialModel
        , Dice.initialModel
        , Dice.initialModel
        , Dice.initialModel
        ]
    }


init : ( BoardModel, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- MESSAGES


type Msg
    = DiceMsg Int Dice.Msg
    | RollDice



-- VIEW


view : BoardModel -> Html Msg
view model =
    div []
        ((model.diceModel
            |> List.indexedMap (\i dieModel -> Html.App.map (DiceMsg i) (Dice.view dieModel))
         )
            ++ [ button [ onClick RollDice ] [ text "Roll Dice" ] ]
        )



-- UPDATE


update : Msg -> BoardModel -> ( BoardModel, Cmd Msg )
update msg model =
    case msg of
        DiceMsg whichDie subMsg ->
            let
                ( updatedDiceModel, diceCmds ) =
                    List.unzip
                        (List.indexedMap
                            (\i dieModel ->
                                if i == whichDie then
                                    let
                                        ( newDie, cmd ) =
                                            Dice.update subMsg dieModel
                                    in
                                        ( newDie, ( i, cmd ) )
                                else
                                    ( dieModel, ( i, Cmd.none ) )
                            )
                            model.diceModel
                        )
            in
                ( { model | diceModel = updatedDiceModel }
                , Cmd.batch (List.map (\( i, cmd ) -> Cmd.map (DiceMsg i) cmd) diceCmds)
                )

        RollDice ->
            let
                ( updatedDiceModel, diceCmds ) =
                    List.unzip
                        (List.indexedMap
                            (\i dieModel ->
                                let
                                    ( newDie, cmd ) =
                                        Dice.update Dice.Roll dieModel
                                in
                                    ( newDie, ( i, cmd ) )
                            )
                            model.diceModel
                        )
            in
                ( { model | diceModel = updatedDiceModel }
                , Cmd.batch (List.map (\( i, cmd ) -> Cmd.map (DiceMsg i) cmd) diceCmds)
                )



-- SUBSCRIPTIONS


subscriptions : BoardModel -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
