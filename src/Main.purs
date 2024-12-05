module Main
  ( Action(..)
  , component
  , main
  )
  where

import Prelude

import Data.Array (mapWithIndex, length, deleteAt)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Data.Maybe (Maybe(..))
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = Increment Int | Decrement Int | AddState | RemoveState Int | GlobalIncrement | GlobalDecrement

type AppState = Array Number

component :: forall q i o. H.Component q i o Aff
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
  where
  initialState :: i -> AppState
  initialState _ = [0.0]

  render :: AppState -> H.ComponentHTML Action () Aff
  render state =
    HH.div [] (
        [
            HH.button [ HE.onClick \_ -> AddState] [ HH.text "Add State"]
            , HH.br_
            , HH.br_
            , HH.button [ HE.onClick \_ -> GlobalIncrement] [ HH.text "Global Increment"]
            , HH.button [ HE.onClick \_ -> GlobalDecrement] [ HH.text "Global Decrement"]
            , HH.br_
        ]
        <> mapWithIndex renderNumber state
    )


  renderNumber :: Int -> Number -> H.ComponentHTML Action () Aff
  renderNumber idx value =
    HH.div_ [
        HH.br_
        , HH.br_
        , HH.button [ HE.onClick \_ -> Decrement idx ] [ HH.text "-" ]
        , HH.div_ [ HH.text $ show value ]
        , HH.button [ HE.onClick \_ -> Increment idx] [ HH.text "+" ]
        , HH.br_
        , HH.br_
        , HH.button [ HE.onClick \_ -> RemoveState idx] [ HH.text "Remove State"]
        , HH.br_
        , HH.br_
        , HH.br_
    ]

  handleAction :: Action -> H.HalogenM AppState Action () o Aff Unit
  handleAction = case _ of
    Increment idx -> H.modify_ (\state -> mapWithIndex (\i x -> if i == idx then x + 1.0 else x ) state)
    Decrement idx -> H.modify_ (\state -> mapWithIndex (\i x -> if i == idx then x - 1.0 else x ) state)
    AddState -> H.modify_ (\state -> state <> [0.0])
    RemoveState idx -> H.modify_ (\state -> removeState idx state)
    GlobalIncrement -> H.modify_ (\state -> map (\x -> x + 1.0) state)
    GlobalDecrement -> H.modify_ (\state -> map (\x -> x - 1.0) state)

  removeState :: Int -> AppState -> AppState
  removeState idx state =
    if length state > 1 then
        case deleteAt idx state of
            Just newState -> newState
            Nothing -> state
    else state