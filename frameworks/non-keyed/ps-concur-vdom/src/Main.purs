module Main where

import Prelude

import Concur.Core.Types (Widget)
import Concur.VDom.DOM as D
import Concur.VDom.Props as P
import Concur.VDom.Run (runWidgetInSelector)
import Concur.VDom.Types (HTML)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn5, runEffectFn5)

main :: Effect Unit
main = runWidgetInSelector "body" app

data Action
  = Create Int
  | AppendOneThousand
  | UpdateEveryTenth
  | Clear
  | Swap
  | Remove Int
  | Select Int

type State =
  { rows :: Array Row
  , lastId :: Int
  , selectedId :: Int
  }

type Row =
  { id :: Int
  , label :: String
  }

app :: forall a. Widget HTML a
app = do
  go initialState
  where
  go st = do
    axn <- render st
    newst <- liftEffect $ handleAction axn st
    go newst

  initialState :: State
  initialState = { rows: [], lastId: 0, selectedId: 0 }

  render :: State -> Widget HTML Action
  render state =
    D.node "div"
      [ P.attr "class" "container" ]
      [ jumbotron
      , D.node "table"
          [ P.attr "class" "table table-hover table-striped test-data" ]
          [ D.node "tbody" [] do
              map (renderRow state.selectedId) state.rows
          ]
      , footer
      ]

  handleAction :: forall a. Action -> State -> Effect State
  handleAction action state = case action of
    Create amount -> do
      newRows <- createRandomNRows amount state.lastId
      pure $ state { rows = newRows, lastId = state.lastId + amount }

    AppendOneThousand -> do
      let amount = 1000
      newRows <- createRandomNRows amount state.lastId
      pure $ state { rows = state.rows <> newRows, lastId = state.lastId + amount }

    UpdateEveryTenth -> do
      let
        updateLabel ix row =
          if ix `mod` 10 == 0 then row { label = row.label <> " !!!" } else row

      pure $ state { rows = Array.mapWithIndex updateLabel state.rows }

    Clear ->
      pure $ state { rows = [] }

    Swap -> do
      case swapRows state.rows 1 998 of
        Nothing -> pure state
        Just rows -> pure $ state { rows = rows }

    Remove id ->
      pure $ state { rows = Array.filter (\r -> r.id /= id) state.rows }

    Select id -> do
      if state.selectedId == id then
        pure state
      else
        pure $ state { selectedId = id }

type ActionButton = { id :: String, label :: String, action :: Action }

buttons :: Array ActionButton
buttons =
    [ { id: "run", label: "Create 1,000 rows", action: Create 1000 }
    , { id: "runlots", label: "Create 10,000 rows", action: Create 10000 }
    , { id: "add", label: "Append 1,000 rows", action: AppendOneThousand }
    , { id: "update", label: "Update every 10th row", action: UpdateEveryTenth }
    , { id: "clear", label: "Clear", action: Clear }
    , { id: "swaprows", label: "Swap Rows", action: Swap }
    ]

renderActionButton :: forall ps. ActionButton -> Widget HTML Action
renderActionButton { id, label, action } =
  D.node "div"
    [ P.attr "class" "col-sm-6 smallpad" ]
    [ D.node "button"
        [ P.attr "type" "button"
        , P.attr "class" "btn btn-primary btn-block"
        , P.attr "id" id
        -- , HP.attr (D.node "AttrName" "ref") "text"
        , action <$ P.handle "click"
        ]
        [ D.text label ]
    ]

renderRow :: forall a. Int -> Row -> Widget HTML Action
renderRow selectedId row =
  D.node "tr"
    (if selectedId == row.id then
      [ P.attr "class" "danger"
      , P.attr "selected" "true"
      ]
    else
      [ ]
    )
    [ D.node "td" colMd1 [ D.text (show row.id) ]
    , D.node "td" colMd4 [ D.node "a" [ Select row.id <$ P.handle "click" ] [ D.text row.label ] ]
    , D.node "td" colMd1 [ D.node "a" [ Remove row.id <$ P.handle "click" ] removeIcon ]
    , spacer
    ]

removeIcon :: forall a. Array (Widget HTML a)
removeIcon =
  [ D.node "span"
      [ P.attr "class" "glyphicon glyphicon-remove"
      , P.attr "aria-hidden" "true"
      ]
      []
  ]

colMd1 :: forall a. Array (P.VDomProps a)
colMd1 = [ P.attr "class" "col-md-1" ]

colMd4 :: forall a. Array (P.VDomProps a)
colMd4 = [ P.attr "class" "col-md-4" ]

spacer :: forall a. Widget HTML a
spacer = D.node "td" [ P.attr "class" "col-md-6" ] []

footer :: forall a. Widget HTML a
footer =
  D.node "span"
    [ P.attr "class" "preloadicon glyphicon glyphicon-remove"
    , P.attr "aria-hidden" "true"
    ]
    []

jumbotron :: Widget HTML Action
jumbotron =
  D.node "div"
    [ P.attr "class" "jumbotron" ]
    [ D.node "div"
        [ P.attr "class" "row" ]
        [ D.node "div"
            [ P.attr "class" "col-md-6" ]
            [ D.node "h1" [] [ D.text "Concur VDom 0.4.2 (non-keyed)" ]
            , D.node "div" [ P.attr "class" "col-md-6" ] do
                map renderActionButton buttons
            ]
        ]
    ]

updateEveryTenth :: Array Row -> Array Row
updateEveryTenth =
  Array.mapWithIndex updateRowLabel
  where
  updateRowLabel ix row =
    if ix `mod` 10 == 0 then row { label = row.label <> " !!!" } else row

swapRows :: Array Row -> Int -> Int -> Maybe (Array Row)
swapRows arr index1 index2 = do
  rowA <- arr Array.!! index1
  rowB <- arr Array.!! index2
  arrA <- Array.updateAt index1 rowB arr
  arrB <- Array.updateAt index2 rowA arrA
  pure arrB

foreign import createRandomNRowsImpl :: EffectFn5 (Array String) (Array String) (Array String) Int Int (Array Row)

createRandomNRows :: Int -> Int -> Effect (Array Row)
createRandomNRows n lastId = runEffectFn5 createRandomNRowsImpl adjectives colours nouns n lastId

adjectives :: Array String
adjectives =
    [ "pretty"
    , "large"
    , "big"
    , "small"
    , "tall"
    , "short"
    , "long"
    , "handsome"
    , "plain"
    , "quaint"
    , "clean"
    , "elegant"
    , "easy"
    , "angry"
    , "crazy"
    , "helpful"
    , "mushy"
    , "odd"
    , "unsightly"
    , "adorable"
    , "important"
    , "inexpensive"
    , "cheap"
    , "expensive"
    , "fancy"
    ]

colours :: Array String
colours =
    [ "red"
    , "yellow"
    , "blue"
    , "green"
    , "pink"
    , "brown"
    , "purple"
    , "brown"
    , "white"
    , "black"
    , "orange"
    ]

nouns :: Array String
nouns =
    [ "table"
    , "chair"
    , "house"
    , "bbq"
    , "desk"
    , "car"
    , "pony"
    , "cookie"
    , "sandwich"
    , "burger"
    , "pizza"
    , "mouse"
    , "keyboard"
    ]
