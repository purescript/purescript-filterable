module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Filterable (filter, filterMap)
import Data.Identity (Identity(Identity))
import Data.Maybe (Maybe(..))
import Data.Witherable (wither)
import Test.Assert (ASSERT, assert)

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  log "Test filterableMaybe instance" *> do
    let pred x = if x > 5 then Just (x * 10) else Nothing
    assert $ filterMap pred (Just 6) == Just 60
    assert $ filterMap pred (Just 5) == Nothing
    assert $ filterMap pred Nothing == Nothing

    assert $ filter (_ > 5) (Just 6) == Just 6
    assert $ filter (_ > 5) (Just 5) == Nothing
    assert $ filter (_ > 5) Nothing == Nothing

  log "Test witherableMaybe instance" *> do
    let pred x = if x > 5 then Identity (Just (x * 10)) else Identity Nothing
    assert $ wither pred (Just 6) == Identity (Just 60)
    assert $ wither pred (Just 5) == Identity Nothing
    assert $ wither pred Nothing == Identity Nothing

  log "All done!"
