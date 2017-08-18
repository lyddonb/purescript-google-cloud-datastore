module Test.Main where

import Prelude                           (Unit, discard)

import Control.Monad.Eff                 (Eff)
import Control.Monad.Eff.Console         (CONSOLE)
import Control.Monad.Eff.Exception       (EXCEPTION)
import GoogleCloud.Datastore             (DATASTORE)
import Test.Spec.Reporter.Console        (consoleReporter)
import Test.Spec.Runner                  (RunnerEffects, run)
import Test.DatastoreSpec as DatastoreSpec   
import Test.TransactionsSpec as TransactionsSpec


main :: forall eff. Eff (RunnerEffects (datastore :: DATASTORE, console :: CONSOLE, exception :: EXCEPTION | eff)) Unit
main = run [consoleReporter] do
  DatastoreSpec.spec
  TransactionsSpec.spec
