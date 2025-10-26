import MiniHM

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import GHC.Wasm.Prim

foreign export javascript "minihm" minihm :: JSString -> JSString

putLn :: String -> ExceptT String (Writer [String]) ()
putLn = lift . tell . pure

minihm = toJSString . go . fromJSString
  where
    go :: String -> String
    go s =
      unlines . execWriter $ do
        x <- runExceptT $ inferType s putLn throwE
        case x of
          Right _ -> pure ()
          Left err -> tell ["*** Caught exception!", err]

main :: IO ()
main = undefined
