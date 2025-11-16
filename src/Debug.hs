module Debug where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text

debugFile :: FilePath
debugFile = "debug.out"

debugInit :: IO ()
debugInit = liftIO $ writeFile debugFile ""

debug :: Text -> IO ()
debug = liftIO . appendFile debugFile . unpack

debugLn :: Text -> IO ()
debugLn msg = debug msg >> debug "\n"
