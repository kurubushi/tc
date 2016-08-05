{-#LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleContexts#-}

import System.Environment
import System.IO
import Atom.Types
import Control.Monad
import System.IO.Unsafe
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Catch
import Control.Monad.Except (runExceptT)
import qualified Parser.Parser as P
import Ta.Ata.Types (Ata(..))
import qualified Ta.Ata.Types as Ata
import qualified Ta.Ata.Utils as Ata
import Ta.Nd.Types (Nd(..))
import qualified Ta.Nd.Types as Nd
import qualified Ta.Nd.Utils as Nd
import Tt.Tdtt.Types (Tdtt(..))
import qualified Tt.Tdtt.Types as Tdtt
import qualified Tt.Tdtt.Utils as Tdtt
import Data.Set (Set)
import qualified Set.Types as SS
import qualified Data.Set as S
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

main :: IO ()
main = do
  [filename] <- getArgs
  c <- readFile filename
  case P.parseProgram c of
    Right (m, execs) -> mapM_ (\e -> exec m e >> putStrLn "") execs
    Left e -> print e
  putStrLn "end."

exec :: P.EMaps -> P.Exec -> IO ()
exec ms ch = do
  putStr "Start!: "
  print ch
  case P.execTest ms ch of
    Left e -> putStrLn $ errorMsg e
    Right (size, mt) -> do
      putStrLn $ "\ttestNd stateset size: " ++ show size
      (maybe
       (putStrLn "True")
       (\t -> putStrLn "False. A counter example is,"
         >> putStrLn (showBTree t))
       mt)

errorMsg :: P.ProgramError -> String
errorMsg pe = "Error!: " ++ P.reson pe
