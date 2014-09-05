-- :buffers or :ls ex command to list buffers.
module Yi.Keymap.Vim.Ex.Commands.Buffers
    ( parse
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.List
import qualified Data.Map as M
import           Data.Rope (fromString)
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Buffer.Basic
import           Yi.Buffer.Misc
import           Yi.Editor
import           Yi.Keymap
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types
import           Yi.Monad

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.try ( P.string "buffers") <|> P.try ( P.string "ls") <|> P.try ( P.string "files" )
    return $ Common.pureExCommand {
        cmdShow = "buffers"
      , cmdAction = EditorA $ withEditor printBuffers
      }



printBuffers :: EditorM ()
printBuffers = do
    -- TODO Don't keep recreating new buffers. Use a pre-existing one.
    --      See the cabal buffer used in Command.hs for an example.
    -- TODO Add some simple keymaps to the buffer, like <CR> to open the buffer?
    bufs <- gets buffers
    let bufLines = M.elems $ M.mapWithKey bufLine bufs
    if length bufLines > 1
      then withEditor . void $
             newBufferE (Left "Buffer list")
                        (fromString $ intercalate "\n" bufLines)
      else printMsgs bufLines
  where
    -- TODO shorten this name string perhaps.
    -- TODO Add more information: modified status, line number.
    bufLine (BufferRef bufNum) buf =
        intercalate "\t" [ show bufNum
                         , show . view identA $ buf
                         ]
