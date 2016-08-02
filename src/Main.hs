{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

import Graphics.Vty
import Data.Char (isDigit)
import Data.List (foldr, intercalate, isPrefixOf)
import Control.Lens
import Control.Lens.TH
import Control.Exception (finally)
import System.Environment (getArgs)
import Text.Highlighting.Kate (highlightAs, TokenType(..))
import Control.Concurrent (threadDelay)
import Control.Monad.State (execStateT, StateT, MonadState)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad (MonadPlus, mzero)

data Zipper =
  Zipper {
    _linesAbove :: [String],
    _linesBelow :: [String],
    _charsLeft  :: [Char],
    _charsRight :: [Char]
  }
makeLenses ''Zipper

zipperLines :: Zipper -> [String]
zipperLines z =
  reverse (map reverse (view linesAbove z)) ++
  [reverse (view charsLeft z) ++ view charsRight z] ++
  view linesBelow z

position :: Zipper -> (Int, Int)
position z =
  (length $ view charsLeft z, length $ view linesAbove z)

insert :: Char -> Zipper -> Zipper
insert c =
  over charsLeft $ (:) c

backspace :: Zipper -> Zipper
backspace z =
  case view charsLeft z of
    "" ->
      case view linesAbove z of
        [] -> z
        (line : lines) -> set charsLeft line (set linesAbove lines z)
    (_:xs) -> set charsLeft xs z

delete :: Zipper -> Zipper
delete z =
  case view charsRight z of
    "" ->
      case view linesBelow z of
        [] -> z
        (line : lines) -> set charsRight line (set linesBelow lines z)
    (_:xs) -> set charsRight xs z

newline :: Zipper -> Zipper
newline z =
  let
    new = view charsLeft z
    z' = over linesAbove ((:) new) z
  in
    set charsLeft "" z'

goUp :: Zipper -> Zipper
goUp z =
  case view linesAbove z of
    [] -> z
    (c:cs) ->
      let
        leftChars = view charsLeft z
        rightChars = view charsRight z
        belowLines = view linesBelow z
        n = length leftChars
        c' = reverse c
        leftChars' = reverse $ take n c'
        rightChars' = drop n c'
        line = reverse leftChars ++ rightChars
      in
        Zipper cs (line : belowLines) leftChars' rightChars'

goDown :: Zipper -> Zipper
goDown z =
  case view linesBelow z of
    [] -> z
    (c:cs) ->
      let
        leftChars = view charsLeft z
        rightChars = view charsRight z
        aboveLines = view linesAbove z
        n = length leftChars
        leftChars' = reverse $ take n c
        rightChars' = drop n c
        line = reverse rightChars ++ leftChars
      in
        Zipper (line : aboveLines) cs leftChars' rightChars'

goLeft :: Zipper -> Zipper
goLeft z =
  case view charsLeft z of
    "" ->
      case view linesAbove z of
        [] -> z
        (line : lines) ->
          let
            rightChars = view charsRight z
            belowLines = view linesBelow z
          in
            Zipper lines (rightChars : belowLines) line []
    (c:cs) ->
       set charsLeft cs $ over charsRight ((:) c) z

goRight :: Zipper -> Zipper
goRight z =
  case view charsRight z of
    "" ->
      case view linesBelow z of
        [] -> z
        (line : lines) ->
          let
            leftChars = view charsLeft z
            aboveLines = view linesAbove z
          in
            Zipper (leftChars : aboveLines) lines [] line
    (c:cs) ->
       set charsRight cs $ over charsLeft ((:) c) z

deleteLineAbove :: Zipper -> Zipper
deleteLineAbove z =
  case view linesAbove z of
    [] -> z
    (line : lines) -> set linesAbove lines z

deleteLine :: Zipper -> Zipper
deleteLine =
  deleteLineAbove . goDown

indent :: Zipper -> Zipper
indent =
  over charsLeft (flip (++) "  ")

unindent :: Zipper -> Zipper
unindent z =
  case reverse (view charsLeft z) of
    (' ' : ' ' : xs) -> set charsLeft (reverse xs) z
    _ -> z

commentOut :: Zipper -> Zipper
commentOut =
  over charsLeft (flip (++) "--")

uncommentOut :: Zipper -> Zipper
uncommentOut z =
  case reverse (view charsLeft z) of
    ('-' : '-' : xs) -> set charsLeft (reverse xs) z
    _ -> z

gotoRelative :: Int -> Zipper -> Zipper
gotoRelative 0 z = z
gotoRelative n z | n > 0 =
  gotoRelative (n - 1) (goDown z)
gotoRelative n z | n < 0 =
  gotoRelative (n + 1) (goUp z)

goto :: Int -> Zipper -> Zipper
goto n z =
  let
    m = length (view linesAbove z) + 1
  in
    gotoRelative (n - m) z

match :: String -> Zipper -> Bool
match term z =
  term `isPrefixOf` view charsRight z

atLineStart :: Zipper -> Bool
atLineStart z =
  null (view charsLeft z)

atLineEnd :: Zipper -> Bool
atLineEnd z =
  null (view charsRight z)

atFileEnd :: Zipper -> Bool
atFileEnd z =
  null (view charsRight z) && null (view linesBelow z)

search :: String -> Zipper -> Zipper
search term z =
  let
    z' = goRight z
  in
    if atFileEnd z' || match term z' then
      z'
    else
      search term z'

gotoLineEnd :: Zipper -> Zipper
gotoLineEnd z =
  if atLineEnd z then
    z
  else
    gotoLineEnd $ goRight z

gotoLineStart :: Zipper -> Zipper
gotoLineStart z =
  if atLineStart z then
    z
  else
    gotoLineStart $ goLeft z

data State =
  State {
    _filename :: FilePath,
    _zipper :: Zipper,
    _lastSearch :: String
  }
makeLenses ''State

runMainLoop vty bounds state = do
  runMaybeT $ flip execStateT (bounds, state) loop
 where
  loop :: StateT ((Int, Int), State) (MaybeT IO) ()
  loop = do
    render

    e <- nextKeyEvent
    handleKeyEvent e
    loop
   where
    showN m n =
      let
        s = show n
      in
        (take (m - length s) $ repeat ' ') ++ s

    generatePicture :: (MonadState ((Int, Int), State) m) => m Picture
    generatePicture = do
      z <- use (_2 . zipper)
      let lines = zipperLines z
      let numLines = length lines

      let lineNumWidth = length (show numLines)
      let lineNumStyle = defAttr `withStyle` bold `withForeColor` yellow
      let lineNumbers = foldr1 (<->) $ map (string lineNumStyle . showN lineNumWidth) [1..numLines]

      let (x, y) = position z
      let text = vertCat $ map (\x -> string defAttr " " <|> string defAttr x) lines
      let image = lineNumbers <|> text

      let height = snd bounds
      let trimAmount = max 0 (y - (height `div` 2))

      let image' = translateY (-trimAmount) image

      let cursor = Cursor (x + lineNumWidth + 1) (y - trimAmount)
      return (picForImage image') { picCursor = cursor }

    render :: (MonadState ((Int, Int), State) m, MonadIO m) => m ()
    render = do
      picture <- generatePicture
      liftIO $ update vty picture

    renderWithStatus status = do
      (width, height) <- use _1
      picture <- generatePicture
      let statusBar = translateY (height - 1) $ string defAttr (status ++ (take width $ repeat ' '))
      let cursor = Cursor (length status) (height - 1)
      liftIO $ update vty (picture `addToTop` statusBar) { picCursor = cursor }

    nextKeyEvent :: (MonadState ((Int, Int), State) m, MonadIO m) => m (Key, [Modifier])
    nextKeyEvent = do
      e <- liftIO $ nextEvent vty
      case e of
        EvKey k m -> return (k, m)
        EvResize width height -> do
          _1 .= (width, height)
          render
          nextKeyEvent
        _ -> do
          liftIO $ print ("unknown event type " ++ show e)
          nextKeyEvent

    handleGoto n = do
      renderWithStatus ("goto: " ++ if n == 0 then "" else show n)

      e <- nextKeyEvent
      case e of
        (KChar c, []) | isDigit c -> handleGoto (n * 10 + read [c])
        (KBS, []) -> handleGoto (n `div` 10)
        (KEnter, []) -> (_2 . zipper) %= goto n
        _ -> return ()

    handleSearch term = do
      renderWithStatus ("search: " ++ reverse term)

      e <- nextKeyEvent
      case e of
        (KChar c, []) -> handleSearch (c:term)
        (KBS, []) -> handleSearch (drop 1 term)
        (KEnter, []) -> do
          let term' = reverse term
          (_2 . lastSearch) .= term'
          (_2 . zipper) %= search term'
        _ -> return ()

    handleKeyEvent :: (MonadState ((Int, Int), State) m, MonadIO m, MonadPlus m) => (Key, [Modifier]) -> m ()
    handleKeyEvent (KChar 'q', [MCtrl]) =
      mzero
    handleKeyEvent (KChar 's', [MCtrl]) = do
      z <- use (_2 . zipper)
      name <- use (_2 . filename)
      liftIO $ writeFile name $ intercalate "\n" $ zipperLines z
    handleKeyEvent (KChar 'd', [MCtrl]) =
      (_2 . zipper) %= deleteLine
    handleKeyEvent (KChar '\t', []) =
      (_2 . zipper) %= indent
    handleKeyEvent (KBackTab, []) =
      (_2 . zipper) %= unindent
    handleKeyEvent (KChar 'c', [MCtrl]) =
      (_2 . zipper) %= commentOut
    handleKeyEvent (KChar 'g', [MCtrl]) =
      (_2 . zipper) %= uncommentOut
    handleKeyEvent (KChar 'l', [MCtrl]) =
      handleGoto 0
    handleKeyEvent (KChar 'f', [MCtrl]) =
      handleSearch []
    handleKeyEvent (KChar 'n', [MCtrl]) = do
      term <- use (_2 . lastSearch)
      (_2 . zipper) %= search term
    handleKeyEvent (KChar 'e', [MCtrl]) =
      (_2 . zipper) %= gotoLineEnd
    handleKeyEvent (KChar 'a', [MCtrl]) =
      (_2 . zipper) %= gotoLineStart
    handleKeyEvent (KChar x, []) =
      (_2 . zipper) %= (insert x)
    handleKeyEvent (KUp, []) =
      (_2 . zipper) %= goUp
    handleKeyEvent (KDown, []) =
      (_2 . zipper) %= goDown
    handleKeyEvent (KLeft, []) =
      (_2 . zipper) %= goLeft
    handleKeyEvent (KRight, []) =
      (_2 . zipper) %= goRight
    handleKeyEvent (KBS, []) =
      (_2 . zipper) %= backspace
    handleKeyEvent (KDel, []) =
      (_2 . zipper) %= delete
    handleKeyEvent (KEnter, []) =
      (_2 . zipper) %= newline
    handleKeyEvent e = do
      liftIO $ print ("unknown event " ++ show e)

loadState [filename] = do
  l <- lines <$> readFile filename
  let zipper =
        case l of
          [] -> Zipper [] [] [] []
          (x:xs) -> Zipper [] xs [] x
  return $ State filename zipper ""

main = do
  args <- getArgs
  state <- loadState args
  cfg <- standardIOConfig
  vty <- mkVty cfg
  bounds <- displayBounds $ outputIface vty

  finally
    (runMainLoop vty bounds state)
    (shutdown vty)