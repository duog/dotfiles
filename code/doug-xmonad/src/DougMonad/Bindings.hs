
{-# language ViewPatterns, DeriveGeneric, DeriveAnyClass, RankNTypes, LambdaCase, GeneralizedNewtypeDeriving, RecursiveDo, TupleSections, FlexibleContexts, FlexibleInstances, OverloadedStrings #-}


module DougMonad.Bindings where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as ExtensibleState
import XMonad.Util.XUtils
import XMonad.Actions.Submap
import XMonad.Util.Font

import Control.Applicative
import Data.Functor
import Data.Coerce
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Monoid
import Data.Semigroup
import Data.Hashable
import GHC.Generics(Generic)
import System.IO
import System.Exit
import Control.Concurrent.MVar

import Data.Text.Lens
import Control.Lens
import Control.Monad.Writer.CPS
import Data.Text (Text)
import Data.Map.Monoidal.Strict (MonoidalMap)
import qualified Data.Map.Monoidal.Strict as MonoidalMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Name = Text

type BindingName = Name

type Action = (Window -> X ())

newtype BindingSet a = BindingSet ([ (Name, [(Input, a)])])

instance Semigroup (BindingSet a) where
  BindingSet x <> BindingSet y = BindingSet $ let
    x_map = Map.fromList x
    y_map = Map.fromList y
    y'_map = y_map `Map.difference` x_map
    x_section n = fromMaybe [] $ Map.lookup  n x_map
    y_section n = fromMaybe [] $ Map.lookup n y_map
    y'_section n = fromMaybe [] $ Map.lookup n y'_map
    in [ (n, x_section n <> y_section n ) | n <- Map.keys x_map ]
      ++ [ (n, y'_section n) | n <- Map.keys y'_map ]
      
instance Monoid (BindingSet a) where
  mempty = BindingSet Empty

newtype HashableKeyMask = HashableKeyMask KeyMask
  deriving (Eq, Ord, Read, Show)

instance Hashable HashableKeyMask where
  hashWithSalt s (HashableKeyMask km) = hashWithSalt s (toInteger km)

data Input
  = KeyInput HashableKeyMask KeySym
  | MouseInput HashableKeyMask Button

data NamedBoundAction = NamedBoundAction BindingName Input Action

-- newtype BindT m a = BindT (ReaderT (XConfig Layout) m a)
--   deriving (Functor, Applicative, Monad, MonadIO, MonadReader (XConfig Layout))

-- type Bind a = BindT Identity a

bindKey :: BindingName -> (KeyMask, KeySym) -> X () -> NamedBoundAction
bindKey name (m, s) a = NamedBoundAction name (KeyInput (coerce m) s) (const $ a)

bindMouse :: BindingName -> (KeyMask, Button) -> (Window -> X ()) -> NamedBoundAction
bindMouse name (m, b) f = NamedBoundAction name (MouseInput (coerce m) b) f

bindKeySubmap :: MonadIO m => BindingName -> (KeyMask, KeySym) -> BindingSet  NamedBoundAction -> m NamedBoundAction
bindKeySubmap name (m, k) bs = do
  let
    (key_binds, _, help) = foldBindings bs
  window_ref <- liftIO $ newMVar Nothing

  pure . NamedBoundAction name (KeyInput (coerce m) k) . const $ do
    showHelp window_ref name help
    submapDefault (hideHelp Nothing) (coerce key_binds)

makeWindow :: MVar (Maybe Window) -> Name -> BindingSet Name -> X Window
makeWindow window_ref title a@(BindingSet sections) = 
  liftIO (tryTakeMVar window_ref) >>= \case
    -- we need to make it!
    Just Nothing -> go >>= \w -> liftIO $ putMVar window_ref (Just w) $> w
    -- it's already made, put it back
    Just (Just w) -> liftIO $ putMVar window_ref (Just w) $> w
    -- it's being made, wait for it to be ready
    Nothing -> liftIO $ fmap fromJust $ readMVar window_ref
  where
    go = do
      d <- asks display
      let
        font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
        (aligns, strs) = unzip $
          [(AlignCenter, "Help for: " <> title)] ++
          msum
            [ (AlignCenter, "section: " <> s) :
              [(AlignLeft, helpLine i n)
              | (i, n) <- lines]
            | (s, lines) <- sections
            ]
      f <- initXMF font
      width <- liftIO $ fmap (fi . fold) . traverse (\(Text s) ->fmap Max . textWidthXMF d f $ s) $ strs
      height <-  fmap (fi . fold) . traverse (\(Text s) -> fmap (\(a,b) -> Sum a <> Sum b) . liftIO . textExtentsXMF f $ s) $ strs
      let 
        border_width = 0 
        bg_colour = "black"
        colour = "white"
      window <- createNewWindow (Rectangle 100 100 width height) Nothing bg_colour True
      paintAndWrite window f width height border_width bg_colour bg_colour colour bg_colour aligns (strs ^.. traverse . unpacked)
      releaseXMF f
      pure window
    helpLine i n = "help line"

showHelp :: MVar (Maybe Window) -> Name -> BindingSet Name -> X ()
showHelp window_ref name bindings = do
  window <- (liftIO $ readMVar window_ref) >>= maybe (makeWindow window_ref name bindings) pure
  hideHelp (Just window)
  showWindow window

hideHelp :: Maybe Window -> X ()
hideHelp mb_window = do
  BindingsState mb_active_window <- ExtensibleState.get
  for_ mb_active_window hideWindow
  ExtensibleState.put mb_window

data BindingsState = BindingsState (Maybe (Window))

instance ExtensionClass BindingsState where
  initialValue = BindingsState Nothing

-- bindingSet :: [NamedBoundAction] -> BindingSet
-- bindingSet = foldMap $ \(NamedBoundAction n m s a) -> 
  -- HashMap.fromList . fmap (\a@(NamedBoundAction _ b _) -> (b, a))

foldBindings :: BindingSet NamedBoundAction -> (MonoidalMap (KeyMask, KeySym) (X ()), MonoidalMap (KeyMask, Button) (Window -> X ()), BindingSet Name)
foldBindings = foldMap $ \(section,  NamedBoundAction name i a) -> let
  doc = BindingMap [(section, [(i, name)])]
  the_action = \w -> a w *> hideHelp
  in case i of
    KeyInput m s ->
      ( MonoidalMap.singleton (coerce m, s) (a ignores_window)
      , mempty
      , doc)
    MouseInput m b ->
      ( mempty
      , MonoidalMap.singleton (coerce m, b) a
      , doc)
  where
    ignores_window f = f $ error "ignores_window"

type HelpKey = Text

withBindings :: (XConfig Layout -> BindingSet NamedBoundAction) -> XConfig Layout -> (XConfig Layout)
withBindings mk_bs old_conf = let
  get_bindings = fmap foldBindings mk_bs
  my_keys = fmap (view $ _1 . coerced) get_bindings
  my_mouse = fmap (view $ _2 . coerced) get_bindings
  -- my_event_handler = _ -- show help on modkey down
  in old_conf { keys = my_keys, mouseBindings = my_mouse } -- startup hook for 


defaultKeyBindings :: XConfig Layout -> BindingSet NamedBoundAction
defaultKeyBindings = do 
  modm <- asks modMask
  myterm <- asks terminal
  layout_hook <- asks layoutHook
  conf <- ask
  pure BindingSet $
    [ ("Applications",) $
      [ bindKey "Launch Terminal" (modm .|. shiftMask, xK_Return) $ spawn myterm -- %! Launch terminal
      , bindKey "Launch dmenu" (modm, xK_p) $ spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"" -- %! Launch dmenu
      , bindKey "Launch gmrun" (modm .|. shiftMask, xK_p) $ spawn "gmrun" -- %! Launch gmrun
      , bindKey "Close the focused window" (modm .|. shiftMask, xK_c) kill -- %! Close the focused window
      ]
    , ("Layouts",) $
      [ bindKey "Next layout" (modm, xK_space ) $ sendMessage NextLayout -- %! Rotate through the available layout algorithms
      , bindKey "Reset the layout" (modm .|. shiftMask, xK_space) $ setLayout layout_hook -- %!  Reset the layouts on the current workspace to default
      , bindKey "Refresh" (modm, xK_n) refresh -- %! Resize viewed windows to the correct size
      ]
    , ("Windows",) $
      [ bindKey "Focus down" (modm, xK_Tab) $ windows W.focusDown -- %! Move focus to the next window
      , bindKey "Focus up" (modm .|. shiftMask, xK_Tab) $ windows W.focusUp -- %! Move focus to the previous window
      , bindKey "Focus down" (modm, xK_j) $ windows W.focusDown -- %! Move focus to the next window
      , bindKey "Focus up" (modm, xK_k) $ windows W.focusUp-- %! Move focus to the previous window
      , bindKey "Focus the master" (modm, xK_m) $ windows W.focusMaster -- %! Move focus to the master window
      , bindKey "Swap with the master" (modm, xK_Return) $ windows W.swapMaster -- %! Swap the focused window and the master window
      , bindKey "Swap down" (modm .|. shiftMask, xK_j) $ windows W.swapDown -- %! Swap the focused window with the next window
      , bindKey "Swap up" (modm .|. shiftMask, xK_k) $ windows W.swapUp -- %! Swap the focused window with the previous window
      , bindKey "Shrink master" (modm, xK_h) $ sendMessage Shrink -- %! Shrink the master area
      , bindKey "Expand master" (modm, xK_l) $ sendMessage Expand -- %! Expand the master area
      , bindKey "Push floating to tiled" (modm, xK_t) $ withFocused $ windows . W.sink -- %! Push window back into tiling
      , bindKey "Add a window to master" (modm, xK_comma) $ sendMessage (IncMasterN 1) -- %! Increment the number of windows in the master area
      , bindKey "Remove a window from master" (modm, xK_period) $ sendMessage (IncMasterN (-1)) -- %! Deincrement the number of windows in the master area
      ]
    , ("System",) $
      [ bindKey "Quit" (modm .|. shiftMask, xK_q) $ io (exitWith ExitSuccess) -- %! Quit xmonad
      , bindKey "Restart" (modm, xK_q) $ spawn "xmonad --recompile && xmonad --restart" -- %! Restart xmonad
      ]
    , ("Workspaces",) $
      [ bindKey (n <> Text (show n)) (m .|. modm, k) $ windows $ f i
      | (f, m, n) <-
        [ (W.greedyView, 0, "Switch to workspace ")
        , (W.shift, shiftMask, "Move client to workspace ")
        ]
      , (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      ]
    , ("Screens",) $
      [ bindKey (n <> Text (show sc)) (m .|. modm, key) $ screenWorkspace sc >>= flip whenJust (windows . f)
      | (f, m, n) <-
        [ (W.view, 0, "Switch to screen number ")
        , (W.shift, shiftMask, "Move client to screen number ")
        ]
      , (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      ]
    ]


-- flashName :: SWNConfig -> Rectangle -> [(a, Rectangle)] -> X ([(a, Rectangle)], Maybe (ShowWName a))
-- flashName c (Rectangle sx sy wh ht) wrs = do
--   d <- asks display
--   n <- withWindowSet (return . S.currentTag)
--   f <- initXMF (swn_font c)
--   width <- fmap (\w -> w + w `div` length n) $ textWidthXMF d f n
--   (as,ds) <- textExtentsXMF f n
--   let hight = as + ds
--       y     = fi sy + (fi ht - hight + 2) `div` 2
--       x     = fi sx + (fi wh - width + 2) `div` 2
--   w <- createNewWindow (Rectangle (fi x) (fi y) (fi width) (fi hight)) Nothing "" True
--   showWindow w
--   paintAndWrite w f (fi width) (fi hight) 0 (swn_bgcolor c) "" (swn_color c) (swn_bgcolor c) [AlignCenter] [n]
--   releaseXMF f
--   i <- startTimer (swn_fade c)
--   return (wrs, Just $ SWN False c $ Just (i,w))
