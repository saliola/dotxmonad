{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- after editing, running
--
--     xmonad --recompile
--     xmonad --restart

-- FEATURES:
-- - configured to work with gnome-panel:
--     - includes gnome-panel, but no launcher
--     - mod+b toggles displaying the panel (default behaviour)
--
-- - uses smartBorders:
--     - no border if there is only one window
--
-- - uses ResizableTall to allow for resizing windows:
--     - mod+s and mod+x for shrink/expand
--
-- - maximize keybinding (Layout.Maximize):
--     - mod+z zooms in on a window
--
-- - workspace navigation (Actions.CycleWS):
--     - mod+` toggle previously displayed workspace ignoring NSP (scratchpad)
--     - mod+ctrl+left/right switches to previous/next workspace
--     - mod+shift+left/right shifts focused window to previous/next workspace
--     - mod+f find a free workspace
--
-- - workspace navigation with vi-like keybindings (Actions.CycleWS):
--     - mod+ctrl+h/l switches to previous/next workspace
--     - mod+ctrl+H/L shifts focused window to previous/next workspace
--
-- - BoringWindows: skip boring windows
--     - mod+shift+B mark window as boring
--     - mod+shift+ctrl+B clear marks
--     - mod+j/mod+k cycle but skip boring windows
--
-- - Scratchpad: terminal
--     - mod+t spawns terminal, or brings terminal to current workspace, or
--       moves terminal to hidden workspace called NSP
--     - in order for this to work, there must be a gnome-terminal profile
--       called "scratchpad" and the title of the window must by "scratchpad";
--       I also like to disable the scrollbars and the menubar.
--
-- - PhysicalScreens : rebind keys to switch xinerama screens (screen 0 is to
--       the left of screen 1)
--     - mod+w : switch to screen 0
--     - mod+e : switch to screen 1
--     - mod+shift+w : send to screen 0
--     - mod+shift+e : send to screen 1
--
-- - ShellPrompt
--     - mod+shift+p : prompt to run a command
--
-- - WorkspacePrompt : for moving between named workspaces
--     - mod+ctrl+p : select workspace
--
-- - WindowPrompt : brings you to windows and windows to you
--     - mod+ctrl+g : Pops open a menu with window titles. Choose one, and you
--       will be taken to the corresponding workspace.
--     - mod+ctrl+b : Pops open a menu with window titles. Choose one, and it
--       will be dragged, kicking and screaming, into your current workspace.
--
-- - XMonadPrompt : a prompt from running XMonad commands
--     - mod+x : Pops open a menu with a list of XMonad commands (can be
--       modified to use a custom command list; see XMonadPromptC)

import Control.Monad (unless)
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BoringWindows as BoringWindows
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.Master
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.StackSet as StackSet
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)


main = do
    xmproc <- spawnPipe "xmobar /home/saliola/.xmonad/xmobarrc"
    xmonad $ myConfig xmproc

myManageHook :: [ManageHook]
myManageHook =  [
    -- float dialog boxes
      isDialog --> doCenterFloat
    -- float pop-up windows (browser pop-ups, for instance)
    , stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doCenterFloat
    , stringProperty "WM_NAME" =? "xmessage" --> doCenterFloat
    , isFullscreen --> doFullFloat
 ]

scratchpads = [
 -- run xterm, find it by title, place it in a floating window: the size is
 -- given as (StackSet.RationalRect l t w h), where h is height, w is width,
 -- t is distance from top, l is distance from left (percentage)
      NS "gnome-terminal"
         "gnome-terminal --window-with-profile=scratchpad --disable-factory --title scratchpad"
         (title =? "scratchpad")
         (customFloating $ StackSet.RationalRect (0) (7/10) (1) (3/10))
 -- run antidote, find by title, don't float
    , NS "antidote"
         "/home/saliola/Applications/bin/antidote"
         (title =? "Nouveau document - Antidote")
         nonFloating
 ]

manageNamedScratchPad :: ManageHook
manageNamedScratchPad = namedScratchpadManageHook scratchpads

myConfig xmproc = gnomeConfig
    { modMask = myModMask
    , XMonad.workspaces = ["mc(1)", "r2r(2)", "lrbs(3)", "imm3(4)", "nserc(5)"] ++ map show [6..8] ++ ["scratch(9)"] ++ ["NSP"]
    , layoutHook = boringWindows $ avoidStruts $ smartBorders $
        -- ThreeCol:
        --   first arg : number of windows in main window
        --   second arg: myDelta
        --   third arg : if positive, size of main window
        (
        myMasterGrid |||
        myMastersimpleTabbed |||
        myLatex |||
        myCode |||
        myTiled |||
        myTwoPane |||
        myThreeColumn |||
        myThreeColumnMid |||
        myMirrorTiled |||
        Full |||
        simpleTabbed
        )
    , manageHook = manageHook gnomeConfig <+> composeAll myManageHook <+> manageNamedScratchPad
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , borderWidth = myBorderWidth
    , focusFollowsMouse = True
    , logHook = myLogHook xmproc
    }
    `additionalKeys`
        [
        -- ResizableTile
          ((myModMask, xK_x), sendMessage MirrorShrink)
        , ((myModMask, xK_s), sendMessage MirrorExpand)
        -- Maximize
        , ((myModMask, xK_z), withFocused (sendMessage . maximizeRestore))
        -- CycleWS : move / shift to toggle previously displayed workspace
        , ((myModMask, xK_grave), toggleWS' ["NSP"])
        , ((myModMask .|. shiftMask, xK_grave), shiftToggleWS' ["NSP"] >> toggleWS)
        , ((myModMask .|. shiftMask, xK_grave), shiftToggleWS' ["NSP"] >> toggleWS)
        -- CycleWS : workspace cycling
        , ((myModMask .|. controlMask, xK_Left), prevWS)
        , ((myModMask .|. controlMask, xK_Right), nextWS)
        , ((myModMask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
        , ((myModMask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
        -- CycleWS : workspace cycling with vi-like keybindings
        , ((myModMask .|. controlMask, xK_h), prevWS)
        , ((myModMask .|. controlMask, xK_l), nextWS)
        , ((myModMask .|. controlMask .|. shiftMask, xK_h), shiftToPrev >> prevWS)
        , ((myModMask .|. controlMask .|. shiftMask, xK_l), shiftToNext >> nextWS)
        -- FindEmptyWorkspace : move / shift to next empty workspace
        , ((myModMask, xK_f), viewEmptyWorkspace)
        , ((myModMask .|. shiftMask, xK_f), tagToEmptyWorkspace)
        -- BoringWindows
        , ((myModMask, xK_k), BoringWindows.focusUp)
        , ((myModMask, xK_j), BoringWindows.focusDown)
        , ((myModMask, xK_m), BoringWindows.focusMaster)
        , ((myModMask .|. shiftMask, xK_b), BoringWindows.markBoring)
        , ((myModMask .|. shiftMask .|. controlMask, xK_b), BoringWindows.clearBoring)
        -- Application launcher dmenu
        , ((myModMask, xK_p), spawn "dmenu_run")
        , ((myModMask, xK_space), spawn "dmenu_run")
        -- WindowShell: command launcher
        , ((myModMask .|. shiftMask, xK_p), shellPrompt defaultXPConfig { position = Top })
        , ((myModMask .|. shiftMask, xK_space), shellPrompt defaultXPConfig { position = Top })
        -- Push window back into tiling (free up xK_t for scratchPad)
        , ((myModMask .|. shiftMask, xK_t), withFocused $ windows.StackSet.sink)
        -- scratchpad (floating terminal)
        , ((myModMask, xK_t), namedScratchpadAction scratchpads "gnome-terminal")
        -- antidote
        , ((myModMask, xK_a), namedScratchpadAction scratchpads "antidote")
        -- quiting / logging out
        , ((myModMask .|. shiftMask, xK_q), spawn "gnome-session-quit")
        -- xinerama (screen 0 is to the left of screen 1)
        , ((myModMask, xK_w), viewScreen 0)
        , ((myModMask .|. shiftMask, xK_w), sendToScreen 0)
        , ((myModMask, xK_e), viewScreen 1)
        , ((myModMask .|. shiftMask, xK_e), sendToScreen 1)
        -- WorkspacePrompt: select workspace
        , ((myModMask .|. controlMask, xK_p), workspacePrompt defaultXPConfig { position = Top, alwaysHighlight = True } (windows . StackSet.greedyView))
        -- WindowPrompt
        , ((myModMask .|. controlMask, xK_b), windowPromptBring defaultXPConfig { position = Top, alwaysHighlight = True })
        , ((myModMask .|. controlMask, xK_g), windowPromptGoto defaultXPConfig { autoComplete = Just 500000, position = Top, alwaysHighlight = True })
        -- XMonadPrompt: prompt for a XMonad command
        , ((myModMask .|. shiftMask, xK_x), xmonadPrompt defaultXPConfig { position = Top, alwaysHighlight = True })
        -- Combo
        , ((myModMask .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
        , ((myModMask .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Move L)
        , ((myModMask .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Move U)
        , ((myModMask .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Move D)
        -- Layout
        , ((myModMask .|. controlMask, xK_space), sendMessage NextLayout)
        , ((myModMask .|. controlMask .|. shiftMask, xK_space), sendMessage FirstLayout)
        ]
    where
        myModMask = mod1Mask
        tiled = ResizableTall nmaster myDelta myRatio []
        nmaster = 1
        -- myRatio = 0.61803399
        myRatio = 0.6
        myDelta = 3/100
        myNormalBorderColor  = "black"
        myFocusedBorderColor = "yellow"
        myBorderWidth = 4
        myMasterGrid = mastered myDelta (1/2) $ maximize Grid
        myMastersimpleTabbed = mastered myDelta (1/2) $ simpleTabbed
        myTiled = maximize tiled
        myTwoPane = TwoPane (3/100) (1/2)
        myThreeColumn = maximize (ThreeCol 1 myDelta (2/5))
        myThreeColumnMid = maximize (ThreeColMid 1 myDelta (2/5))
        myMirrorTiled = maximize (Mirror tiled)
        myLatex = mastered myDelta (1/2) $
                    windowNavigation (combineTwo (Mirror (TwoPane myDelta 0.4)) (simpleTabbed) (Full))
        myCode = mastered myDelta (1/2) $ maximize (Mirror (TwoPane myDelta 0.6))

myLogHook xmproc = dynamicLogWithPP xmobarPP
      {
        ppOutput  = hPutStrLn xmproc
      , ppTitle   = xmobarColor "purple" "" . shorten 50
      , ppCurrent = xmobarColor "orange" "" . wrap "[" "]"
      , ppVisible = xmobarColor "yellow" "" . wrap "(" ")"
      , ppHidden = xmobarColor "gray" ""
      , ppHiddenNoWindows = xmobarColor "#333333" ""
      , ppUrgent  = xmobarColor "red" ""
      }


-- Shift window to workspace displayed previously
-- Source: https://github.com/mariuswol/xmonad-config/blob/master/Utils.hs
shiftToggleWS' :: [WorkspaceId] -> X ()
shiftToggleWS' skips = do
    hs' <- cleanHiddens skips
    unless (null hs') (windows . StackSet.shift . StackSet.tag $ head hs')
    where
        -- | Copied from XMonad.Actions.CycleWS
        cleanHiddens :: [WorkspaceId] -> X [WindowSpace]
        cleanHiddens skips =  gets $ (flip skipTags) skips . StackSet.hidden . windowset
