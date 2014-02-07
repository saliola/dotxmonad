{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- after editing, running
--
--     xmonad --recompile
--     xmonad --restart

-- NOTES:
-- - There is some code below from when I configured XMonad to integrate with
-- unity-2d-panel and unity-2d-launcher. But Ubuntu migrated from these to the
-- combined unity interface. I haven't looked into this yet, but it could be
-- cool to integrate Ubuntu's HUD with XMonad (as a launcher, at the very
-- least). I left the code dealing with creating a Gap for the launcher in case
-- I decide to include another launcher.

-- FEATURES:
-- - configured to work with gnome-panel:
--     - includes gnome-panel, but no launcher
--     - mod+b toggles displaying the panel (default behaviour)
-- - uses smartBorders:
--     - no border if there is only one window
-- - uses ResizableTall to allow for resizing windows:
--     - mod+s and mod+x for shrink/expand
-- - gvim keybinding:
--     - mod+g spans gvim
-- - maximize keybinding (Layout.Maximize):
--     - mod+z zooms in on a window
-- - workspace navigation (Actions.CycleWS):
--     - mod+` switches to previous workspace
--     - mod+ctrl+left/right switchs to previous/next workspace
--     - mod+shift+left/right shifts focused window to previous/next workspace
-- - BoringWindows: skip boring windows
--     - mod+shift+B mark window as boring
--     - mod+shift+ctrl+B clear marks
--     - mod+j/mod+k cycle but skip boring windows
-- - Gaps: create gap on left side for the Unity launcher
--     - mod+u toggle Unity launcher
--     - this is an artifact of when I configured this to work with the Unity launcher
-- - Scratchpad: terminal
--     - mod+t spawns terminal, or brings terminal to current workspace, or
--       moves terminal to hidden workspace called NSP
--     - in order for this to work, there must be a gnome-terminal profile
--       called "scratchpad" and the title of the window must by "scratchpad";
--       I also like to disable the scrollbars and the menubar.

import XMonad
import XMonad.Util.EZConfig
import XMonad.Config.Gnome
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Maximize
import XMonad.Actions.CycleWS
import XMonad.Layout.BoringWindows as BoringWindows
import XMonad.Layout.Tabbed
import XMonad.Layout.Gaps
import XMonad.Util.NamedScratchpad
import XMonad.StackSet as StackSet

main = xmonad $ myConfig

myManageHook :: [ManageHook]
-- EXAMPLE: how to tell XMonad to ignore a particular window
-- myManageHook =  [ className =? "Unity-2d-panel"    --> doIgnore
--                 , className =? "Unity-2d-launcher" --> doIgnore
--                 ]
myManageHook =  []

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

-- EXAMPLES OF SCRATCHPADS
-- scratchpads = [
--  -- run gvim, find by role, place it in a floating window
--     , NS "notes"
--          "gvim --role notes ~/notes.txt"
--          (role =? "notes")
--          (customFloating $ StackSet.RationalRect (1/6) (1/6) (2/3) (2/3))
--  -- run gvim, find by role, don't float
--      NS "notes" "gvim --role notes ~/notes.txt" (role =? "notes") nonFloating
--  ] where role = stringProperty "WM_WINDOW_ROLE"

manageNamedScratchPad :: ManageHook
manageNamedScratchPad = namedScratchpadManageHook scratchpads

myConfig = gnomeConfig
    { modMask = myModMask
    -- , layoutHook = gaps [(L, 66)] $ boringWindows $ avoidStruts $ smartBorders $
    , layoutHook = boringWindows $ avoidStruts $ smartBorders $
                (maximize tiled ||| maximize (Mirror tiled) ||| Full ||| simpleTabbed)
    , manageHook = manageHook gnomeConfig <+> composeAll myManageHook <+> manageNamedScratchPad
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , borderWidth = myBorderWidth
    }
    `additionalKeys`
        [
        -- ResizableTile
          ((myModMask, xK_x), sendMessage MirrorShrink)
        , ((myModMask, xK_s), sendMessage MirrorExpand)
        -- Maximize
        , ((myModMask, xK_z), withFocused (sendMessage . maximizeRestore))
        -- CycleWS
        , ((myModMask, xK_grave), toggleWS)
        , ((myModMask .|. controlMask, xK_Left), prevWS)
        , ((myModMask .|. controlMask, xK_Right), nextWS)
        , ((myModMask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
        , ((myModMask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
        -- BoringWindows
        , ((myModMask, xK_k), BoringWindows.focusUp)
        , ((myModMask, xK_j), BoringWindows.focusDown)
        , ((myModMask, xK_m), BoringWindows.focusMaster)
        , ((myModMask .|. shiftMask, xK_b), BoringWindows.markBoring)
        , ((myModMask .|. shiftMask .|. controlMask, xK_b), BoringWindows.clearBoring)
        -- Gaps
        , ((myModMask, xK_u), sendMessage $ ToggleGap L)
        -- Launch gvim
        , ((myModMask, xK_g), spawn "gvim")
        , ((myModMask, xK_p), spawn "dmenu_run")
        -- Push window back into tiling (free up xK_t for scratchPad)
        , ((myModMask .|. shiftMask, xK_t), withFocused $ windows.StackSet.sink)
        -- scratchpad
        , ((myModMask, xK_t), namedScratchpadAction scratchpads "gnome-terminal")
        , ((myModMask, xK_a), namedScratchpadAction scratchpads "antidote")
        -- quiting / logging out
        , ((myModMask .|. shiftMask, xK_q), spawn "gnome-session-quit")
        ]
    where
        myModMask = mod1Mask
        tiled = ResizableTall nmaster delta ratio []
        nmaster = 1
        -- ratio = 0.61803399
        ratio = 0.6
        delta = 3/100
        myNormalBorderColor  = "#7c7c7c"
        myFocusedBorderColor = "#ffb6b0"
        myBorderWidth = 2

