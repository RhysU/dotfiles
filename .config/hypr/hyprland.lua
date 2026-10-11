-- Hyprland 0.56 rendition of ~/.xmonad/xmonad.hs
-- Press SUPER + slash at any time to pop up a cheat sheet of every binding below.
--
-- Install on Ubuntu 26.10+, whose Hyprland reads Lua and whose archive has hyprshutdown:
--   sudo apt install brightnessctl grim hyprland hyprlock hyprshutdown slurp wmenu

local mod  = "SUPER"
local last = 9  -- Workspaces run 1 through last, as XMonad's do

-- Scale by pixel density: 2 above 200 PPI, 1.5 above 140, else 1
-- TODO Check a HiDPI panel, pinning a whole number should 1.5 look soft
hl.monitor({ output = "", mode = "preferred", position = "auto", scale = "auto" })

-- Save screenshots where gnome-screenshot did, not in grim's working directory
hl.env("GRIM_DEFAULT_DIR", os.getenv("HOME") .. "/Pictures")

hl.config({
    general = {
        layout      = "master",
        border_size = 1,
        gaps_in     = 0,
        gaps_out    = 0,
        col         = { active_border = "rgb(ff0000)", inactive_border = "rgb(bebebe)" },
    },
    -- Tall with avoidMaster: open new windows in the stack, above the focused one
    master = {
        mfact         = 0.5,
        new_status    = "slave",
        new_on_top    = true,
        new_on_active = "before",
    },
    animations = { enabled = false },
    misc = {
        disable_hyprland_logo     = true,
        disable_splash_rendering  = true,
        force_default_wallpaper   = 0,
        on_focus_under_fullscreen = 1,  -- Hand maximization to whatever gains focus
    },
})

-- Ignore maximize requests, since SUPER + backslash decides
hl.window_rule({
    name           = "suppress-maximize-events",
    match          = { class = ".*" },
    suppress_event = "maximize",
})

-- Cheat sheet

local sheet = {}

local function section(name)
    sheet[#sheet + 1] = ">> " .. name
end

local function note(keys, what)
    sheet[#sheet + 1] = string.format("%-30s %s", keys, what)
end

-- Bind KEYS to ACTION, noting WHAT it does on the cheat sheet
local function bind(keys, what, action, opts)
    local described = { description = what }
    for k, v in pairs(opts or {}) do described[k] = v end
    hl.bind(keys, action, described)
    note(keys, what)
end

-- Quote S for sh
local function quote(s)
    return "'" .. s:gsub("'", "'\\''") .. "'"
end

-- List every binding in wmenu, where typing narrows the list
local function show_keybindings()
    hl.exec_cmd("printf %s " .. quote(table.concat(sheet, "\n"))
        .. " | wmenu -i -l 30 -f 'monospace 11' -p Keys >/dev/null")
end

-- Actions

local exec   = hl.dsp.exec_cmd
local layout = hl.dsp.layout

-- Close applications in order, exiting abruptly only should hyprshutdown be missing
local quit = "hyprshutdown; [ $? = 127 ] && hyprctl dispatch 'hl.dsp.exit()'"

-- Run each dispatcher in turn
local function all(...)
    local steps = { ... }
    return function()
        for _, step in ipairs(steps) do hl.dispatch(step) end
    end
end

-- View workspace N on the focused screen, swapping screens when it shows on
-- another, as greedyView does.  Send the focused window to N, as shift does.
local view, send = {}, {}
for n = 1, last do
    view[n] = hl.dsp.focus({ workspace = n, on_current_monitor = true })
    send[n] = hl.dsp.window.move({ workspace = n, follow = false })
end

-- Workspace N holds no windows
local function empty(n)
    local ws = hl.get_workspace(n)
    return not ws or ws.windows == 0
end

-- Workspace N holds windows and shows on no screen
local function hidden(n)
    local ws = hl.get_workspace(n)
    return ws and ws.windows > 0 and not ws.visible
end

-- Nearest workspace WANT accepts, stepping DIR from the current one
local function seek(dir, want)
    local here = hl.get_active_workspace().id
    for i = 1, last - 1 do
        local n = (here - 1 + dir * i) % last + 1
        if want(n) then return n end
    end
end

-- View the workspace seek finds, as CycleWS.moveTo does
local function move_to(dir, want)
    return function()
        local n = seek(dir, want)
        if n then hl.dispatch(view[n]) end
    end
end

-- Carry the focused window along, as followTo does
local function follow_to(dir, want)
    return function()
        local n, w = seek(dir, want), hl.get_active_window()
        if not (n and w) then return end
        hl.dispatch(send[n])
        hl.dispatch(view[n])
        hl.dispatch(hl.dsp.focus({ window = w }))
    end
end

-- Screens ordered left to right, then top to bottom
local function screens()
    local ms = hl.get_monitors()
    table.sort(ms, function(a, b) return a.x < b.x or (a.x == b.x and a.y < b.y) end)
    return ms
end

-- Screen N, counting from one
local function nth(n)
    return function() return screens()[n] end
end

-- Screen STEP places from the focused one, wrapping
local function near(step)
    return function()
        local ms = screens()
        for i, m in ipairs(ms) do
            if m.focused then return ms[(i - 1 + step) % #ms + 1] end
        end
    end
end

-- Apply HOW to the screen WHICH picks, should there be one
local function screen(which, how)
    return function()
        local m = which()
        if m then hl.dispatch(how(m)) end
    end
end

local function focus(m) return hl.dsp.focus({ monitor = m }) end
local function shift(m) return hl.dsp.window.move({ monitor = m, follow = false }) end
local function swap(m)  return hl.dsp.workspace.swap_monitors({ monitor1 = "current", monitor2 = m }) end

-- Bindings, section by section as in xmonad.hs

bind(mod .. " + slash", "Show keybindings", show_keybindings)

section("Launchers")
bind(mod .. " + D",              "Run",         exec("wmenu-run"))
bind(mod .. " + SHIFT + Return", "Terminal",    exec("gnome-terminal"))
bind(mod .. " + B",              "Browser",     exec("xdg-open https://"))
bind(mod .. " + SHIFT + L",      "Lock screen", exec("hyprlock --config /usr/share/hypr/hyprlock.conf"))

-- A maximized window stands in for Full
section("Layout")
bind(mod .. " + backslash", "Maximize / restore", hl.dsp.window.fullscreen({ mode = "maximized" }))

section("Workspaces")
bind(mod .. " + F",            "Move to next empty workspace",           move_to(1, empty))
bind(mod .. " + Down",         "Move to next non-empty workspace",       move_to(1, hidden))
bind(mod .. " + Up",           "Move to previous non-empty workspace",   move_to(-1, hidden))
bind(mod .. " + SHIFT + F",    "Follow to next empty workspace",         follow_to(1, empty))
bind(mod .. " + SHIFT + Down", "Follow to next non-empty workspace",     follow_to(1, hidden))
bind(mod .. " + SHIFT + Up",   "Follow to previous non-empty workspace", follow_to(-1, hidden))
bind(mod .. " + Z",            "Toggle last workspace",                  hl.dsp.focus({ workspace = "previous_per_monitor" }))

section("Screens")
bind(mod .. " + Right",                "Focus next screen",         screen(near(1), focus))
bind(mod .. " + Left",                 "Focus previous screen",     screen(near(-1), focus))
bind(mod .. " + SHIFT + Right",        "Shift to next screen",      screen(near(1), shift))
bind(mod .. " + SHIFT + Left",         "Shift to previous screen",  screen(near(-1), shift))
bind(mod .. " + CTRL + SHIFT + Right", "Swap with next screen",     screen(near(1), swap))
bind(mod .. " + CTRL + SHIFT + Left",  "Swap with previous screen", screen(near(-1), swap))

section("Media")
local held = { locked = true, repeating = true }
bind("XF86AudioRaiseVolume",  "Volume up",       exec("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"), held)
bind("XF86AudioLowerVolume",  "Volume down",     exec("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"), held)
bind("XF86AudioMute",         "Toggle mute",     exec("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"), { locked = true })
bind("XF86MonBrightnessUp",   "Brightness up",   exec("brightnessctl set +5%"), held)
bind("XF86MonBrightnessDown", "Brightness down", exec("brightnessctl set 5%-"), held)

section("Screenshots")
bind("Print",          "Region screenshot", exec('grim -g "$(slurp)"'))
bind(mod .. " + Print", "Full screenshot",   exec("grim"))

section("Standard bindings")
bind(mod .. " + SHIFT + C",     "Close focused window",   hl.dsp.window.close())
bind(mod .. " + SHIFT + Q",     "Quit Hyprland",          exec(quit))
bind(mod .. " + space",         "Cycle layout",           layout("orientationcycle left top right bottom"))
bind(mod .. " + SHIFT + space", "Reset layout",           all(layout("orientationleft"), layout("mfact exact 0.5")))
bind(mod .. " + Tab",           "Focus next window",      hl.dsp.window.cycle_next({ tiled = true }))
bind(mod .. " + SHIFT + Tab",   "Focus prev window",      hl.dsp.window.cycle_next({ tiled = true, next = false }))
bind(mod .. " + J",             "Focus next in stack",    hl.dsp.window.cycle_next({ tiled = true }))
bind(mod .. " + K",             "Focus prev in stack",    hl.dsp.window.cycle_next({ tiled = true, next = false }))
bind(mod .. " + SHIFT + J",     "Swap with next",         layout("swapnext"))
bind(mod .. " + SHIFT + K",     "Swap with prev",         layout("swapprev"))
bind(mod .. " + Return",        "Promote to master",      layout("swapwithmaster ignoremaster"))
bind(mod .. " + H",             "Shrink master",          layout("mfact -0.03"))
bind(mod .. " + L",             "Expand master",          layout("mfact +0.03"))
bind(mod .. " + T",             "Sink floating window",   hl.dsp.window.float({ action = "disable" }))
bind(mod .. " + comma",         "Increment master count", layout("addmaster"))
bind(mod .. " + period",        "Decrement master count", layout("removemaster"))
bind(mod .. " + mouse:272",     "Move window",            hl.dsp.window.drag())
bind(mod .. " + mouse:273",     "Resize window",          hl.dsp.window.resize())

for n, key in ipairs({ "W", "E", "R" }) do
    bind(mod .. " + " .. key,         "Focus screen " .. n,          screen(nth(n), focus))
    bind(mod .. " + SHIFT + " .. key, "Move window to screen " .. n, screen(nth(n), shift))
end

-- Note one row for all of 1 through last, keeping the cheat sheet short
for n = 1, last do
    hl.bind(mod .. " + " .. n,         view[n], { description = "View workspace " .. n })
    hl.bind(mod .. " + SHIFT + " .. n, send[n], { description = "Move to workspace " .. n })
end
note(mod .. " + 1.." .. last,         "View workspace")
note(mod .. " + SHIFT + 1.." .. last, "Move to workspace")

-- Test plan: `Hyprland --verify-config` catches load-time mistakes only, so
-- press these in a live session with two screens and workspace 2 on the other
--   SUPER + slash                  every binding listed, typing narrows
--   new window, master focused     lands atop the stack, master unmoved
--   new window, stack focused      lands above the focused window
--   SUPER + 2                      screens swap workspaces
--   SUPER + Down                   skips 2, wraps past 9
--   SUPER + F                      next empty workspace, wrapping past 9
--   SUPER + SHIFT + F              window comes along and keeps focus
--   SUPER + Z twice                there and back
--   SUPER + E                      right-hand screen, whatever its id
--   SUPER + SHIFT + Right          window crosses, focus stays
--   SUPER + CTRL + SHIFT + Right   screens swap workspaces
--   SUPER + backslash, then J      next window takes over maximized
--   SUPER + space four times       master visits top, right, bottom, then left
--   SUPER + SHIFT + space          master left at half width
--   SUPER + SHIFT + L              locks, password unlocks
--   Print                          drag a region, PNG lands in ~/Pictures
