#!/usr/bin/env luajit
--[[
Copyright 2020 Iweidieng Iep

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
]]

-- Outer sandboxing

-- Unbind `io` library
local io_write = io.write;
local io_flush = io.flush;
local io_close = io.close;
local io_open = io.open;
local io_lines = io.lines;
io = nil;

-- Unbind `os` library
local os_time = os.time;
local os_date = os.date;
os = nil;

-- BBS-Lua on Lua Extensions
-- Runtime version
local __RUNTIME = jit and jit.version or _VERSION;
jit = nil;
-- A getter-only debug library for convenience
local debug_bbslua = {
    getfenv = debug.getfenv,
    gethook = debug.gethook,
    getinfo = debug.getinfo,
    getlocal = debug.getlocal,
    getmetatable = debug.getmetatable,
    -- getregistry = debug.getregistry,  -- Useless in pure Lua and danger
    -- getupvalue = debug.getupvalue,  -- Too powerful and danger
    traceback = debug.traceback,
};
debug = debug_bbslua;
-- An empty package library so that users can define their own modules
local package_bbslua = {
    loaded = {
        package = package_bbslua,
        string = string,
        table = table,
        math = math,
        debug = debug_bbslua,
        bit = bit,
    },
    loaders = {function(mod) return preload[mod]; end},
    preload = {},
    seeall = package.seeall,
};
package = package_bbslua;

-- Unbind eval functions
local basic_load = load;
dofile = nil;
loadfile = nil;
load = nil;
loadstring = nil;

-- Free memory for unbound API
collectgarbage();

-- Cache library functions to immune to library modification by custom program
local coroutine_wrap = coroutine.wrap;
local coroutine_yield = coroutine.yield;
local coroutine_create = coroutine.create;
local coroutine_resume = coroutine.resume;

local string_match = string.match;
local string_sub = string.sub;
local string_byte = string.byte;
local string_char = string.char;
local string_gmatch = string.gmatch;
local string_gsub = string.gsub;
local string_format = string.format;
local string_rep = string.rep;

local table_concat = table.concat;
local table_remove = table.remove;
local table_insert = table.insert;

local math_abs = math.abs;
local math_max = math.max;
local math_fmod = math.fmod;
local math_modf = math.modf;
local math_min = math.min;
local math_random = math.random;
local math_randomseed = math.randomseed;

-- Make BitOp compatible with deprecated bitlib used by older BBS-Lua versions
local bit = require "bit";
bit.cast = bit.cast or bit.tobit;

local bnot = bit.bnot;
local band, bor, bxor = bit.band, bit.bor, bit.bxor;
local lshift, rshift, arshift = bit.lshift, bit.rshift, bit.arshift;
local bit_tohex = bit.tohex;

-- Utility

local function noop(...)
    return ...;
end

-- Object to string

local objstr;
local Weak_set;

local function idxstr(idx, lookup)
    lookup = lookup == nil and Weak_set{} or lookup;
    if type(idx) == "string" then
        if string_match(idx, "^[%a_][%w_]*$") then
            return idx;
        end
    end
    return "[" .. objstr(idx) .. "]";
end

local function tblstr(tbl, lookup)
    lookup = lookup == nil and Weak_set{} or lookup;
    local res = {};
    local idx = 0;
    for k, v in pairs(tbl) do
        local vstr = objstr(v, lookup);
        if k == idx + 1 then
            res[#res + 1] = vstr;
            idx = idx + 1;
        else
            res[#res + 1] = idxstr(k, lookup) .. " = " .. vstr;
        end
    end
    return "{" .. table_concat(res, ", ")  .. "}";
end

function objstr(obj, lookup)
    lookup = lookup == nil and Weak_set{} or lookup;
    if lookup and lookup[obj] then
        return (type(obj) == "table" and obj.classname or type(obj)) .. "<skipped>";
    end
    if type(obj) == "string" then
        -- Prevent escaped newlines in short strings
        if #obj <= 8 then
            return (string_gsub(string_format("%q", obj), "\\\n", "\\n"));
        end
        return string_format("%q", obj);
    end
    if type(obj) ~= "table" then
        return tostring(obj);
    end
    if getmetatable(obj) and getmetatable(obj).__tostring ~= nil then
        return getmetatable(obj).__tostring(obj, lookup);
    end
    if lookup then
        lookup[obj] = true;
    end
    return tblstr(obj, lookup);
end

-- String to object

local obj_from_str;

local function idx_from_str(str, i)
    i = i or 1;
    local idx, eq = string_match(string_sub(str, i), "^([%a_][%w_]*)( *= *)");
    if idx == nil then
        local lbracket = string_match(string_sub(str, i), "^%[");
        if lbracket == nil then
            return nil, i;
        end
        local res, i = obj_from_str(str, i + #lbracket);
        local rbracket = string_match(string_sub(str, i), "^%]");
        return res, i + #rbracket;
    end
    return idx, i + #idx + #eq;
end

local function tbl_from_str(str, i)
    i = i or 1;
    local res = {};
    local idx = 1;
    local open = string_match(string_sub(str, i), "^ *{ *");
    i = i + #open;
    local close = string_match(string_sub(str, i), "^ *} *");
    while not close do
        local key, val;
        key, i = idx_from_str(str, i);
        if key == nil then
            key = idx;
            idx = idx + 1;
        end
        val, i = obj_from_str(str, i);
        res[key] = val;
        local comma = string_match(string_sub(str, i), "^ *, *") or "";
        i = i + #comma;
        close = string_match(string_sub(str, i), "^ *} *");
    end
    return res, i + #close;
end

local str_esc_tbl = {
    a = "\a", b = "\b", f = "\f", n = "\n", r = "\r", t = "\t", v = "\v",
    [([[\]])] = [[\]], [([["]])] = [["]], [([[']])] = [[']],
    ["\n"] = "\n",
};
local function str_from_str(str, i)
    i = i or 1;
    local res = {};
    local open = string_match(string_sub(str, i), [[^ *"]]);
    i = i + #open;
    local close = string_match(string_sub(str, i), [[^"]]);
    while not close do
        local esc = string_match(string_sub(str, i), "^\\([abfnrtv\\\"\'\n])");
        if esc ~= nil then
            res[#res + 1] = str_esc_tbl(esc);
            i = i + #esc;
        else
            esc = string_match(string_sub(str, i), [[^\([0-9][0-9]?[0-9]?)]]);
            if esc ~= nil then
                res[#res + 1] = string_char(tonumber(esc));
                i = i + #esc;
            else
                esc = string_match(string_sub(str, i), [[^\x([0-9][0-9])]]);
                if esc ~= nil then
                    res[#res + 1] = string_char(tonumber(esc, 16));
                    i = i + #esc;
                else
                    res[#res + 1] = string_sub(str, i, i);
                    i = i + 1;
                end
            end
        end
        close = string_match(string_sub(str, i), [[^"]]);
    end
    return table_concat(res), i + #close;
end

local classes = {};  -- Need the class loaders
local word_obj_tbl = {
    nan = math_abs(0 / 0), ["-nan"] = -math_abs(0 / 0),
    inf = 1 / 0, ["-inf"] = (-1 / 0),
    ["true"] = true, ["false"] = false,
};
function obj_from_str(str, i)
    i = i or 1;
    local sp, classname = string_match(string_sub(str, i), [[^( *)([%a_][%w_]*) *{]]);
    if classname ~= nil then
        i = i + #sp + #classname;
        local classobj, objdef;
        if classname == "class" then
            local classdef;
            classdef, i = tbl_from_str(str, i);
            classobj = class(classdef);
            if not string_match(string_sub(str, i), "^ *{") then
                -- Return the anonymous class itself
                return classdef, i;
            end
        else
            classobj = classes[classname];
        end
        objdef, i = tbl_from_str(str, i);
        return classobj(objdef), i;
    end
    if string_match(string_sub(str, i), [[^ *"]]) then
        return str_from_str(str, i);
    end
    if string_match(string_sub(str, i), "^ *{") then
        return tbl_from_str(str, i);
    end
    for k, v in ipairs{
            [[^( *)(%-?[0-9]+[Ee]%-?[0-9]+)]],
            [[^( *)(%-?[0-9]*%.[0-9]+[Ee]%-?[0-9]+)]],
            [[^( *)(%-?[0-9]+%.[0-9]*)]],
            [[^( *)(%-?%.?[0-9]+)]]} do
        local sp, num = string_match(string_sub(str, i), v);
        if num ~= nil then
            return tonumber(num), i + #sp + #num;
        end
    end
    local sp, word = string_match(string_sub(str, i), [[^( *)(%-?[%a_][%w_]*)]]);
    if word ~= nil then
        i = i + #sp + #word;
        local obj = word_obj_tbl[word];
        if obj ~= nil then
            return obj, i + #sp + #word;
        end
        if word == "nil" then return nil, i + #sp + #word; end
    end
    error("Invalid object string at position " .. i .. " : " .. str);
end

-- Ranged `ipairs`
local ipairs_orig = ipairs;
local function ipairs(obj, i, j)
    local iter_f, obj, it = ipairs_orig(obj);
    it = i or it;
    return (j == nil and iter_f) or function(obj, i)
        if i >= j then
            return nil;
        end
        return iter_f(obj, i);
    end, obj, (it and (not j or it <= j)) and it or nil;
end

local function each_ch(str) return string_gmatch(str, "."); end
local function each_ch_utf8(str) return string_gmatch(str, ".[\128-\191]*"); end

local function unpack_str(str, i, j)
    local res = {};
    for ch in each_ch(string_sub(str, i or 1, j)) do
        res[#res + 1] = ch;
    end
    return unpack(res);
end

local function unpack_str_utf8(str, i, j)
    local res = {};
    i = i or 1;
    for ch in each_ch_utf8(str) do
        res[#res + 1] = ch;
    end
    j = (j and j < 0 and #res + 1 + j) or j;
    return unpack(res, i, j);
end

local function tbl_merge(self, rhs, keys)
    if keys ~= nil then
        rhs = getargs(rhs, keys);
        keys = arr_to_set(keys);
    end
    for k, v in pairs(rhs) do
        if keys == nil or keys[k] then
            self[k] = v;
        end
    end
    return self;
end

local function obj_copy(self)
    if type(self) == "table" then
        local res = {};
        for k, v in pairs(self) do
            res[k] = v;
        end
        setmetatable(res, getmetatable(self));
        return res;
    end
    if type(self) == "function" then
        return function(...) return self(...); end;
    end
    -- Cannot copy
    return self;
end

local function getargs(keys, ...)
    local res = {};
    local args = {...};
    if #args == 1 and type(args[1]) == "table" then
        args = args[1];
    end
    local kidx = 1;
    local idx = 0;
    for k, v in pairs(args) do
        if k == idx + 1 then
            while kidx <= #keys do
                local spec = keys[kidx];
                if type(spec) == "table" and #spec == 2 and type(spec[2]) == "string" then
                    if (type(v) == "table" and v.classname == spec[2]) or type(v) == spec[2] then
                        res[spec[1]] = v;
                        kidx = kidx + 1;
                        break;
                    end
                else
                    res[spec] = v;
                    kidx = kidx + 1;
                    break;
                end
                kidx = kidx + 1;
            end
            idx = idx + 1;
        else
            res[k] = v;
        end
    end
    return res;
end

local function arr_to_set(arr, i)
    local res = {};
    local idx = (i or 1) - 1;
    for k, v in pairs(arr) do
        if k == idx + 1 then
            res[v] = true;
            idx = idx + 1;
        else
            res[k] = v;
        end
    end
    return res;
end

local function zip(tb1, tb2)
    local res = {};
    for k = 1, math_max(#tb1, #tb2), 1 do
        res[k] = {tb1[k], tb2[k]};
    end
    return res;
end
local function zipiter(gen1, gen2)
    local iter_f1, obj1, it1 = unpack(gen1);
    local iter_f2, obj2, it2 = unpack(gen2);
    return function(obj, it)
        local res1 = {iter_f1(obj[1], it[1])};
        local res2 = {iter_f2(obj[2], it[2])};
        if res1[1] == nil and res2[1] == nil then
            return nil;
        end
        return unpack(zip(res1, res2));
    end, {obj1, obj2}, {it1, it2};
end

-- A simple class system
function class(def, is_local, lv)
    lv = lv or 1;
    if type(def) ~= "table" then
        -- `def` is the class name
        return function(def_, is_local_)
            def_.classname = def;
            -- Avoid tail call, otherwise `getfenv` on vanilla Lua will broken
            return unpack{class(def_, is_local_ or is_local, 2)};
        end;
    end
    def.__index = def;
    function def:__tostring(lookup)
        return tostring(def) .. tblstr(self, lookup);
    end
    setmetatable(def, {
        __call = function(...)
            local res;
            if def.new then
                res = def.new(...);
            else
                res = {select(2, ...)};
                if #res == 1 and type(res[1]) == "table" then
                    res = obj_copy(res[1]);
                end
            end
            setmetatable(res, def);
            return res;
        end,
        __tostring = function(self, lookup)
            return def.classname or "class" .. tblstr(self, lookup);
        end
    });
    if def.classname then
        classes[def.classname] = def;
        local env = getfenv(is_local and lv + 1 or 0);
        env[def.classname] = def;
        setfenv(is_local and lv + 1 or 0, env);
    end
    return def;
end
local function local_class(def)
    -- Avoid tail call, otherwise `getfenv` on Vanilla Lua will broken
    return unpack{class(def, true, 2)};
end

Weak_set = local_class "Weak_set"{__mode = "k"};

local_class "Mergable"{};
function Mergable:__concat(rhs)
    return tbl_merge(obj_copy(self), rhs);
end

-- A full, stand-alone BBS-Lua implementation for shell (requires LuaJIT)
local bbs;
local store_new;
local _fini_cb = {};
do
    local term_setup;
    local wait_input;
    local readall_or_block;
    local getwinsize;
    local mkdir;
    local getclocktp;
    local getpwd;
    do
        local ffi = require "ffi";
        ffi.cdef[[
            typedef void fd_set;  /* Not defined */
            int select(int nfds, fd_set *readfds, fd_set *writefds,
                       fd_set *exceptfds, struct timeval *timeout);
            struct timeval {
                long    tv_sec;         /* seconds */
                long    tv_usec;        /* microseconds */
            };
            ssize_t read(int fd, void *buf, size_t len);

            int ioctl(int fd, int cmd, ...);
            struct winsize {
                uint16_t ws_row;
                uint16_t ws_col;
                uint16_t ws_xpixel;   /* unused */
                uint16_t ws_ypixel;   /* unused */
            };
            int tcgetattr(int fd, struct termios *termios_p);
            int tcsetattr(int fd, int optional_actions,
                          const struct termios *termios_p);
            enum {NCCNS = 128};  // Should be large enough
            typedef uint8_t cc_t;
            typedef uint32_t tcflag_t;
            struct termios {
                tcflag_t c_iflag;      /* input modes */
                tcflag_t c_oflag;      /* output modes */
                tcflag_t c_cflag;      /* control modes */
                tcflag_t c_lflag;      /* local modes */
                cc_t     c_cc[NCCNS];  /* special characters */
            };

            int clock_gettime(int /* clockid_t */ clk_id, struct timespec *tp);
            struct timespec {
                long    tv_sec;         /* seconds */
                long    tv_nsec;        /* nanoseconds */
            };

            typedef uint32_t mode_t;
            int mkdir(const char *pathname, mode_t mode);
            char *getcwd(char *buf, size_t size);
        ]];
        -- Hardcoded constants; may need to change when on a different platform
        local termio = {TIOCGWINSZ = 0x5413, ICANON = 0x2, ECHO = 0x8, TCSANOW = 0};
        local clock = {CLOCK_REALTIME = 0};

        -- Terminal handling
        local pattr = ffi.new "struct termios[1]";
        ffi.C.tcgetattr(1, pattr);

        local pattr_orig = ffi.new("struct termios[1]", {pattr[0]});
        pattr[0].c_lflag = band(pattr[0].c_lflag, bnot(bor(termio.ICANON, termio.ECHO)));

        function term_setup()
            ffi.C.tcsetattr(1, termio.TCSANOW, pattr);
        end
        _fini_cb[#_fini_cb + 1] = function()
            ffi.C.tcsetattr(1, termio.TCSANOW, pattr_orig);
        end;
        term_setup();

        function wait_input(sec, fd)
            fd = fd or 0;
            local pfd = fd >= 0
                and ffi.new("uint8_t[?]", rshift(fd, 3) + 1, {[rshift(fd, 3)] = lshift(1, band(fd, 0x7))})
                or nil;
            local ptv = sec >= 0
                and ffi.new("struct timeval[1]", {{math_modf(sec), 1e6 * math_fmod(sec, 1)}})
                or nil;
            local res = ffi.C.select(fd + 1, pfd, nil, nil, ptv);
            return res > 0;
        end
        function readall_or_block(fd)
            local buf = ffi.new "int8_t[32]";
            local len = ffi.C.read(fd or 0, buf, 32);
            return ffi.string(buf, len);
        end
        function getwinsize()
            local psize = ffi.new "struct winsize[1]";
            ffi.C.ioctl(0, termio.TIOCGWINSZ, psize);
            return psize[0];
        end
        function mkdir(path, perm)
            ffi.C.mkdir(path, tonumber(perm, 8));
        end
        function getclocktp()
            local ptp = ffi.new "struct timespec[1]";
            ffi.C.clock_gettime(clock.CLOCK_REALTIME, ptp);
            return ptp[0];
        end
        function getpwd()
            return ffi.string(ffi.C.getcwd(ffi.new "char [4096]", 4096));
        end
    end
    -- Ensure `ffi` library is unbound
    ffi = nil;

    -- Utilities
    -- Special escapes
    local ESC = "\27";
    local CSI = ESC .. "[";
    local bell_str = "\a";
    local clrtoeol_str = CSI .. "K";

    -- Input utilities
    local Key = {ini_delay = -1, esc_delay = 0.650, seq_delay = 0.010};
    local function parse_csi_param(keygen)
        -- Parse parameter for CSI
        local pri_head = "";
        local ctrl_args = {0, rest = ""};
        local inter = "";
        local final = "";
        local orig = "";
        local arg_idx = 1;
        local ch = keygen(Key.seq_delay);
        if #ch == 0 then
            -- End of input
            return;
        end

        -- Private-use parameter string
        if string_match(ch, "<=>?") then
            orig = orig .. ch;
            pri_head = ch;
            ch = keygen(Key.seq_delay, true);
        end
        -- Standard parameter string
        -- Cannot handle non-trivial private-use format
        while string_match(ch, "[0-9;]") do
            orig = orig .. ch;
            if string_match(ch, "[0-9]") then
                ctrl_args[arg_idx] = 10 * ctrl_args[arg_idx] + tonumber(ch);
            else
                arg_idx = arg_idx + 1;
                ctrl_args[arg_idx] = 0;
            end
            ch = keygen(Key.seq_delay, true);
        end
        -- Non-parsable part of parameter string goes here
        while string_match(ch, "[0-9:;<=>?]") do
            orig = orig .. ch;
            ctrl_args.rest = ctrl_args.rest .. ch;
            ch = keygen(Key.seq_delay, true);
        end
        -- Intermediate bytes
        while string_match(ch, "[ -/]") do
            orig = orig .. ch;
            inter = inter .. ch;
            ch = keygen(Key.seq_delay, true);
        end
        -- Final byte
        if string_match(ch, "[@-~]") then
            orig = orig .. ch;
            final = ch;
        end
        return pri_head, ctrl_args, inter, final, orig;
    end

    local function readall(fd)
        if not wait_input(0) then
            return "";
        end
        return readall_or_block(fd);
    end

    -- Input - key modifiers
    local mod_code = {SHIFT = 0x1, ALT = 0x2, CTRL = 0x4, META = 0x8};
    local function ctrl(ch)
        return string_char(band(string_byte(ch), 0x1f));
    end
    local function Mod(key, code, prefix)
        if type(key) == "table" then
            key[1] = bit.bor(key[1], code);
            return key;
        end
        if key ~= nil and #key > 0 then
            return prefix .. key;
        end
        return "";
    end
    local function Ctrl(key)
        return Mod(key, mod_code.CTRL, "^");
    end
    local function Shift(key)
        return Mod(key, mod_code.SHIFT, "SHIFT-");
    end
    local function Meta(key)
        return Mod(key, mod_code.META, "ESC-");
    end
    local function mod_key(mod, key)
        local res = key;
        if band(mod, mod_code.CTRL) ~= 0 then
            res = Ctrl(res);
        end
        if band(mod, mod_code.SHIFT) ~= 0 then
            res = Shift(res);
        end
        if band(mod, bit.bor(mod_code.ALT, mod_code.META)) ~= 0 then
            res = Meta(res);
        end
        return res;
    end

    -- Input - escape sequence tables
    local getch_ss3_csi = Mergable{
        -- The following bytes (SS3 `ESC O`)
        -- Intermediate bytes plus final byte (CSI)
        A = "UP", B = "DOWN", C = "RIGHT", D = "LEFT",
        -- xterm
        H = "HOME", F = "END",
    };
    local getch_ss3_csi_p = Mergable{
        -- The following bytes (SS3 `ESC O`)
        -- Intermediate bytes plus final byte (CSI) with parameter
        -- xterm
        P = "F1", Q = "F2", R = "F3", S = "F4",
    };
    local getch_csi_fi = {
        -- Intermediate bytes plus final byte
        -- SCO
        G = "PGDN",
        I = "PGUP",
        L = "INS",

        -- Back Tabulation (`SHIFT-TAB`)
        Z = "STAB",

        -- rxvt
        a = Shift{0, "UP"},
        b = Shift{0, "DOWN"},
        c = Shift{0, "RIGHT"},
        d = Shift{0, "LEFT"},
        ["~"] = noop,
        ["$"] = Shift,
        ["^"] = Ctrl,
        ["@"] = function(...) return Shift(Ctrl(...)); end,
    };
    local getch_csi_p = {
        -- First argument of parameter string
        [1] = "HOME", [2] = "INS", [3] = "DEL", [4] = "END",
        [5] = "PGUP", [6] = "PGDN",

        -- rxvt
        [7] = "HOME", [8] = "END",

        -- Function keys
        [11] = "F1", [12] = "F2", [13] = "F3", [14] = "F4", [15] = "F5",
        [17] = "F6", [18] = "F7", [19] = "F8", [20] = "F9", [21] = "F10",
        [23] = "F11", [24] = "F12",

        -- vt220
        [25] = "F13", [26] = "F14",
        [28] = "F15", [29] = "F16",
        [31] = "F17", [32] = "F18", [33] = "F19", [34] = "F20",
    };
    local getch_esc = {
        ["["] = function(ch, keygen)
            local pri_head, ctrl_args, inter, final = parse_csi_param(keygen);
            if pri_head == nil then
                -- End of input
                return Meta{0, "["};
            end
            if #pri_head ~= 0 then
                -- Connot handle private-use parameter
                return "";
            end
            local mod = (ctrl_args[2] or 1) - 1;
            local key = getch_csi_fi[inter .. final]
                or (ctrl_args[2] and getch_ss3_csi_p[inter .. final] or "");
            if type(key) == "function" then
                local p_key = getch_csi_p[ctrl_args[1]];
                return key{mod, p_key};
            end
            if type(key) == "table" and type(key[1]) == "number" then
                return {bit.bor(key[1], mod), p_key};
            end
            return {mod, key};
        end,
        O = {
            -- PuTTY-rxvt
            w = "END",

            -- rxvt
            a = Ctrl{0, "UP"},
            b = Ctrl{0, "DOWN"},
            c = Ctrl{0, "RIGHT"},
            d = Ctrl{0, "LEFT"},

            [false] = Meta{0, "O"},
        };
        [false] = function(ch) return #ch > 0 and Meta{0, ch} or ""; end,
    };
    local function handle_esc(ch, keygen, sec)
        sec = (sec and sec / 2) or Key.esc_delay;
        ch = keygen(sec);
        local mod_func = type(ch) == "function" and ch or noop;
        if #ch == 0 then
            return mod_func("ESC");
        end
        if ch == ESC then
            -- Proper tail recursion
            return handle_esc(Meta, keygen, math_min(sec, 2 * Key.seq_delay));
        end
        return mod_func(getch_esc[ch] or getch_esc[false](ch));
    end;
    local getch_key;
    local function handle_pass_nl(ch)
        term_setup();  -- Workaround for broken output when resuming from `SIGSTOP`
        return #ch > 0
            and {"ENTER", getch_key[ch] or getch_key[false](ch)}
            or "ENTER";
    end
    getch_key = {
        [ctrl"C"] = noop,  -- Should cause interruption
        ["\t"] = "TAB",
        ["\b"] = "BS", ["\127"] = "BS",
        ["\n"] = {
            ["\r"] = "ENTER",
            [false] = handle_pass_nl,
        },
        ["\r"] = {
            ["\0"] = "ENTER", ["\n"] = "ENTER",
            [false] = handle_pass_nl,
        },
        [ESC] = handle_esc,
        [false] = function(ch)
            if #ch > 0 and string_byte(ch) < string_byte(" ") then
                return "^" .. string_char(string_byte("@") + string_byte(ch));
            end
            return ch;
        end,
    };
    getch_esc.O = getch_esc.O .. getch_ss3_csi .. getch_ss3_csi_p;
    getch_csi_fi = getch_csi_fi .. getch_ss3_csi;
    getch_ss3_csi = nil;

    -- Cached cursor position
    local term_y = 0;
    local term_x = 0;
    local function move(y, x)
        y, x = y and math_modf(y) or 0, x and math_modf(x) or 0;
        io_write(CSI .. math_max(y, 0) + 1 .. ";" .. math_max(x, 0) + 1 .. "H");
        term_y, term_x = y, x;
    end
    -- Input - escape sequence interpreter
    local function getkey(keygen, delay, key)
        delay = delay or Key.seq_delay;
        key = key or getch_key;
        local y = 0;
        local ch;
        while type(key) ~= "string" do
            if type(key) == "table" and #key == 0 then
                ch = keygen(delay);
                delay = Key.seq_delay;
                key = key[ch] or key[false];
            end
            if type(key) == "function" then
                key = key(ch, keygen);
            end
            if type(key) == "table" and #key > 0 then
                if type(key[1]) ~= "number" then
                    local res = {key[1], getkey(keygen, nil, {unpack(key, 2)})};
                    return unpack(res);
                end
                key = mod_key(unpack(key));
            end
            if key == nil then
                return "";
            end
        end
        return key;
    end
    -- FIFO buffer
    local_class "P_buf"{size = 32};
    function P_buf:pop()
        local elem = self[1][self.idx];
        if elem ~= nil then
            self.idx = self.idx + 1;
            if self.idx > self.size then
                self[1] = {unpack(self[1], self.idx)};
                self.idx = 1;
            end
            return elem;
        end
        return nil;
    end
    function P_buf:pop_reread()
        local res = self:pop();
        while res == nil do
            self[1] = {unpack_str(readall_or_block())};
            self.idx = 1;
            res = self:pop();
        end
        return res;
    end
    function P_buf:push(...)
        for k, v in ipairs{...} do
            self[1][#self[1] + 1] = v;
        end
    end
    local pch_buf = P_buf{{}, idx = 1};
    local pkey_buf = P_buf{{}, idx = 1};
    local str_to_keys = coroutine_wrap(function(str)
        while true do
            local res = {unpack(pkey_buf[1], pkey_buf.idx)};
            pkey_buf[1] = {};
            pkey_buf.idx = 1;
            pch_buf:push(unpack_str(str));
            while pch_buf[1][pch_buf.idx] ~= nil do
                local pch_buf_bk = {obj_copy(pch_buf[1]), idx = pch_buf.idx};
                local resk = {getkey(function(sec, may_yield)
                    local may_yield = may_yield;
                    while true do
                        local ch = pch_buf:pop();
                        if ch ~= nil then
                            return ch;
                        end
                        -- Input timed out
                        if not may_yield then
                            return "";
                        end
                        may_yield = false;
                        -- Restore buffer
                        pch_buf, pch_buf_bk = pch_buf_bk, pch_buf;
                        str = coroutine_yield(unpack(res));
                        res = {};
                        -- Merge new content into the buffer
                        pch_buf_bk:push(unpack(pch_buf[1], pch_buf.idx));
                        pch_buf = pch_buf_bk;
                        -- Redo backup
                        pch_buf_bk = {obj_copy(pch_buf[1]), idx = pch_buf.idx};
                    end
                end)};
                for k, v in ipairs(resk) do
                    res[#res + 1] = v;
                end
            end
            str = coroutine_yield(unpack(res));
        end
    end);
    local function getch()
        repeat
            local key = pkey_buf:pop();
            if key ~= nil and #key > 0 then
                return key;
            end
        until key == nil;
        local res = {getkey(function(sec)
            local ch = pch_buf:pop();
            if ch ~= nil then
                return ch;
            end
            sec = sec or -1;
            if sec < 0 then
                io_flush();
            end
            if wait_input(sec) then
                local res = pch_buf:pop_reread();
                return res;
            end
            -- Input timed out
            return "";
        end, Key.ini_delay)};
        pkey_buf:push(unpack(res, 2));
        return res[1];
    end

    -- Output utilities
    local function sgr_str(...)
            return CSI .. table_concat({...}, ";")  .. "m";
    end
    local function strip_ansi(str)
        -- The order matters
        return (string_gsub(string_gsub(str, ESC .. "%[[0-9:;<=>?]*[ -/]*[@-~]", ""), ESC .. "[@-~]", ""));
    end
    local function str_width(str)
        -- Assume UTF-8 is used; remove `gsub` if using Big5
        -- Assume all non-ASCII characters are full-width
        return #string_gsub(string_gsub(strip_ansi(str), "[\192-\252][\128-\191]*", "  "), "[\128-\191]+", "");
    end
    local utf8_head_ffz_len = {1, 0, 2, 3, 4, 5, 6, 0, 0};
    local utf8_head_ffz_width = {1, 0, 2, 2, 2, 2, 2, 0, 0};
    local ffz_map = {
        [0x00] = 1,
        [0x80] = 2, [0xc0] = 3, [0xe0] = 4, [0xf0] = 5,
        [0xf8] = 6, [0xfc] = 7, [0xfe] = 8, [0xff] = 9,
    };
    local function ch_map_ffz(ch, tbl)
        local byte = bor(0xffffff00, string_byte(ch) or 0xff);
        byte = band(byte, arshift(byte, 1));
        byte = band(byte, arshift(byte, 2));
        byte = band(byte, arshift(byte, 4));
        return tbl[ffz_map[band(byte, 0x000000ff)]];
    end
    local function ch_complete_len(ch)
        return ch_map_ffz(ch, utf8_head_ffz_len);
    end
    local function ch_complete_width(ch)
        return ch_map_ffz(ch, utf8_head_ffz_width);
    end
    local function truncate_str(str, width)
        local res = {};
        local len = 0;
        for ch in each_ch_utf8(str) do
            local w = str_width(ch);
            if len + w > width then
                break;
            end
            res[#res + 1] = ch;
            len = len + w;
        end
        return table_concat(res);
    end

    -- UI functions
    local function flush_input(pbuf_out)
        while wait_input(0) do
            local str = readall_or_block();
            if pbuf_out ~= nil then
                pbuf_out:push(unpack_str(str));
            end
        end
    end
    local function getyx()
        return term_y, term_x;
    end
    local function getrealyx()
        flush_input(pch_buf);
        local preply = P_buf{{}, idx = 1};
        local pos;
        local try = 0;
        repeat
            -- Send query and try to read the response for 16 times
            -- Resend query if no valid response is received
            if try == 0 then
                io_write(CSI .. "6n");
                io_flush();
            end
            try = band(try + 1, 0x0f);
            local pri_head, ctrl_args, inter, final;
            local esc = preply:pop_reread();
            if esc == ESC then
                local lbracket = preply:pop_reread();
                if lbracket == "[" then
                    pri_head, ctrl_args, inter, final =
                        parse_csi_param(function()
                            return preply:pop_reread();
                        end);
                    pos = ctrl_args;
                else
                    pch_buf:push(esc, lbracket);
                end
            else
                pch_buf:push(esc);
            end
        until pri_head == "" and inter .. final == "R";
        pch_buf:push(unpack(preply[1], preply.idx));
        io_close();
        return pos[1] - 1, (pos[2] or 1) - 1;
    end
    local function getmaxyx()
        local size = getwinsize();
        return size.ws_row, size.ws_col;
    end

    -- Pause message
    local function pause_null_msg(fg)
        return sgr_str(0, fg) .. [=[▏▎▍▋▊▉ ]=] .. sgr_str() .. [[請按任意鍵繼續]] .. sgr_str(fg) .. [=[▉ ]=] .. sgr_str();
    end
    local pause_null_msg_len = str_width(pause_null_msg(0));
    local pause_tail = sgr_str(31, 47) .. [[ [請按任意鍵繼續] ]] .. sgr_str();
    local pause_tail_len = str_width(pause_tail);
    local function pause(msg)
        local ymax, xmax = getmaxyx();
        if msg == nil then
            local fg = 30 + math_random(1, 6);
            move(ymax - 1, xmax - pause_null_msg_len);
            io_write(pause_null_msg(fg));
            term_x = xmax - 1;
        else
            local msg_head = sgr_str(0, 34, 46) .. [=[ ★ ]=] .. msg;
            move(ymax - 1, 0);
            io_write(msg_head .. string_rep(" ", xmax - str_width(msg_head) - pause_tail_len));
            move(ymax - 1, xmax - pause_tail_len);
            io_write(pause_tail);
            term_x = xmax - 1;
        end
        return getch();
    end

    -- Initialize cursor position
    term_y, term_x = getrealyx();

    -- Input field
    local history = {{}, idx = 1, dirty = false};
    local Echo = Mergable{
        -- Echo flags
        NOECHO = 0x0,
        DOECHO = 0x1,
        LCECHO = 0x2,
        NUMECHO = 0x4,
        PASSECHO = 0x10,
        HIDEECHO = 0x20,
        -- GCARRY = 0x8,  -- Ignored
    };  -- Combined echo: DreamBBS BBS-Lua Extension
    local function preprocess_data(key, data, cur, w, echo, history)
        if band(echo, Echo.NUMECHO) ~= 0 then
            data, sub_count = string_gsub(data, "[^0-9]", "");
        end
        data = truncate_str(strip_ansi(data), w);
        return data, #data, w, echo, history;
    end
    local function delete(key, data, cur, w, echo, history)
        local suffix = string_gsub(string_sub(data, cur + 1), "^.[\128-\191]*", "");
        return string_sub(data, 1, cur) .. suffix, cur, w, echo, history;
    end
    local function left(key, data, cur, w, echo, history)
        local prefix = string_gsub(string_sub(data, 1, cur), ".[\128-\191]*$", "");
        return data, #prefix, w, echo, history;
    end
    local function save_data(key, data, cur, w, echo, history)
        if band(echo, bit.bor(Echo.PASSECHO, Echo.HIDEECHO)) == 0
                and str_width(data) > 2 then
            if history.dirty then
                history[1][history.idx] = data;
            end
        end
        history.dirty = false;
        return data, #data, w, echo, history;
    end
    local getdata_keybind = {
        -- Need to handle multi-byte character (assume UTF-8)
        ENTER = function(key, data, cur, w, echo, history)
            save_data(key, data, cur, w, echo, history);
            local removed = table_remove(history[1], history.idx);
            history[1][#history[1] + 1] = removed;
            if str_width(history[1][#history[1]]) <= 2 then
                history[1][#history[1]] = nil;
            end
            history.idx = #history[1] + 1;
            return data, cur, w, echo, history;
        end,
        BS = function(key, data, cur, w, echo, history)
            if cur <= 0 then
                io_write(bell_str);
                return data, cur, w, echo, history;
            end
            history.dirty = true;
            return delete(key, left(key, data, cur, w, echo, history));
        end,
        DEL = function(key, data, cur, w, echo, history)
            if cur >= #data then
                io_write(bell_str);
                return data, cur, w, echo, history;
            end
            history.dirty = true;
            return delete(key, data, cur, w, echo, history);
        end,
        LEFT = function(key, data, cur, w, echo, history)
            if cur <= 0 or band(echo, Echo.HIDEECHO) ~= 0 then
                io_write(bell_str);
                return data, cur, w, echo, history;
            end
            return left(key, data, cur, w, echo, history);
        end,
        RIGHT = function(key, data, cur, w, echo, history)
            if cur >= #data then
                io_write(bell_str);
                return data, cur, w, echo, history;
            end
            local suffix = string_gsub(string_sub(data, cur + 1), "^.[\128-\191]*", "");
            return data, #data - #suffix, w, echo, history;
        end,
        HOME = function(key, data, cur, w, echo, history)
            if cur <= 0 or band(echo, Echo.HIDEECHO) ~= 0 then
                io_write(bell_str);
                return data, cur, w, echo, history;
            end
            return data, 0, w, echo, history;
        end,
        END = function(key, data, cur, w, echo, history)
            if cur >= #data then
                io_write(bell_str);
                return data, cur, w, echo, history;
            end
            return data, #data, w, echo, history;
        end,
        ["^Y"] = function(key, data, cur, w, echo, history)
            if #data == 0 then
                io_write(bell_str);
                return data, cur, w, echo, history;
            end
            history.dirty = true;
            return "", 0, w, echo, history;
        end,
        ["^K"] = function(key, data, cur, w, echo, history)
            if cur >= #data then
                io_write(bell_str);
                return data, cur, w, echo, history;
            end
            history.dirty = true;
            return string_sub(data, 1, cur), cur, w, echo, history;
        end,
        UP = function(key, data, cur, w, echo, history)
            if band(echo, bit.bor(Echo.PASSECHO, Echo.HIDEECHO)) ~= 0
                    or history[1][history.idx - 1] == nil then
                io_write(bell_str);
                return data, cur, w, echo, history;
            end
            save_data(key, data, cur, w, echo, history);
            history.idx = history.idx - 1;
            data = history[1][history.idx];
            history.dirty = false;
            return preprocess_data(key, data, cur, w, echo, history);
        end,
        DOWN = function(key, data, cur, w, echo, history)
            if band(echo, bit.bor(Echo.PASSECHO, Echo.HIDEECHO)) ~= 0
                    or history[1][history.idx + 1] == nil then
                io_write(bell_str);
                return data, cur, w, echo, history;
            end
            save_data(key, data, cur, w, echo, history);
            history.idx = history.idx + 1;
            data = history[1][history.idx];
            history.dirty = false;
            return preprocess_data(key, data, cur, w, echo, history);
        end,
        [false] = function(key, data, cur, w, echo, history)
            if #key ~= 1 or string_byte(key) < string_byte(" ")
                  or str_width(data .. key) > w
                  or (band(echo, Echo.NUMECHO) ~= 0 and string_match(key, "[^0-9]")) then
                io_write(bell_str);
                return data, cur, w, echo, history;
            end
            history.dirty = true;
            return (string_sub(data, 1, cur) .. key .. string_sub(data, cur + 1)), cur + 1, w, echo, history;
        end,
    };
    getdata_keybind[ctrl"D"] = getdata_keybind.DEL;
    getdata_keybind[ctrl"B"] = getdata_keybind.LEFT;
    getdata_keybind[ctrl"F"] = getdata_keybind.RIGHT;
    getdata_keybind[ctrl"A"] = getdata_keybind.HOME;
    getdata_keybind[ctrl"E"] = getdata_keybind.END;
    local function getdata_display(data, cur, w, echo)
        local w_data = str_width(data);
        local w_cur = ch_complete_len(string_sub(data, cur + 1, cur + 1));
        local prefix = string_sub(data, 1, cur);
        local w_prefix = str_width(prefix);
        local ch_cur = string_sub(data, cur + 1, cur + 1 + w_cur - 1);
        local suffix = string_sub(data, cur + 1 + w_cur);
        local passecho = band(echo, Echo.PASSECHO) ~= 0;
        if passecho then
            prefix = string_rep("*", w_prefix);
            ch_cur = string_rep("*", str_width(ch_cur));
            suffix = string_rep("*", str_width(suffix));
        end
        io_write(clrtoeol_str .. sgr_str(0, 30, 47) .. prefix);
        if #ch_cur == 0 and #suffix == 0 then
            if w - w_data > 0 then
                io_write(sgr_str(1, 37, 40, 100) .. " "  .. sgr_str(0, 30, 47) .. suffix .. string_rep(" ", w - w_data - 1));
            end
        else
            io_write(sgr_str(1, 37, 40, 100) .. ch_cur .. sgr_str(0, 30, 47) .. suffix .. string_rep(" ", w - w_data));
        end
        io_write(sgr_str());
        term_x = term_x + w;
        return w_prefix;
    end
    local function getdata(w, echo, str)
        w = math_max(w - 1, 0);  -- `- 1` for C-string ending `'\0'`
        echo = echo ~= Echo.NOECHO and (echo or 0) or Echo.HIDEECHO;
        local y, x = getyx();
        local data, cur = preprocess_data("", str or "", 0, w, echo, history);
        history[1][history.idx] = data;
        if band(echo, Echo.HIDEECHO) == 0 then
            move(y, x + getdata_display(data, cur, w, echo));
        end
        repeat
            local key = getch();
            data, cur = (getdata_keybind[key] or getdata_keybind[false])(key, data, cur, w, echo, history);
            if band(echo, Echo.HIDEECHO) == 0 then
                move(y, x);
                move(y, x + getdata_display(data, cur, w, echo));
            end
        until key == "ENTER";
        if band(echo, Echo.LCECHO) ~= 0 then
            data = data:lower();
        end
        if band(echo, Echo.HIDEECHO) == 0 then
            move(y, x);
            getdata_display(data, cur, str_width(data), echo);
        end
        io_write(clrtoeol_str .. "\n");
        term_y, term_x = term_y + 1, 0;
        return data;
    end

    -- The BBS-Lua API table
    bbs = {
        addstr = function(...)
            local ymax, xmax = getmaxyx();
            local msg, r = string_gsub(table_concat{...}, "\n", clrtoeol_str .. "\n");
            r = r + ({string_gsub(msg, string_rep("[^\n]", xmax), "")})[2];
            local tail = string_match(msg, "\n(.*)$");
            io_write(msg);
            term_y = term_y + r;
            if tail ~= nil then
                term_x = str_width(tail) % xmax;
            else
                term_x = (term_x + str_width(msg)) % xmax;
            end
        end,
        title = function(ttl)
            move(0, 0);
            bbs.addstr(sgr_str(1, 44), [[【]], ttl, [[】]], sgr_str());
        end,
        print = function(...)
            bbs.addstr(...);
            io_write(clrtoeol_str .. "\n");
            term_y, term_x = term_y + 1, 0;
        end,
        getyx = getyx,
        getmaxyx = getmaxyx,
        move = move,
        moverel = function(dy, dx)
            dy, dx = math_fmod(dy), dx and math_fmod(dx) or 0;
            if dy > 0 then
                io_write(CSI .. dy .. "B");
            elseif dy < 0 then
                io_write(CSI .. -dy .. "A");
            end
            if dx > 0 then
                io_write(CSI .. dx .. "C");
            elseif dx < 0 then
                io_write(CSI .. -dx .. "D");
            end
            term_y, term_x = term_y + dy, term_x + dx;
        end,
        clear = function()
            io_write(CSI .. "2J");
            move(0, 0);
        end,
        clrtoeol = function() io_write(clrtoeol_str); end,
        clrtobot = function() io_write(CSI .. "J"); end,
        rect = function(r, c, ttl)
            c = math_max(c, 0);
            local y, x = getyx();
            for kr = 0, r - 1, 1 do
                move(y + kr, x);
                io_write(string_rep(" ", c));
            end
            if ttl ~= nil then
                -- Center the title, both vertically and horizontally
                local r_ttl = #string_gsub(ttl, "[^\n]", "") + 1;
                local kr = 0;
                for line in string_gmatch(ttl, "[^\n]+") do
                    move(y + (r - r_ttl) / 2 + kr, x + (c - str_width(line)) / 2);
                    io_write(line);
                    kr = kr + 1;
                end
            end
            move(y, x);
        end,
        refresh = io_flush,
        attrset = function(...)
            io_write(sgr_str(...));
        end,
        ANSI_COLOR = sgr_str,
        ANSI_RESET = sgr_str(),
        ESC = ESC,
        strip_ansi = strip_ansi,
        str_width = str_width,  -- BBS-Lua on Lua Extension
        getch = getch,
        getdata = getdata,
        pause = pause,
        kbhit = function(sec)
            io_flush();
            return wait_input(sec or 0);
        end,
        kbreset = function()
            flush_input()
            pch_buf[1] = {};
            pch_buf.idx = 1;
            pkey_buf[1] = {};
            pkey_buf.idx = 1;
        end,
        kball = function(sec)
            io_flush();
            -- Sleep first if needed
            if sec and sec > 0 then
                bbs.sleep(sec);
            end
            -- Then read
            return str_to_keys(readall());
        end,
        time = os_time,
        ctime = os_date,
        clock = function()
            local tp = getclocktp();
            return tonumber(tp.tv_sec) + 1e9 * tonumber(tp.tv_nsec);
        end,
        sleep = function(sec)
            io_flush();
            if sec > 0 then
                wait_input(sec, -1);
            end
        end,
        userid = "guest",
        usernick = "夢之大地訪客",
        sitename = "DreamBBS",
        interface = "0.201",
    };
    -- Aliases
    bbs.outs = bbs.addstr;
    print = bbs.print;
    bbs.color = bbs.attrset;
    bbs.getstr = bbs.getdata;
    bbs.now = bbs.time;

    -- Other constants
    bbs = bbs .. Echo;  -- BBS-Lua on Lua Extension

    -- Store API implementation

    -- Path
    local BBSHOME = ".";  -- Can be changed if needed
    local store_cate = {GLOBAL = "global", USER = "user"};
    local function get_global_path_tbl(hash)
        -- Why does the original BBS-Lua implementation use `U`?
        return {"luastore", "v1_U" .. bit_tohex(hash, 8)};
    end
    local function get_user_path_tbl(userid, hash)
        -- Why does the original BBS-Lua implementation use `G`?
        return {"usr", string_sub(userid, 1, 1), userid, ".luastore", "v1_G" .. bit_tohex(hash, 8)};
    end
    local function store_path(cate, userid, hash)
        local path_tbl;
        if cate == store_cate.GLOBAL then
            path_tbl = get_global_path_tbl(hash);
        elseif cate == store_cate.USER then
            path_tbl = get_user_path_tbl(userid, hash);
        end
        if path_tbl == nil then
            return nil;
        end
        local path = BBSHOME;
        for k, v in ipairs(path_tbl) do
            mkdir(path, "0755");
            path = path .. "/" .. v;
        end
        return path;
    end

    -- Hash
    -- FreeBSD uses a non-standard 32-bit FNV offset basis,
    -- which is used in the original BBS-Lua implementation
    local fnv = {FNV_32_OFFSET = 0x811c9dc5, FNV_32_OFFSET_FREEBSD = 33554467, FNV_32_PRIME = 0x01000193};
    local function fnv1_32_hash(str)
        local res = fnv.FNV_32_OFFSET_FREEBSD;
        for ch in each_ch(str) do
            -- Prevent the result from exceeding the 51-bit integer range
            -- Signness does not matter (no sign extension)
            local res_ll = band(res, 0xffff) * band(fnv.FNV_32_PRIME, 0xffff);
            local res_lh = band(res, 0xffff) * rshift(fnv.FNV_32_PRIME, 16);
            local res_hl = rshift(res, 16) * band(fnv.FNV_32_PRIME, 0xffff);
            res = lshift(res_lh + res_hl, 16) + res_ll;
            res = bxor(res, string_byte(ch));
        end
        return res;
    end

    local curr_dir = getpwd();
    -- Function for initializing a new instance of BBS-Lua store API
    function store_new(path)
        -- Normalize and hash the path
        local store_hash_path = path;
        store_hash_path = string_gsub(store_hash_path, "^%.?/", "");
        if not string_match(store_hash_path, "^/") then
            store_hash_path = curr_dir .. "/" .. store_hash_path;
        end
        local store_hash = fnv1_32_hash(store_hash_path);

        -- IO limiter
        local store_max = 32;
        local store_counter = 0;

        local store;

        local function limit(cate)
            if cate == store.USER then
                return 16 * 1024;
            end
            if cate == store.GLOBAL then
                return 16 * 1024;
            end
            return 0;
        end

        -- The BBS-Lua store API table
        store = {
            load = function(cate)
                if store_counter >= store_max then
                    return nil, "IO limit exceeded";
                end
                local path = store_path(cate, bbs.userid, store_hash);
                if path == nil then
                    return nil, "invalid store category";
                end
                local file = io_open(path, "r");
                if file == nil then
                    return nil, "failed to open file: " .. path;
                end
                store_counter = store_counter + 1;
                local res = file:read("*a");
                file:close();
                if #res > limit(cate) then
                    return res, "data size limit exceeded (kept): " .. #res;
                end
                return res;
            end,
            save = function(cate, str)
                if store_counter >= store_max then
                    return false, "IO limit exceeded";
                end
                local path = store_path(cate, bbs.userid, store_hash);
                if path == nil then
                    return false, "invalid store category";
                end
                local file = io_open(path, "w");
                if file == nil then
                    return false, "failed to open file: " .. path;
                end
                store_counter = store_counter + 1;
                local res = {true};
                local len_limit = limit(cate);
                if #str > len_limit then
                    str = string_sub(str, 1, len_limit);
                    res[2] = string_format("data size limit exceeded (truncated to %d): %d", len_limit, #str);
                end
                file:write(str);
                file:close();
                return unpack(res);
            end,
            limit = limit,
            iolimit = function() return store_max; end,
            USER = store_cate.USER,
            GLOBAL = store_cate.GLOBAL,
        };
        return store;
    end
end

local function _fini()
    if _fini_cb ~= nil then
        for k = #_fini_cb, 1, -1 do
            _fini_cb[k]();
        end
    end
end
local function _init(...)
    math_randomseed(bbs.clock());

    local function report_error(errmsg, pausemsg)
        local ymax, xmax = bbs.getmaxyx();
        local r = #string_gsub(errmsg, "[^\n]", "") + 1;
        bbs.move(ymax - 1 - r, 0);
        bbs.attrset(0, 1, 44);
        bbs.rect(r, xmax);
        bbs.print(errmsg);
        bbs.pause(pausemsg);
    end
    local traceback = debug.traceback;

    local function launcher(...)
        local ymax, xmax = bbs.getmaxyx();
        local toc;
        local prog;
        do
            local store = store_new(arg[0]);

            bbs.attrset();
            bbs.clrtobot();
            bbs.print[[
BBS-Lua on Lua - x86-64 Linux
Copyright (c) 2020 Iweidieng Iep <iid@ccns.ncku.edu.tw>

BBS-Lua is a project run by Hung-Te Lin <piaip@csie.org>
For more information, please refer to https://term.ptt2.cc BBSLua
    ]];
            local conf = obj_from_str(store.load(store.GLOBAL) or "{}");

            bbs.print("Configurations: ");
            bbs.print("Lua runtime: ", __RUNTIME)
            bbs.print("Lua API version: ", _VERSION)
            bbs.print("Terminal size: ", ymax, "x", xmax);

            bbs.addstr("userid: ");
            conf.userid = conf.userid or bbs.userid;
            conf.userid = bbs.getdata(13, bbs.DOECHO, conf.userid);
            bbs.userid = conf.userid;

            bbs.addstr("usernick: ");
            conf.usernick = conf.usernick or bbs.usernick;
            conf.usernick = bbs.getdata(25, bbs.DOECHO, conf.usernick);
            bbs.usernick = conf.usernick;

            bbs.addstr("sitename: ");
            conf.sitename = conf.sitename or bbs.sitename;
            conf.sitename = bbs.getdata(25, bbs.DOECHO, conf.sitename);
            bbs.sitename = conf.sitename;
            bbs.print();

            local save_res, e = store.save(store.GLOBAL, objstr(conf, false));
            if save_res then
                bbs.print("Configuration saved.");
            else
                bbs.print("Failed to save configuration: ", e);
            end

            -- Prepare for loading the program
            local prog_path = arg[1];
            if prog_path == nil or prog_path == "" then
                bbs.print("\nUsage: `" .. arg[0] .. " <program-path>`");
                bbs.addstr("Enter path to BBS-Lua program: ");
                prog_path = bbs.getdata(48, bbs.DOECHO, conf.last_prog_path);
            end
            -- Test whether the path is valid
            local file, err = io_open(prog_path);
            if err ~= nil then
                report_error(err, "BBS-Lua 載入失敗 ('" .. prog_path .. "' 不是有效檔案路徑)。");
                return;
            end
            -- Unbind `arg`, which is not in BBS-Lua
            arg = nil;
            bbs.pause();

            -- Prepare for loading TOC (table of content/summary)
            toc = {};
            local toc_tags = {
                interface = true,
                title = true,
                notes = true,
                author = true,
                version = true,
                date = true,
                latestref = true,
            };
            local bbslua_pat = [[^--#BBSLUA$]];
            local toctag_pat = [[^ *%-%-+ *([_A-Za-z][_0-9A-Za-z]*) *: *(.*) *$]];
            local is_code = false;
            local is_toc = true;

            -- Inner sandboxing
            local prog_global = {
                -- Lua standard library
                assert = assert,
                collectgarbage = collectgarbage,
                error = error,
                getfenv = getfenv,
                getmetatable = getmetatable,
                ipairs = ipairs,
                next = next,
                pairs = pairs,
                pcall = pcall,
                rawequal = rawequal,
                rawget = rawget,
                rawlen = rawlen,
                rawset = rawset,
                select = select,
                setfenv = setfenv,
                setmetatable = setmetatable,
                tonumber = tonumber,
                tostring = tostring,
                type = type,
                unpack = unpack,
                _VERSION = _VERSION,
                xpcall = xpcall,

                coroutine = coroutine,
                string = string,
                table = table,
                math = math,
                bit = bit,

                -- BBS-Lua Exclusive API
                print = bbs.print,
                bbs = bbs,
                toc = toc,
                store = store_new(prog_path),

                -- BBS-Lua on Lua Extensions
                __RUNTIME = __RUNTIME,
                module = module,
                require = require,
                package = package_bbslua,
                debug = debug_bbslua,

                -- Global Lua libraries not in BBS-Lua
                io = nil,
                os = nil,
                jit = nil,
                ffi = nil,

                -- Global Lua API functions not in BBS-Lua
                dofile = nil,
                loadfile = nil,
                load = nil,
                loadstring = nil,
            };
            prog_global._G = prog_global;

            -- Load the program and TOC, with an isolated executation environment
            local err;
            prog, err = basic_load(coroutine_wrap(function()
                -- Load the program line by line to handle plain text and TOC
                for line in io_lines(prog_path) do
                    if string_match(line, bbslua_pat) then
                        if is_code then
                            -- End of code
                            break;
                        end
                        -- Start of code
                        is_code = true;
                        coroutine_yield("\n");  -- Ignore this line
                    elseif is_code then
                        local k, v = string_match(line, toctag_pat);
                        if is_toc then
                            if k and v then
                                if toc_tags[k:lower()] then
                                    toc[k:lower()] = v;
                                end
                            else
                                is_toc = false;
                            end
                        end
                        coroutine_yield(line .. "\n");
                    else
                        -- Plain text before code
                        -- Replace with empty lines to keep the line count
                        coroutine_yield("\n");
                    end
                end
            end),
            prog_path, "t", prog_global);
            -- Handling loading errors
            if err ~= nil then
                report_error(err, [[BBS-Lua 載入失敗。]]);
                return;
            elseif not is_code then
                bbs.pause([[BBS-Lua 載入失敗 (檔案不含 BBS-Lua 程式)。]]);
                return;
            end

            -- `prog_path` is valid; save it in the configuration
            conf.last_prog_path = prog_path;
            -- Configuration changed; save again (ignore errors)
            store.save(store.GLOBAL, objstr(conf, false));

            -- Make the current environment the same as the isolated environment
            basic_load = nil;
            setfenv(0, prog_global);

            -- Free memory for unbound API
            collectgarbage();
        end

        local function stacked_print(y, ...)
            local str = table_concat{...};
            local h = #string_gsub(str, "[^\n]", "") + 1;
            bbs.move(y - h, x);
            bbs.rect(h, xmax);
            bbs.addstr(str);
            return y - h;
        end
        local y = ymax - 1;

        -- Check interface
        local function vercmp(lhs, rhs)
            for v in zipiter({string_gmatch(lhs, "[0-9]+")}, {string_gmatch(rhs, "[0-9]+")}) do
                local lv, rv = unpack(v);
                if (lv == nil and rv > 0) or lv < rv then
                    return -1;
                end
                if lv > rv then
                    return 1;
                end
            end
            return 0;
        end
        if toc.interface == nil or vercmp(toc.interface, "0.0.1") < 0 then
            bbs.attrset(0, 31, 47);
            y = stacked_print(y, [[
 ▲ 此程式缺少相容性資訊，您可能無法正常執行
 若執行出現錯誤，請向原作者取得新版
]]);
        elseif vercmp(toc.interface, bbs.interface) > 0 then
            bbs.attrset(0, 1, 31, 47);
            y = stacked_print(y, [[
 ▲ 此程式使用新版的 BBS-Lua 規格 (]] .. toc.interface .. [[)，您可能無法正常執行
 若執行出現錯誤，建議您重新登入 BBS 後再重試
]]);
        end

        -- Print out TOC
        local toc_items = {
            {"title", [[名稱]]},
            {"notes", [[說明]]},
            {"author", [[作者]]},
            {"version", [[版本]]},
            {"date", [[日期]]},
        };
        local toc_info = {};
        for k, v in ipairs(toc_items) do
            if toc[v[1]] ~= nil then
                table_insert(toc_info, "  " .. v[2] .. ": " .. toc[v[1]]);
            end
        end
        bbs.attrset(0, 1, 30, 47);
        y = stacked_print(y, "\n", table_concat(toc_info, "\n"), "\n");

        bbs.attrset(0, 37, 44);
        y = stacked_print(y,
            [[ ■ BBS-Lua on Lua]], "  ", "Version: ", "v1.00", "  ", "API Level: ", bbs.interface, "\n    ", "Launched at: ", bbs.ctime());
        local msg = [[提醒您執行中隨時可按]] .. bbs.ANSI_COLOR(31) .. " [Ctrl-C] " .. bbs.ANSI_COLOR(34) .. [[強制中斷 BBS-Lua 程式]];
        bbs.pause(msg);
        bbs.clear();

        return prog(...);
    end

    local co;
    co = coroutine_create(function(...)
        local args = {...};
        return xpcall(
            function() return launcher(unpack(args)); end,
            function(e) return traceback(co, e, 1); end);
    end);
    local res, res_co, e = coroutine_resume(co, ...);
    if not res_co and e ~= nil then
        if not string_match(e, "^[^\n]*interrupted!\n") then
            report_error(e, [[BBS-Lua 執行結束 (程式發生錯誤)。]]);
        else
            report_error(e, [[BBS-Lua 執行結束 (使用者中斷)。]]);
        end
    else
        bbs.pause([[BBS-Lua 執行結束。]]);
    end
    _fini();
end

_init(...);
