--#BBSLUA
-- aInterface: 0.119
-- Title:     Tetraint　～ 不四方塊
-- Version:   0.00a
-- Date:      2020-05-23
-- Author:    Iweidieng Iep
-- Notes:     TetrIS　～ 是四方塊
-- Usage:     `./tetraint.lua` or `luajit tetraint.lua`
-- License:   MIT License
--[[
Copyright 2020 Iweidieng Iep

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
]]

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
        if idx:match("^[%a_][%w_]*$") then
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
    return "{" .. table.concat(res, ", ")  .. "}";
end

function objstr(obj, lookup)
    lookup = lookup == nil and Weak_set{} or lookup;
    if lookup and lookup[obj] then
        return (type(obj) == "table" and obj.classname or type(obj)) .. "<skipped>";
    end
    if type(obj) == "string" then
        -- Prevent escaped newlines in short strings
        if #obj <= 8 then
            return (("%q"):format(obj):gsub("\\\n", "\\n"));
        end
        return ("%q"):format(obj);
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
    local idx, eq = str:sub(i):match("^([%a_][%w_]*)( *= *)");
    if idx == nil then
        local lbracket = str:sub(i):match("^%[");
        if lbracket == nil then
            return nil, i;
        end
        local res, i = obj_from_str(str, i + #lbracket);
        local rbracket = str:sub(i):match("^%]");
        return res, i + #rbracket;
    end
    return idx, i + #idx + #eq;
end

local function tbl_from_str(str, i)
    i = i or 1;
    local res = {};
    local idx = 1;
    local open = str:sub(i):match("^ *{ *");
    i = i + #open;
    local close = str:sub(i):match("^ *} *");
    while not close do
        local key, val;
        key, i = idx_from_str(str, i);
        if key == nil then
            key = idx;
            idx = idx + 1;
        end
        val, i = obj_from_str(str, i);
        res[key] = val;
        local comma = str:sub(i):match("^ *, *") or "";
        i = i + #comma;
        close = str:sub(i):match("^ *} *");
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
    local open = str:sub(i):match([[^ *"]]);
    i = i + #open;
    local close = str:sub(i):match([[^"]]);
    while not close do
        local esc = str:sub(i):match("^\\([abfnrtv\\\"\'\n])");
        if esc ~= nil then
            res[#res + 1] = str_esc_tbl(esc);
            i = i + #esc;
        else
            esc = str:sub(i):match([[^\([0-9][0-9]?[0-9]?)]]);
            if esc ~= nil then
                res[#res + 1] = string.char(tonumber(esc));
                i = i + #esc;
            else
                esc = str:sub(i):match([[^\x([0-9][0-9])]]);
                if esc ~= nil then
                    res[#res + 1] = string.char(tonumber(esc, 16));
                    i = i + #esc;
                else
                    res[#res + 1] = str:sub(i, i);
                    i = i + 1;
                end
            end
        end
        close = str:sub(i):match([[^"]]);
    end
    return table.concat(res), i + #close;
end

local classes = {};  -- Need the class loaders
local word_obj_tbl = {
    nan = math.abs(0 / 0), ["-nan"] = -math.abs(0 / 0),
    inf = 1 / 0, ["-inf"] = (-1 / 0),
    ["true"] = true, ["false"] = false,
};
function obj_from_str(str, i)
    i = i or 1;
    local sp, classname = str:sub(i):match([[^( *)([%a_][%w_]*) *{]]);
    if classname ~= nil then
        i = i + #sp + #classname;
        local classobj, objdef;
        if classname == "class" then
            local classdef;
            classdef, i = tbl_from_str(str, i);
            classobj = class(classdef);
            if not str:sub(i):match("^ *{") then
                -- Return the anonymous class itself
                return classdef, i;
            end
        else
            classobj = classes[classname];
        end
        objdef, i = tbl_from_str(str, i);
        return classobj(objdef), i;
    end
    if str:sub(i):match([[^ *"]]) then
        return str_from_str(str, i);
    end
    if str:sub(i):match("^ *{") then
        return tbl_from_str(str, i);
    end
    for k, v in ipairs{
            [[^( *)(%-?[0-9]+[Ee]%-?[0-9]+)]],
            [[^( *)(%-?[0-9]*%.[0-9]+[Ee]%-?[0-9]+)]],
            [[^( *)(%-?[0-9]+%.[0-9]*)]],
            [[^( *)(%-?%.?[0-9]+)]]} do
        local sp, num = str:sub(i):match(v);
        if num ~= nil then
            return tonumber(num), i + #sp + #num;
        end
    end
    local sp, word = str:sub(i):match([[^( *)(%-?[%a_][%w_]*)]]);
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

local function each_ch(str) return str:gmatch("."); end
local function each_ch_utf8(str) return str:gmatch(".[\128-\191]*"); end

local function unpack_str(str, i, j)
    local res = {};
    for ch in each_ch(str:sub(i or 1, j)) do
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
    for k = 1, math.max(#tb1, #tb2), 1 do
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
            -- Avoid tail call, otherwise `getfenv` on Vanilla Lua will broken
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

-- Terminal color
local_class "Color"{};
function Color:new(...)
    local args = getargs({{"copy", "Color"}, "fg", {"bold", "boolean"}, "bg", "blink"}, ...);
    return {
        obj_copy(args.copy) or bit.bor(
            args.fg,
            bit.lshift(args.bold and 1 or 0, 3),
            bit.lshift(args.bg, 4),
            bit.lshift(args.blink and 1 or 0, 7)),
    };
end
function Color:decode()
    local color = self[1];
    return {
        fg = bit.band(color, 0x07),
        bold = bit.band(color, 0x08) ~= 0,
        bg = bit.rshift(bit.band(color, 0x70), 4),
        blink = bit.band(color, 0x80) ~= 0,
    };
end

-- Tetronimo cell
local_class "Cell"{};
Cell.State = {
    EMPTY = 1,
    FILLED = 2,
    GHOST = 3,
    FLOAT = 4,
};
Cell.StateCh = {
    [Cell.State.EMPTY] = "  ",
    [Cell.State.FILLED] = "[]",
    [Cell.State.GHOST] = "##",
    [Cell.State.FLOAT] = "[]",
};
function Cell:new(...)
    local args = getargs({{"copy", "Cell"}, {"state", "number"}, {"color", "Color"}, "fg"}, ...);
    return obj_copy(args.copy) or {
        state = args.state or Cell.State.FLOAT,
        color = args.color or Color{
            args.fg or 7,
            (args.state or Cell.State.FLOAT) ~= Cell.State.FILLED,
            0,
            false},
    };
end
Cell.R = Cell{fg = 1};  -- Red
Cell.G = Cell{fg = 2};  -- Green
Cell.Y = Cell{fg = 3};  -- Yellow
Cell.B = Cell{fg = 4};  -- Blue
Cell.M = Cell{fg = 5};  -- Magenta
Cell.C = Cell{fg = 6};  -- Cyan
Cell.W = Cell{fg = 7};  -- White
Cell.V = Cell{Cell.State.EMPTY};  -- Void

-- Tetronimo piece
local_class "Tetro"{};
function Tetro:new(...)
    local args = getargs({{"copy", "Tetro"}, "piece", "kick"}, ...);
    return args.copy or {
        piece = args.piece,
        kick = args.kick,
    };
end
do
    -- Orientation:
    -- rot0   - Initial
    -- rot90  - Rotated 90 degree clockwise from initial
    -- rot180 - Rotated 180 degree from initial
    -- rotc90 - Rotated 90 degree counter-clockwise from initial

    -- Cell shorthands
    local R, G, Y, B, P, C, W, _ = Cell.R, Cell.G, Cell.Y, Cell.B, Cell.M, Cell.C, Cell.W, Cell.V;
    -- Wall kick data for rotation (clockwise): {
    --     normal,
    --     rotated clockwise while left pressed or counter-clockwise while right-pressed,
    --     rotated clockwise while right pressed or counter-clockwise while left-pressed}
    -- {rot0 to rot90, rot90 to rot180, rot180 to rotc90, rotc90 to rot0}
    -- {offset 1, offset 2, offset 3, ...}
    -- {offset x, offset y}
    local kick_common = {
        {{{ 0,  0}, {-1,  0}, {-1,  1}, { 0, -2}, {-1, -2}},
         {{ 0,  0}, { 1,  0}, { 1, -1}, { 0,  2}, { 1,  2}},
         {{ 0,  0}, { 1,  0}, { 1,  1}, { 0, -2}, { 1, -2}},
         {{ 0,  0}, {-1,  0}, {-1, -1}, { 0,  2}, {-1,  2}}},
        {{{ 0,  0}, {-1,  0}, { 0,  1}, {-1,  1}, {-1, -2}, { 0, -2}, { 0, -1}},
         {{ 0,  0}, { 1,  0}, {-1, -1}, { 0, -1}, {-1,  2}, { 0,  2}, { 0,  1}},
         {{ 0,  0}, { 1,  0}, {-1,  1}, { 0,  1}, {-1, -2}, { 0, -2}, { 0, -1}},
         {{ 0,  0}, {-1,  0}, { 0, -1}, {-1, -1}, {-1,  2}, { 0,  2}, { 0,  1}}},
        {{{ 0,  0}, {-1,  0}, { 0,  1}, { 1,  1}, { 1, -2}, { 0, -2}, { 0, -1}},
         {{ 0,  0}, { 1,  0}, { 1, -1}, { 0, -1}, { 1,  2}, { 0,  2}, { 0,  1}},
         {{ 0,  0}, { 1,  0}, { 1,  1}, { 0,  1}, { 1, -2}, { 0, -2}, { 0, -1}},
         {{ 0,  0}, {-1,  0}, { 0, -1}, { 1, -1}, { 1,  2}, { 0,  2}, { 0,  1}}}};
    local kick_i = {
        {{{ 0,  0}, {-2,  0}, { 1,  0}, {-2, -1}, { 1,  2}},
         {{ 0,  0}, {-1,  0}, { 2,  0}, {-1,  2}, { 2, -1}},
         {{ 0,  0}, { 2,  0}, {-1,  0}, { 2,  1}, {-1, -2}},
         {{ 0,  0}, { 1,  0}, {-2,  0}, { 1, -2}, {-2,  1}}},
        {{{ 0,  0}, {-2,  0}, { 1,  0}, {-2, -1}, { 1,  2}, { 1, -1}, {-1, -1}},
         {{ 0,  0}, {-1,  0}, { 2,  0}, {-1,  2}, { 2, -1}, {-1,  1}, { 1, -1}},
         {{ 0,  0}, {-1,  0}, { 2,  0}, {-1, -2}, { 2,  1}, { 1, -1}, {-1, -1}},
         {{ 0,  0}, {-2,  0}, { 1,  0}, {-2,  1}, { 1, -2}, {-1,  1}, { 1, -1}}},
        {{{ 0,  0}, { 1,  0}, {-2,  0}, { 1,  2}, {-2, -1}, { 1,  1}, {-1, -1}},
         {{ 0,  0}, { 2,  0}, {-1,  0}, { 2, -1}, {-1,  2}, {-1, -1}, { 1, -1}},
         {{ 0,  0}, { 2,  0}, {-1,  0}, { 2,  1}, {-1, -2}, { 1,  1}, {-1, -1}},
         {{ 0,  0}, { 1,  0}, {-2,  0}, { 1, -2}, {-2,  1}, {-1, -1}, { 1, -1}}}};
    local kick_none = {
        {{{0, 0}}, {{0, 0}}, {{0, 0}}, {{0, 0}}},
        {{{0, 0}}, {{0, 0}}, {{0, 0}}, {{0, 0}}},
        {{{0, 0}}, {{0, 0}}, {{0, 0}}, {{0, 0}}}};
    -- Piece data:
    -- {initial,
    --  rotated 90 degree clockwise,
    --  rotated 180 degree,
    --  rotated 90 degree counter-clockwise}
    Tetro.I = Tetro(
        {{{_, _, _, _},
          {C, B, C, C},
          {_, _, _, _},
          {_, _, _, _}},
         {{_, _, C, _},
          {_, _, B, _},
          {_, _, C, _},
          {_, _, C, _}},
         {{_, _, _, _},
          {_, _, _, _},
          {C, C, B, C},
          {_, _, _, _}},
         {{_, C, _, _},
          {_, C, _, _},
          {_, B, _, _},
          {_, C, _, _}}}, kick_i);
    Tetro.J = Tetro(
        {{{B, _, _, _},
          {B, B, B, _},
          {_, _, _, _},
          {_, _, _, _}},
         {{_, B, B, _},
          {_, B, _, _},
          {_, B, _, _},
          {_, _, _, _}},
         {{_, _, _, _},
          {B, B, B, _},
          {_, _, B, _},
          {_, _, _, _}},
         {{_, B, _, _},
          {_, B, _, _},
          {B, B, _, _},
          {_, _, _, _}}}, kick_common);
    Tetro.L = Tetro(
        {{{_, _, Y, _},
          {Y, Y, Y, _},
          {_, _, _, _},
          {_, _, _, _}},
         {{_, Y, _, _},
          {_, Y, _, _},
          {_, Y, Y, _},
          {_, _, _, _}},
         {{_, _, _, _},
          {Y, Y, Y, _},
          {Y, _, _, _},
          {_, _, _, _}},
         {{Y, Y, _, _},
          {_, Y, _, _},
          {_, Y, _, _},
          {_, _, _, _}}}, kick_common);
    local orient_o = {
        {_, W, W, _},
        {_, W, W, _},
        {_, _, _, _},
        {_, _, _, _}};
    Tetro.O = Tetro({orient_o, orient_o, orient_o, orient_o}, kick_none);
    Tetro.Z = Tetro(
        {{{_, G, G, _},
          {G, Y, _, _},
          {_, _, _, _},
          {_, _, _, _}},
         {{_, G, _, _},
          {_, Y, G, _},
          {_, _, G, _},
          {_, _, _, _}},
         {{_, _, _, _},
          {_, Y, G, _},
          {G, G, _, _},
          {_, _, _, _}},
         {{G, _, _, _},
          {G, Y, _, _},
          {_, G, _, _},
          {_, _, _, _}}}, kick_common);
    Tetro.T = Tetro(
        {{{_, P, _, _},
          {P, P, P, _},
          {_, _, _, _},
          {_, _, _, _}},
         {{_, P, _, _},
          {_, P, P, _},
          {_, P, _, _},
          {_, _, _, _}},
         {{_, _, _, _},
          {P, P, P, _},
          {_, P, _, _},
          {_, _, _, _}},
         {{_, P, _, _},
          {P, P, _, _},
          {_, P, _, _},
          {_, _, _, _}}}, kick_common);
    Tetro.S = Tetro(
        {{{R, R, _, _},
          {_, Y, R, _},
          {_, _, _, _},
          {_, _, _, _}},
         {{_, _, R, _},
          {_, Y, R, _},
          {_, R, _, _},
          {_, _, _, _}},
         {{_, _, _, _},
          {R, Y, _, _},
          {_, R, R, _},
          {_, _, _, _}},
         {{_, R, _, _},
          {R, Y, _, _},
          {R, _, _, _},
          {_, _, _, _}}}, kick_common);
    local orient_v = {
        {_, _, _, _},
        {_, _, _, _},
        {_, _, _, _},
        {_, _, _, _}};
    Tetro.V = Tetro({orient_v, orient_v, orient_v, orient_v}, kick_none);
    for k, v in ipairs{Tetro.V, Tetro.I, Tetro.J, Tetro.L, Tetro.O, Tetro.Z, Tetro.T, Tetro.S} do
        Tetro[k - 1] = v;
    end
end

local_class "Draw_border"{};
function Draw_border:new(pos, size, pat)
    local r, c = unpack(size);
    local pat_disp = {};
    local pat_width = {};
    for r = 1, 4, 1 do
        pat_width[r] = {};
        pat_disp[r] = {};
        for c = 1, 2, 1 do
            local pa = pat[r][c];
            local w = bbs.str_width(pa);
            pat_width[r][c] = w;
            pat_disp[r][c] = (" "):rep(w) .. ("\b"):rep(w) .. pa;
        end
    end
    return {
        y = pos[1],
        x = pos[2],
        r = r,
        c = c,
        pat_disp = pat_disp,
        pat_width = pat_width,
    };
end
function Draw_border:vertical(kc, vc)
    for kr = 0, self.r - 1, 1 do
        bbs.move(self.y + kr, self.x + vc);
        bbs.addstr(self.pat_disp[1][kc]);
    end
    return self;
end
function Draw_border:horizontal(kr, vr)
    bbs.move(self.y + vr, self.x - 2);
    bbs.addstr(self.pat_disp[2][kr]);
    local kw = self.pat_width[3][kr];
    for k = 0, self.c - kw, kw do
        bbs.move(self.y + vr, self.x + k);
        bbs.addstr(self.pat_disp[3][kr]);
    end
    bbs.move(self.y + vr, self.x + self.c);
    bbs.addstr(self.pat_disp[4][kr]);
    return self;
end
function Draw_border:left() return self:vertical(1, -self.pat_width[1][1]); end
function Draw_border:right() return self:vertical(2, self.c); end
function Draw_border:top() return self:horizontal(1, -1); end
function Draw_border:bottom() return self:horizontal(2, self.r); end

local double_border_pat = {
    {[[║]], [[║]]},  -- left, right
    {[[╔]], [[╚]]},  -- top-left, bottom-left
    {[[═]], [[═]]},  -- top, bottom
    {[[╗]], [[╝]]},  -- top-right, bottom-right
};

-- Tetronimo board (cell size is 2x1)
-- 0-indexed to prevent confusions
--[[
y
  +--------------------+
-6|                    |
-5|                    |
-4|                    |
-3|                    |
-2|                    |
-1|                    |
-0|                    |
  +--------------------+
    0 1 2 3 4 5 6 7 8 9 x
]]
local_class "Board"{};
function Board:new(...)
    local args = getargs({{"copy", "Board"}, {"y", "number"}, {"x", "number"}, {"h", "number"}, {"w", "number"}, "data"}, ...);
    return obj_copy(args.copy) or {
        y = args.y or 0,
        x = args.x or 0,
        h = args.h,
        w = args.w,
        data = args.data or {};
    };
end
function Board:init_row(r)
    self.data[r] = {};
    for c = 0, self.w - 1, 1 do
        self.data[r][c] = Cell.V;
    end
end
function Board:init()
    for r = -(self.h - 1), 0, 1 do
        self:init_row(r);
    end
end
function Board:draw()
    local draw = Draw_border({self.y, self.x}, {self.h, 2 * self.w}, double_border_pat);
    bbs.attrset();
    draw:left():right():bottom();
    for kr = -(self.h - 1), 0, 1 do
        local row = self.data[kr];
        for kc = 0, self.w - 1, 1 do
            local vc = row[kc];
            local attr = vc.color:decode();
            bbs.move(self.y + self.h - 1 + kr, self.x + 2 * kc);
            bbs.attrset(
                30 + attr.fg,
                attr.bold and 1 or 22,
                40 + attr.bg,
                attr.blink and 5 or 25);
            bbs.addstr(Cell.StateCh[vc.state]);
        end
    end
    bbs.attrset();
    draw:top();
end

-- Tetronimo piece (cell size is 2x1)
-- 1-indexed
--[[
 y
  +--------+
 4|        |
 3|        |
 2|        |
 1|        |
  +--------+
    1 2 3 4 x
]]
function Tetro:child_pos(r, c, orient)
    return (r - 1) - (#self.piece[orient + 1] - 1), c - 1;
end
function Tetro:draw(pos, orient, posmin, poslimit)
    local piece = self.piece[orient + 1];
    for kr, vr in ipairs(piece) do
        local ypos = pos[1] + (self:child_pos(kr, 0, orient));
        if ypos >= posmin[1] and ypos < poslimit[1] then
            for kc, vc in ipairs(vr) do
                local xpos = pos[2] + 2 * (select(2, self:child_pos(0, kc, orient)));
                if xpos >= posmin[2] and xpos < poslimit[2] then
                    if vc.state ~= Cell.State.EMPTY then
                        local attr = vc.color:decode();
                        bbs.move(ypos, xpos);
                        bbs.attrset(
                        30 + attr.fg,
                        attr.bold and 1 or 22,
                        40 + attr.bg,
                        attr.blink and 5 or 25);
                        bbs.addstr(Cell.StateCh[vc.state]);
                    end
                elseif xpos >= poslimit[2] then
                    break;
                end
            end
        elseif ypos >= poslimit[1] then
            break;
        end
    end
end

local_class "Player"{};
function Player:new(...)
    local args = getargs({{"copy", "Player"}, {"board", "Board"}, "keybind"}, ...);
    return obj_copy(args.copy) or {
        board = args.board,
        keybind = args.keybind,
        hold = Tetro.V,
        hold_lock = false;
        tetro = Tetro.V,
        y = 0,
        x = 0,
        rot = 0,
        dx = 0,
    };
end
function Player:spawn()
    self.y = -11;
    self.x = 3;
    self.rot = 0;
end
function Player:next()
    self.tetro = Tetro[math.random(#Tetro)];
    self.hold_lock = false;
    self:spawn();
end
function Player:hold()
    if not self.hold_lock then
        self.tetro, self.hold = self.hold ~= Tetro.V and self.hold or Tetro[math.random(#Tetro)], self.tetro;
        self.hold_lock = true;
        self:spawn();
    end
end
function Player:draw()
    self.tetro:draw(
        {self.board.y + self.board.h - 1 + self.y, self.board.x + 2 * self.x},
        self.rot,
        {self.board.y, self.board.x - 2},
        {self.board.y + self.board.h + 1, self.board.x + 2 * (self.board.w + 1)});
    local hold_pos = {self.board.y, self.board.x + 2 * (self.board.w + 1 + 1)};
    local hold_size = {4, 2 * 4};
    local draw = Draw_border(hold_pos, hold_size, double_border_pat);
    bbs.move(unpack(hold_pos));
    bbs.attrset();
    bbs.rect(unpack(hold_size));
    self.hold:draw(
        {hold_pos[1] + hold_size[1] - 1, hold_pos[2]},
        0,
        hold_pos,
        {hold_pos[1] + hold_size[1], hold_pos[2] + hold_size[2]});
    draw:top():left():right():bottom();
end
function Player:piece_pos(r, c)
    local ypos, xpos = self.tetro:child_pos(r, c, self.rot);
    return self.y + ypos, self.x + xpos;
end
function Player:is_collided()
    local piece = self.tetro.piece[self.rot + 1];
    for kr, vr in ipairs(piece) do
        for kc, vc in ipairs(vr) do
            if vc.state == Cell.State.FLOAT then
                local pos = {self:piece_pos(kr, kc)};
                local vr_brd = self.board.data[pos[1]];
                local vc_brd = vr_brd and vr_brd[pos[2]] or nil;
                if vc_brd == nil or vc_brd.state == Cell.State.FILLED then
                    vr[kc] = Cell{Color(7, true, 4, true)};
                    self.board:draw();
                    self:draw();
                    bbs.attrset(0);
                    bbs.move(13, 40);
                    print("collided at ", tblstr(pos));
                    bbs.kbhit(0.1);
                    vr[kc] = vc;
                    self.board:draw();
                    return true;
                end
            end
        end
    end
    return false;
end
function Player:rotate(orient)
    local rot_orig = self.rot;
    local y_orig, x_orig = self.y, self.x;
    self.rot = bit.band(self.rot + orient, 0x3);

    local shift_idx = self.dx < 0 and 3 or self.dx > 0 and 2 or 1;
    local orient_idx = orient == 1 and rot_orig + 1 or self.rot + 1;
    bbs.move(14, 40);
    print(shift_idx, orient_idx, tblstr(self.tetro.kick[shift_idx][orient_idx]));
    for k, v in ipairs(self.tetro.kick[shift_idx][orient_idx]) do
        local v = orient == 1 and {v[1], -v[2]} or {-v[1], v[2]};
        bbs.move(15, 40);
        print("trying ", k, ": ", tblstr(v));
        self.y, self.x = y_orig + v[2], x_orig + v[1];
        if not self:is_collided() then
            return;
        end
    end
    self.y, self.x = y_orig, x_orig;
    self.rot = rot_orig;
end
function Player:rot90() self:rotate(1); end
function Player:rotc90() self:rotate(3); end
function Player:shift(dx)
    local x_orig = self.x;
    self.x = self.x + dx;
    if self:is_collided() then
        self.x = x_orig;
    end
end
function Player:lshift() self:shift(-1); end
function Player:rshift() self:shift(1); end
function Player:softdrop()
    local y_orig = self.y;
    self.y = self.y + 1;
    if self:is_collided() then
        self.y = y_orig;
    end
end
function Player:softlift()
    local y_orig = self.y;
    self.y = self.y - 1;
    if self:is_collided() then
        self.y = y_orig;
    end
end
function Player:harddrop()
    local piece = self.tetro.piece[self.rot + 1];
    for kr, vr in ipairs(piece) do
        for kc, vc in ipairs(vr) do
            if vc.state == Cell.State.FLOAT then
                local pos = {self:piece_pos(kr, kc)};
                local vr_brd = self.board.data[pos[1]];
                local vc_brd = vr_brd and vr_brd[pos[2]] or nil;
                if vc_brd.state ~= Cell.State.FILLED then
                    vr_brd[pos[2]] = Cell{Cell.State.FILLED, vc.color:decode().fg};
                end
            end
        end
    end
    for kr = -(self.board.h - 1), 0, 1 do
        local row = self.board.data[kr];
        for kc = 0, self.board.w - 1, 1 do
            local vc = row[kc];
            if vc.state ~= Cell.State.FILLED then
                break;
            end
            if kc == self.board.w - 1 then
                for kr_mv = kr - 1, -(self.board.h - 1), -1 do
                    self.board.data[kr_mv + 1] = self.board.data[kr_mv];
                end
                self.board:init_row(-(self.board.h - 1));
            end
        end
    end
    self:next();
end

local keybind = {
    UP = --[[Player.rot90]] Player.softlift,
    ["1"] = Player.rot90,
    ["5"] = Player.rot90,
    ["9"] = Player.rot90,
    DOWN = Player.softdrop,
    ["2"] = Player.softdrop,
    LEFT = Player.lshift,
    ["4"] = Player.lshift,
    RIGHT = Player.rshift,
    ["6"] = Player.rshift,
    ["3"] = Player.rotc90,
    ["7"] = Player.rotc90,
    Z = Player.rotc90,
    z = Player.rotc90,
    X = Player.rot90,
    x = Player.rot90,
    C = Player.hold,
    c = Player.hold,
    ["0"] = Player.hold,
    ["8"] = Player.softlift,
    [" "] = Player.harddrop,
    F1 = Player.pause,
    ["^Z"] = Player.pause,
};

local function get_skip_tbl(player)
    return Weak_set(arr_to_set{player.board, unpack(Tetro, 0)});
end

local function main(...)
    local board = Board(1, 6, 15, 10);
    local player = Player(board, keybind);
    board:init();
    player:next();
    bbs.clear();
    bbs.print(tblstr(player, get_skip_tbl(player)));
    bbs.pause();
    bbs.clear();
    -- Main loop
    local y = 0;
    while true do
        bbs.move(17);
        bbs.addstr(tblstr(player, get_skip_tbl(player)));
        board:draw();
        player:draw();

        local input = {bbs.kball(0.016)};
        bbs.move(16, 40);
        bbs.print(tblstr(input));
        for k, v in ipairs(input) do
            if player.keybind[v] == Player.lshift then
                player.dx = -1;
            elseif player.keybind[v] == Player.rshift then
                player.dx = 1;
            end
        end
        for k, v in ipairs(input) do
            (player.keybind[v] or noop)(player);
        end
        player.dx = 0;
    end
end

main(...);

--#BBSLUA
