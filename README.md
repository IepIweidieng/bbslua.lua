# bbslua.lua
Run BBS-Lua on LuaJIT

`./bbslua.lua <program-path>`

## Prerequest
- x86-64 Linux\
    For other platform, you may need to change the constant definition (`termio` and `clock`) in `bbslua.lua` to make it work correctly.
- LuaJIT >= 2.1.0-beta3\
    Older versions may work, but their are not tested.

## Implemented BBS-Lua API
All functions and constants specified in BBS-Lua API level 0.201 are implemented, with some extensions.

Table legend:
- _with extensions_
- **bbslua.lua extensions**
- ~~Incompatible with C implementation~~

### Library availability

| Library       | Description                                                        | Availability                                                                                                           | Note                                                                                             |
| ------------- | ------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| Basic         | Lua standard library                                               | - `print` aliases `bbs.print` <br> - `dofile`, `load`, `loadfile`, `loadstring` are disabled                           |                                                                                                  |
| `coroutine`   | Sub-library of basic library                                       | Full                                                                                                                   |                                                                                                  |
| **`package`** | Lua standard library                                               | - Import from file is disabled <br> - Only available standard/builtin modules and user-defined modules can be imported | Not in BBS-Lua                                                                                   |
| `string`      | Lua standard library                                               | Full                                                                                                                   |                                                                                                  |
| `table`       | Lua standard library                                               | Full                                                                                                                   |                                                                                                  |
| `math`        | Lua standard library                                               | Full                                                                                                                   |                                                                                                  |
| `io`          | Lua standard library                                               | Disabled                                                                                                               |                                                                                                  |
| `os`          | Lua standard library                                               | Disabled                                                                                                               |                                                                                                  |
| **`debug`**   | Lua standard library                                               | Getter only                                                                                                            | Not in BBS-Lua                                                                                   |
| `bit`         | [Lua BitOp](https://bitop.luajit.org/) <br> LuaJIT builtin library | Full <br> - `bit.cast` aliases to `bit.tobit`                                                                          | Older C implementations use [bitlib](http://luaforge.net/projects/bitlib/), which is deprecated. |
| `ffi`         | LuaJIT builtin library                                             | Disabled                                                                                                               |                                                                                                  |
| `jit`         | LuaJIT builtin library                                             | Disabled                                                                                                               |                                                                                                  |
| _`bbs`_       | BBS-Lua library                                                    | Full, with extension                                                                                                   | Cannot be imported                                                                               |
| `toc`         | BBS-Lua library                                                    | Full                                                                                                                   | Cannot be imported                                                                               |
| _`store`_     | BBS-Lua library                                                    | Full, with extension                                                                                                   | Cannot be imported                                                                               |

### `bbs` library

| Function            | Parameter ↦ Return Value                                      | Description                                                                                                                                                                                                                                                                                                                       | Note                                                                |
| ------------------- | ------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| `bbs.addstr`        | `(...)` ↦ `()`                                                | Concatenated arguments and print it on the cursor with graphic attribute of the cursor.                                                                                                                                                                                                                                           |                                                                     |
| `bbs.outs`          | `(...)` ↦ `()`                                                | An alias to `bbs.addstr`                                                                                                                                                                                                                                                                                                          |                                                                     |
| `bbs.title`         | ~~`(ttl)`~~ ↦ `()`                                            | Move cursor to position `(0, 0)` and print `ttl` with implementation-defined appearance for title.                                                                                                                                                                                                                                | `ttl` cannot be omitted.                                            |
| `bbs.print`         | `(...)` ↦ `()`                                                | Concatenated arguments, print it on the cursor with graphic attribute of the cursor, and then print a newline.                                                                                                                                                                                                                    |                                                                     |
| `print`             | `(...)` ↦ `()`                                                | An alias to `bbs.print`                                                                                                                                                                                                                                                                                                           |                                                                     |
| `bbs.getyx`         | `()` ↦ `(y: number, x: number)`                               | Get internal cursor position (not the physical cursor).                                                                                                                                                                                                                                                                           |                                                                     |
| `bbs.getmaxyx`      | `()` ↦ `(row: number, col: number)`                           | Get screen size.                                                                                                                                                                                                                                                                                                                  |                                                                     |
| `bbs.move`          | ~~`(y, x=0)`~~ ↦ `()`                                         | Move cursor to position `(y, x)` (0-indexed).                                                                                                                                                                                                                                                                                     | `y` cannot be omitted.                                              |
| `bbs.moverel`       | ~~`(dy, dx=0)`~~ ↦ `()`                                       | Relative cursor move                                                                                                                                                                                                                                                                                                              | `y` cannot be omitted.                                              |
| `bbs.clear`         | `()` ↦ `()`                                                   | Clear screen with default graphic attribute and move cursor to position `(0, 0)`                                                                                                                                                                                                                                                  |                                                                     |
| `bbs.clrtoeol`      | `()` ↦ `()`                                                   | - Clear from cursor to the line end with default graphic attribute. <br> - The cursor is kept at the original position.                                                                                                                                                                                                           |                                                                     |
| `bbs.clrtobot`      | `()` ↦ `()`                                                   | - Clear from cursor to the line end of the bottom line of screen with default graphic attribute. <br> - The cursor is kept at the original position.                                                                                                                                                                              |                                                                     |
| `bbs.rect`          | ~~`(r, c, ttl=nil)`~~ ↦ `()`                                  | - Draw a rectangle of size `r` × `c` with the graphic attribute of cursor. <br> - Print `ttl`, if presents, with the graphic attribute of cursor at the center of the rectangle. <br> - The cursor is kept at the original position.                                                                                              | `r`, `c` cannot be omitted.                                         |
| `bbs.refresh`       | `()` ↦ `()`                                                   | Flush buffered outputs to screen.                                                                                                                                                                                                                                                                                                 |                                                                     |
| `bbs.attrset`       | `(...)` ↦ `()`                                                | Change the graphic attribute of cursor.                                                                                                                                                                                                                                                                                           |                                                                     |
| `bbs.color`         | `(...)` ↦ `()`                                                | An alias to `bbs.attrset`                                                                                                                                                                                                                                                                                                         |                                                                     |
| `bbs.ANSI_COLOR`    | `(...)` ↦ `(str: string)`                                     | Get a ANSI escape string for changing the graphic attribute of cursor.                                                                                                                                                                                                                                                            |                                                                     |
| `bbs.strip_ansi`    | ~~`(str)`~~ ↦ `(stripped: string)`                            | Get a copy of `str` with all ANSI escapes removed.                                                                                                                                                                                                                                                                                | `str` cannot be omitted.                                            |
| **`bbs.str_width`** | `(str)` ↦ `(w: number)`                                       | **Get displayed width of string (assume UTF-8)**                                                                                                                                                                                                                                                                                  |                                                                     |
| `bbs.getch`         | `()` ↦ `(key: string)`                                        | Wait for a key from user input. Return that key.                                                                                                                                                                                                                                                                                  |                                                                     |
| `bbs.getdata`       | `(w, echo=bbs.DOECHO, str="")` ↦ `(data: string)`             | - Open an input field at the cursor and wait for the input to complete. <br> - Return the input. <br> - `echo` controls how the input is displayed and processed. <br> - `0` causes the input text to be invisible. <br> - Other values have implementation-defined effects. <br> - `str` is the initial text in the input field. | See the constant table for available `echo` values in `bbslua.lua`. |
| `bbs.getstr`        | `(w, echo=bbs.DOECHO, str="")` ↦ `(data: string)`             | An alias to `bbs.getdata`                                                                                                                                                                                                                                                                                                         |                                                                     |
| `bbs.pause`         | `(msg=nil)` ↦ `(key: string)`                                 | - Display a pause message `msg` with implementation-defined appearance. The message can be dismissed with any key. <br> - If `msg` presents, another implementation-defined appearance may be used instead. <br> - Return the key used to dismiss the message.                                                                    |                                                                     |
| `bbs.kbhit`         | `(sec)` ↦ `(hit: boolean)`                                    | - Wait for a key from user input for atmost `sec` seconds (can be a decimal). <br> - Return whether any key is pressed. <br> - The key, if any, remains unprocessed.                                                                                                                                                              |                                                                     |
| `bbs.kbreset`       | `()` ↦ `()`                                                   | Ignore all unprocessed buffered inputs.                                                                                                                                                                                                                                                                                           |                                                                     |
| `bbs.kball`         | `(sec)` ↦ `(keys: string...)`                                 | Wait for `sec` seconds (can be a decimal) and then return all keys pressed during the waiting.                                                                                                                                                                                                                                    |                                                                     |
| _`bbs.time`_        | _`(table=nil)`_ ↦ `(time: number)`                            | Get a number representing the time. <br> - _`table` can be used to specify a time different from now._                                                                                                                                                                                                                            | Aliases to `os.time(table)` in `bbslua.lua`                         |
| _`bbs.now`_         | _`(table=nil)`_ ↦ `(time: number)`                            | An alias to `bbs.time`                                                                                                                                                                                                                                                                                                            |                                                                     |
| _`bbs.ctime`_       | _`(format: string=nil, time: number=nil)`_ ↦ `(time: string)` | Get the formatted time. <br> - _`format` can be used to specify the format to use._ <br> - _`time` can be used to specify the time to format._                                                                                                                                                                                    | Aliases to `os.date(format, time)` in `bbslua.lua`                  |
| `bbs.clock`         | `()` ↦ `(time: number)`                                       | Get the current time from a timer with higher resolution than `bbs.time`.                                                                                                                                                                                                                                                         |                                                                     |
| `bbs.sleep`         | `(sec)` ↦ `()`                                                | Sleep for `sec` seconds (can be a decimal).                                                                                                                                                                                                                                                                                       |                                                                     |

| Constant           | Value                                  | Description                                                                     | Note                                                     |
| ------------------ | -------------------------------------- | ------------------------------------------------------------------------------- | -------------------------------------------------------- |
| `bbs.ANSI_RESET`   | `"\x1b[m": string`                     | A ANSI escape string for resetting the graphic attribute of cursor.                                                                                |                                                          |
| `bbs.ESC`          | `"\x1b": string`                       | The ESC character                                                               |                                                          |
| `bbs.userid`       | `<Defaults to "guest">: string`        | The identifier string of the user                                               |                                                          |
| `bbs.usernick`     | `<Defaults to "夢之大地訪客">: string` | The display name of the user                                                    |                                                          |
| `bbs.sitename`     | `<Defaults to "DreamBBS": string`      | The name of the site                                                            |                                                          |
| `bbs.interface`    | ~~`"0.201": string`~~                  | The BBS-Lua API level of the implementation                                     | `number` in the C implementation.                        |
| **`bbs.NOECHO`**   | `0x0: number`                          | The `echo` value for instructing `bbs.getdata` to make the input text invisible | Has the same effect as `bbs.HIDEECHO` in `bbslua.lua`.   |
| **`bbs.DOECHO`**   | `0x1: number`                          | The default `echo` value for `bbs.getdata`                                      | Not a flag, but can be combined with other `echo` flags. |
| **`bbs.LCECHO`**   | `0x2: number`                          | The `echo` flag for instructing `bbs.getdata` to convert input into lowercases  | Can be combined with other `echo` flags.                 |
| **`bbs.NUMECHO`**  | `0x4: number`                          | Instruct `bbs.getdata` to accept only number digits                             | Can be combined with others.                             |
| **`bbs.PASSECHO`** | `0x10: number`                         | Instruct `bbs.getdata` to draw the input text as asterisks.                     | Can be combined with others.                             |
| **`bbs.HIDEECHO`** | `0x20: number`                         | Instruct `bbs.getdata` to not drawing the input field at all.                   | Can be combined with others.                             |

### `toc` library

| Constant        | Type     | Description                                                  | Note                      |
| --------------- | -------- | ------------------------------------------------------------ | ------------------------- |
| `toc.interface` | `string` | The required BBS-Lua API level                               |                           |
| `toc.title`     | `string` | Program title                                                |                           |
| `toc.notes`     | `string` | Additional notes for the program                             |                           |
| `toc.author`    | `string` | Author information                                           |                           |
| `toc.version`   | `string` | Version string assigned by author                            |                           |
| `toc.date`      | `string` | Last modified date                                           |                           |
| `toc.latestref` | `string` | Reference to the file with the latest version of the program | No effect in `bbslua.lua` |

### `store` library

| Function        | Parameter ↦ Return Value                                        | Description                                                                                                                                                   | Note |
| --------------- | --------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---- |
| _`store.load`_  | `(cate: string)` ↦ _`(res: string, err: string)`_               | - Load the file for given category as a string. Return `nil` if fails <br> _- When operation fails, `err` is a string explaining the reason_                  |      |
| _`store.save`_  | `(cate: string, str: string)` ↦ _`(res: boolean, err: string)`_ | - Save a string as the file for given category. Return whether the operation success   <br> _- When operation fails, `err` is a string explaining the reason_ |      |
| `store.limit`   | `(cate: string)`                                                | Get the maximum permitted file size for given category, in bytes                                                                                              |      |
| `store.iolimit` | `()` ↦ `(max: number)`                                          | Get the maximum permitted number of times that `store.load` and `store.save` may open files                                                                   |      |

| Constant       | Value              | Description                                      | Note |
| -------------- | ------------------ | ------------------------------------------------ | ---- |
| `store.USER`   | `"user": string`   | The category for per user storage of the program |      |
| `store.GLOBAL` | `"global": string` | The category for sitewide storage of the program |      |

### `bit` library (Lua BitOp with extension)

| Function       | Parameter ↦ Return Value                        | Description                                                                                                           | Note |
| -------------- | ----------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- | ---- |
| `bit.tobit`    | `(x: number)` ↦ `(res: number)`                 | - Get a range-normalized copy a number <br> - Not necessary for other bitwise operations                              |      |
| **`bit.cast`** | `(x: number)` ↦ `(res: number)`                 | **An alias to `bit.tobit` for compatibility with bitlib**                                                             |      |
| `bit.tohex`    | `(x: number, n: number=8)` ↦ `(res: string)`    | - Get a `n`-digit hexadecimal number string of `x` <br> - Negative `n` produces a uppercase hexadecimal number string |      |
| `bit.bnot`     | `(x: number)` ↦ `(res: number)`                 | Bitwise `not`                                                                                                         |      |
| `bit.bor`      | `(x1: number, x2: number...)` ↦ `(res: number)` | Bitwise `or` of all arguments                                                                                         |      |
| `bit.band`     | `(x1: number, x2: number...)` ↦ `(res: number)` | Bitwise `and` of all arguments                                                                                        |      |
| `bit.bxor`     | `(x1: number, x2: number...)` ↦ `(res: number)` | Bitwise `xor` of all arguments                                                                                        |      |
| `bit.lshift`   | `(x: number, n: number)` ↦ `(res: number)`      | Bitwise left-shift. Use only the lowest 5 bits of `n`                                                                 |      |
| `bit.rshift`   | `(x: number, n: number)` ↦ `(res: number)`      | Bitwise logical right-shift. Use only the lowest 5 bits of `n`                                                        |      |
| `bit.arshift`  | `(x: number, n: number)` ↦ `(res: number)`      | Bitwise arithmetic right-shift. Use only the lowest 5 bits of `n`                                                     |      |
| `bit.rol`      | `(x: number, n: number)` ↦ `(res: number)`      | Bitwise right rotation. Use only the lowest 5 bits of `n`                                                             |      |
| `bit.ror`      | `(x: number, n: number)` ↦ `(res: number)`      | Bitwise left rotation. Use only the lowest 5 bits of `n`                                                              |      |
| `bit.bswap`    | `(x: number)` ↦ `(res: number)`                 | Get a copy of `x` with byte order reversed                                                                            |      |