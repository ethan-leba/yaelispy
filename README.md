
**YAE-Lispy** is _Yet-Another-Evil_ [Lispy](https://github.com/abo-abo/lispy)
variant that aims to make Lispy familiar and intuitive to the Vim user while
retaining the full power of Lispy's structural editing capabilities.

YAE-Lispy differs from it's peers (see [lispyville](https://github.com/noctuid/lispyville) and [sp3ctum/evil-lispy](https://github.com/sp3ctum/evil-lispy)) in the following goals:

#### 1. Vimify Lispy as much as possible.

YAE-Lispy leverages the Vimmer's muscle memory and minimizes the need to
learn/use Emacs binding with lispy, and thus provides **a complete overhaul of
Lispy's keybindings**. The keybindings are closely aligned to Vim's Visual
state, while differing where sensible. In addition to rebinding keys to more
familiar positions, YAE-Lispy includes some primitive operators to the
keymapping that vanilla Lispy intended to be accessed via. the Emacs bindings.

#### 2. Make Lispy the primary state.

In contrast to other evil-flavored lispy variants, which provide support for a
hybrid Lispy/Normal state approach, dipping into the featureset of Lispy while
remaining in the comfort of Normal state. YAE-Lispy opts for a different
approach, **intending for users to primarily use Lispy state** while allowing
for transient forays into Normal state. 

The key <kbd>n</kbd> will drop the user into Normal state, and will be sent back
to Lispy state under the following conditions:

1. An edit is made to the buffer in normal mode.
2. Insert mode is exited.

The main intended usecase is to modify single symbols. The YAE-Lisper can
perform a quick operation on a symbol, and get dropped right back into the Lispy
state on completion.

#### 3. Make insertion vs. structure editing explicit.

YAE-Lispy uses the Vimmer's familiar Insert state instead of implicitly
dispatching based on point location, as vanilla Lispy and evil-lispy does. One
large benefit to this is that the <kbd>SPC</kbd> key is now freed to operate as
a leader key. _(\*cough\* doom/spacemacs \*cough\*)_ Insert mode is entered with
<kbd>i</kbd> (unsurprisingly).

#### 4. No knowledge of vanilla Lispy required.

Lispy is an incredibly powerful tool, but along with it's power comes great
complexity -- the following sections serve to give a short and sweet overview of
Lispy's capabilities from the context of YAE-Lispy's modifications that users
with no experience with Lispy/Paredit/Structural Editing can hit the ground
running with.


## A quick recap on structural editing for the uninitiated

While most text editors operate on a character level, in Lispy state we operate
via the tree structure of the program itself, which conveniently maps to the
sexp structure.

For example, consider the venerable `append` function.

```
(defun append (x y)
  (cond
   ((not x) y)
   (t (cons (car x) (append (cdr x) y)))))
```

Here is the same function represented as a tree, where `s` marks a node on the
tree.

```text
   ┌─────┬───s───┬────────────┐
   │     │       │            │
   │     │     ┌─s─┐    ┌─────s─────────────┐
   ▼     ▼     │   │    ▼     │             │
defun append   │   │   cond   │             │
               │   │          │     ┌───────s────────────────┐
               ▼   ▼          │     ▼       │                │
               x   y          │    cons     │                │
                          ┌───s───┐     ┌───s───┐      ┌─────s─────┐
                          │       │     │       │      │     │     │
                          │       ▼     ▼       ▼      ▼     │     ▼
                          │       y    car      x    append  │     y
                       ┌──s──┐                           ┌───s───┐
                       │     │                           │       │
                       │     │                           │       │
                       ▼     ▼                           ▼       ▼
                      not    x                          cdr      x
```

We can see that leaves correspond to symbols, while the nodes correspond to lists.

## Keybindings

First off, here are the keybindings in YAE-Lispy that map very closely to Vim's bindings.

| key                | command                                                                      |
| ------------------ | -------------------------------------------------------------------          |
| <kbd>h</kbd>       | [`lispy-backward`](http://abo-abo.github.io/lispy/#lispy-backward)           |
| <kbd>j</kbd>       | [`lispy-down`](http://abo-abo.github.io/lispy/#lispy-down)                   |
| <kbd>k</kbd>       | [`lispy-up`](http://abo-abo.github.io/lispy/#lispy-up)                       |
| <kbd>l</kbd>       | [`lispy-forward`](http://abo-abo.github.io/lispy/#lispy-forward)             |
| <kbd>d</kbd>       | [`lispy-kill-at-point`](http://abo-abo.github.io/lispy/#lispy-kill-at-point) |
| <kbd>c</kbd>       | [`yae-lispy-change`](http://abo-abo.github.io/lispy/#yae-lispy-change)       |
| <kbd>p</kbd>       | [`lispy-paste`](http://abo-abo.github.io/lispy/#lispy-down)                  |
| <kbd>y</kbd>       | [`lispy-new-copy`](http://abo-abo.github.io/lispy/#lispy-new-copy)           |
| <kbd>u</kbd>       | [`lispy-undo`](http://abo-abo.github.io/lispy/#lispy-undo)                   |
| <kbd>i</kbd>       | [`yae-lispy-insert`](#yae-lispy-insert)                                      |


FOO

| key                | command                                                             |
| ------------------ | ------------------------------------------------------------------- |
| <kbd>h</kbd>       | [`lispy-backward`](http://abo-abo.github.io/lispy/#lispy-backward)  |

### Reversible commands

A lot of Lispy commands come in pairs - one reverses the other:

 key            | command                       | key                              | command
----------------|-------------------------------|----------------------------------|----------------------
 <kbd>j</kbd>   | `lispy-down`                  | <kbd>k</kbd>                     | `lispy-up`
 <kbd>s</kbd>   | `lispy-move-down`             | <kbd>w</kbd>                     | `lispy-move-up`
 <kbd>></kbd>   | `lispy-slurp`                 | <kbd><</kbd>                     | `lispy-barf`
 <kbd>c</kbd>   | `lispy-clone`                 | <kbd>C-d</kbd> or <kbd>DEL</kbd> |
 <kbd>C</kbd>   | `lispy-convolute`             | <kbd>C</kbd>                     | reverses itself
 <kbd>d</kbd>   | `lispy-different`             | <kbd>d</kbd>                     | reverses itself
 <kbd>M-j</kbd> | `lispy-split`                 | <kbd>+</kbd>                     | `lispy-join`
 <kbd>O</kbd>   | `lispy-oneline`               | <kbd>M</kbd>                     | `lispy-multiline`
 <kbd>S</kbd>   | `lispy-stringify`             | <kbd>C-u "</kbd>                 | `lispy-quotes`
 <kbd>;</kbd>   | `lispy-comment`               | <kbd>C-u ;</kbd>                 | `lispy-comment`
 <kbd>xi</kbd>  | `lispy-to-ifs`                | <kbd>xc</kbd>                    | `lispy-to-cond`
 <kbd>x></kbd>  | `lispy-toggle-thread-last`    | <kbd>x></kbd>                    | reverses itself

### Keys that modify whitespace

These commands handle whitespace in addition to inserting the expected
thing.

 key            | command
----------------|---------------------------
 <kbd>SPC</kbd> | `lispy-space`
 <kbd>:</kbd>   | `lispy-colon`
 <kbd>^</kbd>   | `lispy-hat`
 <kbd>C-m</kbd> | `lispy-newline-and-indent`

### Command chaining

Most special commands will leave the point special after they're
done.  This allows to chain them as well as apply them
continuously by holding the key.  Some useful hold-able keys are
<kbd>jkf<>cws;</kbd>.
Not so useful, but fun is <kbd>/</kbd>: start it from `|(` position and hold
until all your Lisp code is turned into Python :).

### Navigating with `avy`-related commands

 key            | command
----------------|--------------------------
 <kbd>q</kbd>   | `lispy-ace-paren`
 <kbd>Q</kbd>   | `lispy-ace-char`
 <kbd>a</kbd>   | `lispy-ace-symbol`
 <kbd>H</kbd>   | `lispy-ace-symbol-replace`
 <kbd>-</kbd>   | `lispy-ace-subword`

<kbd>q</kbd> - `lispy-ace-paren` jumps to a "(" character within current
top-level form (e.g. `defun`). It's much faster than typing in the
`avy` binding + selecting "(", and there's less candidates,
since they're limited to the current top-level form.

<kbd>a</kbd> - `lispy-ace-symbol` will let you select which symbol to
mark within current form. This can be followed up with e.g. eval,
describe, follow, raise etc. Or you can simply <kbd>m</kbd> to
deactivate the mark and edit from there.

<kbd>-</kbd> - `lispy-ace-subword` is a niche command for a neat combo. Start with:

    (buffer-substring-no-properties
     (region-beginning)|)

Type <kbd>c</kbd>, <kbd>-</kbd>, <kbd>b</kbd> and <kbd>C-d</kbd> to get:

    (buffer-substring-no-properties
     (region-beginning)
     (region-|))

Fill `end` to finish the statement.

# Operating on regions
Sometimes the expression that you want to operate on isn't bounded by parens.
In that case you can mark it with a region and operate on that.

## Ways to activate region
While in special:
- Mark a sexp with <kbd>m</kbd> - `lispy-mark-list`
- Mark a symbol within sexp <kbd>a</kbd> - `lispy-ace-symbol`.

While not in special:
- <kbd>C-SPC</kbd> - `set-mark-command`
- mark a symbol at point with <kbd>M-m</kbd> - `lispy-mark-symbol`
- mark containing expression (list or string or comment) with <kbd>C-M-,</kbd> - `lispy-mark`

## Move region around

The arrow keys <kbd>j</kbd>/<kbd>k</kbd> will move the region up/down within the current
list.  The actual code will not be changed.

## Switch to the other side of the region

Use <kbd>d</kbd> - `lispy-different` to switch between different sides
of the region. The side is important since the grow/shrink operations
apply to current side of the region.

## Grow/shrink region

Use a combination of:
- <kbd>></kbd> - `lispy-slurp` - extend by one sexp from the current side. Use digit
  argument to extend by several sexps.
- <kbd><</kbd> - `lispy-barf` - shrink by one sexp from the current side. Use digit
  argument to shrink by several sexps.

The other two arrow keys will mark the parent list of the current region:

- <kbd>h</kbd> - `lispy-left` - mark the parent list with the point on the left
- <kbd>l</kbd> - `lispy-right` - mark the parent list with the point on the right

To do the reverse of the previous operation, i.e. to mark the first
child of marked list, use <kbd>i</kbd> - `lispy-tab`.

## Commands that operate on region
- <kbd>m</kbd> - `lispy-mark-list` - deactivate region
- <kbd>c</kbd> - `lispy-clone` - clone region and keep it active
- <kbd>s</kbd> - `lispy-move-down` - move region one sexp down
- <kbd>w</kbd> - `lispy-move-up` - move region one sexp up
- <kbd>u</kbd> - `lispy-undo` - deactivate region and undo
- <kbd>t</kbd> - `lispy-teleport` - move region inside the sexp you select with `lispy-ace-paren`
- <kbd>C</kbd> - `lispy-convolute` - exchange the order of application of two sexps that contain region
- <kbd>n</kbd> - `lispy-new-copy` - copy region as kill without deactivating the mark
- <kbd>P</kbd> - `lispy-paste` - replace region with current kill

### YAE-Lispy specific commands

#### `yae-lispy-insert`



#### Post command behavior
1. Jump to closest left paren
2. Enter insert mode
3. Enter insert mode if point is not special
