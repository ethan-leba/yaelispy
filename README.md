
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
a leader key. _(\*cough\* doom/spacemacs \*cough\*)_
