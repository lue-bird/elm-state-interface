## from model-view-msg-update-sub-cmd-task to state-interface

We'll explore an architecture similar to the traditional model-view-update
which is simpler and safer.

→ ["event with state"](event-with-state.md)

## planned before release

mirror elm commands & tasks listed in
  - https://github.com/lamdera/program-test/blob/b64f089d7ef846b58bda73dd1780819e7b5e5d82/src/Effect/Internal.elm#L80
  - https://github.com/andrewMacmurray/elm-concurrent-task/blob/1.0.0/runner/index.ts


## summaries of existing patterns

Many patterns evolved to adapt "the elm architecture" (TEA) for different needs. Examples
- out-msg pattern
- config pattern
- effect pattern
- nesting
- only top-level

read
  - ["event to super" (not public, yet)](event-to-super.md)
