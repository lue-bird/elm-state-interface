## from model-view-update-sub-cmd to state-interface

Many patterns evolved to adapt "the elm architecture" (TEA) for different needs. Examples
- out-msg pattern
- config pattern
- effect pattern
- nesting
- only top-level

We'll explore a nicer alternative to make the traditional model-view-update simpler and safer

## names we'll use
- event, usually called msg in elm
- state/app state, usually called model in elm
- interface, called view and subscriptions in elm
- `...Interface.on`, usually called update in elm
- state-interface, called TEA and model-view-update/MVU in elm

## topics
The core ideas for reform:
- ["event with state"](event-with-state.md)

Summaries of existing patterns
- ["event to super"](event-to-super.md)
