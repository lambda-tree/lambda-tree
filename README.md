# Type proof checker for lambda calculus

## Implementation Progress

Model:
- [ ] Lambda calculus core
    - [x] Term expressions
    - [x] Type expressions
    - [x] Context expressions
    - [x] Beta reduction on types (System F)
    - [ ] Type inference (H-M)
- [x] Parsing
    - [x] Types
    - [x] Terms
    - [x] Context
- [ ] Rule Checking
    - [x] T-True
    - [x] T-False
    - [x] T-Var
    - [x] T-Abs
    - [x] T-App
    - [x] T-If
    - [ ] T-Let
    - [ ] T-Inst
    - [ ] T-Gen
    - [ ] T-Var' (T-Var + T-Inst Combined rule for H-M)
    - [ ] T-Let' (T-Let + T-Gen Combined rule for H-M)
    - [ ] T-TAbs
    - [ ] T-TApp
- [ ] Whole tree checking
- [ ] Export to LaTex

UI:
- [x] Building tree from rules
- [x] Simple manual mode (type-in everything)
- [ ] Interactive mode


## Installation
- Install elm 0.19.0 from https://guide.elm-lang.org/install.html
- Run: `elm make src/Main.elm`
- Open `index.html` in browser

## Tests
- Tests are in `tests/` directory
- Run: `elm-test`

## Development
- Run: `elm reactor`
- Open listed server URL in browser
- Navigate to Main.elm

