
# minihm

Toy implementation of Hindley-Milner typesystem (aka algorithm W) that prints
out the inference steps. It supports `let`-polymorphism with recursion (all
`let`s are `letrec`s by default).

Mostly meant for education purposes.

Build and start with `cabal`:

```sh
cd minihm
cabal run
```

## Demo

### Inferring `flip`

```
hm> \f.\x.\y.f y x
- inferring \f . \x . \y . f y x
- pushed variable f into context with type 0
  - inferring \x . \y . f y x
  - pushed variable x into context with type 1
    - inferring \y . f y x
    - pushed variable y into context with type 2
      - inferring f y x
        - inferring f y
          - inferring f
          - found monomorphic f :: 0
          - inferring y
          - found monomorphic y :: 2
        - assuming f y to return 3
        - substitute: 0 := 2 -> 3
        - inferred: f :: 2 -> 3
        - inferred: f y :: 3
        - inferring x
        - found monomorphic x :: 1
      - assuming f y x to return 4
      - substitute: 3 := 1 -> 4
      - inferred: f y :: 1 -> 4
      - inferred: f y x :: 4
    - popped variable y from context with type 2
    - inferred: \y . f y x :: 2 -> 4
  - popped variable x from context with type 1
  - inferred: \x . \y . f y x :: 1 -> 2 -> 4
- popped variable f from context with type 2 -> 1 -> 4
- inferred: \f . \x . \y . f y x :: (2 -> 1 -> 4) -> 1 -> 2 -> 4
>>> \f . \x . \y . f y x
 :: (a -> b -> c) -> b -> a -> c
```

### Inferring `fix`

```
hm> let fix = \f.f(fix f) in fix
- inferring let fix = \f . f (fix f) in fix
- pushed rec-variable fix into context with type 0
  - inferring \f . f (fix f)
  - pushed variable f into context with type 1
    - inferring f (fix f)
      - inferring f
      - found monomorphic f :: 1
      - inferring fix f
        - inferring fix
        - found monomorphic fix :: 0
        - inferring f
        - found monomorphic f :: 1
      - assuming fix f to return 2
      - substitute: 0 := 1 -> 2
      - inferred: fix :: 1 -> 2
      - inferred: fix f :: 2
    - assuming f (fix f) to return 3
    - substitute: 1 := 2 -> 3
    - inferred: f :: 2 -> 3
    - inferred: f (fix f) :: 3
  - popped variable f from context with type 2 -> 3
  - inferred: \f . f (fix f) :: (2 -> 3) -> 3
- substitute: 3 := 2
- generalized let-variable fix to type (2 -> 3) -> 3
  - inferring fix
  - found polymorphic fix :: forall 4 . (4 -> 4) -> 4
  - instantiated fix :: (5 -> 5) -> 5
- popped let-variable fix from context
- inferred: let fix = \f . f (fix f) in fix :: (5 -> 5) -> 5
>>> let fix = \f . f (fix f) in fix
 :: (a -> a) -> a
```

### Inferring the dreaded `(.).(.)`

```
hm> let compose = \f.\g.\x.f(g x) in compose compose compose
- inferring let compose = \f . \g . \x . f (g x) in compose compose compose
- pushed rec-variable compose into context with type 0
  - inferring \f . \g . \x . f (g x)
  - pushed variable f into context with type 1
    - inferring \g . \x . f (g x)
    - pushed variable g into context with type 2
      - inferring \x . f (g x)
      - pushed variable x into context with type 3
        - inferring f (g x)
          - inferring f
          - found monomorphic f :: 1
          - inferring g x
            - inferring g
            - found monomorphic g :: 2
            - inferring x
            - found monomorphic x :: 3
          - assuming g x to return 4
          - substitute: 2 := 3 -> 4
          - inferred: g :: 3 -> 4
          - inferred: g x :: 4
        - assuming f (g x) to return 5
        - substitute: 1 := 4 -> 5
        - inferred: f :: 4 -> 5
        - inferred: f (g x) :: 5
      - popped variable x from context with type 3
      - inferred: \x . f (g x) :: 3 -> 5
    - popped variable g from context with type 3 -> 4
    - inferred: \g . \x . f (g x) :: (3 -> 4) -> 3 -> 5
  - popped variable f from context with type 4 -> 5
  - inferred: \f . \g . \x . f (g x) :: (4 -> 5) -> (3 -> 4) -> 3 -> 5
- substitute: 0 := (4 -> 5) -> (3 -> 4) -> 3 -> 5
- generalized let-variable compose to type (4 -> 5) -> (3 -> 4) -> 3 -> 5
  - inferring compose compose compose
    - inferring compose compose
      - inferring compose
      - found polymorphic compose :: forall 6 . forall 7 . forall 8 . (6 -> 7) -> (8 -> 6) -> 8 -> 7
      - instantiated compose :: (9 -> 10) -> (11 -> 9) -> 11 -> 10
      - inferring compose
      - found polymorphic compose :: forall 6 . forall 7 . forall 8 . (6 -> 7) -> (8 -> 6) -> 8 -> 7
      - instantiated compose :: (12 -> 13) -> (14 -> 12) -> 14 -> 13
    - assuming compose compose to return 15
    - substitute: 9 := 12 -> 13
    - substitute: 10 := (14 -> 12) -> 14 -> 13
    - substitute: 15 := (11 -> 12 -> 13) -> 11 -> (14 -> 12) -> 14 -> 13
    - inferred: compose :: ((12 -> 13) -> (14 -> 12) -> 14 -> 13) -> (11 -> 12 -> 13) -> 11 -> (14 -> 12) -> 14 -> 13
    - inferred: compose compose :: (11 -> 12 -> 13) -> 11 -> (14 -> 12) -> 14 -> 13
    - inferring compose
    - found polymorphic compose :: forall 6 . forall 7 . forall 8 . (6 -> 7) -> (8 -> 6) -> 8 -> 7
    - instantiated compose :: (16 -> 17) -> (18 -> 16) -> 18 -> 17
  - assuming compose compose compose to return 19
  - substitute: 11 := 16 -> 17
  - substitute: 12 := 18 -> 16
  - substitute: 13 := 18 -> 17
  - substitute: 19 := (16 -> 17) -> (14 -> 18 -> 16) -> 14 -> 18 -> 17
  - inferred: compose compose :: ((16 -> 17) -> (18 -> 16) -> 18 -> 17) -> (16 -> 17) -> (14 -> 18 -> 16) -> 14 -> 18 -> 17
  - inferred: compose compose compose :: (16 -> 17) -> (14 -> 18 -> 16) -> 14 -> 18 -> 17
- popped let-variable compose from context
- inferred: let compose = \f . \g . \x . f (g x) in compose compose compose :: (16 -> 17) -> (14 -> 18 -> 16) -> 14 -> 18 -> 17
>>> let compose = \f . \g . \x . f (g x) in compose compose compose
 :: (a -> b) -> (c -> d -> a) -> c -> d -> b
```
