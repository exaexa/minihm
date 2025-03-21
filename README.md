
# minihm

Toy implementation of Hindley-Milner typesystem (aka algorithm W) that prints
out the inference steps. It supports `let`-polymorphism with recursion (all
`let`s are single-binding-`letrec`s by default).

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
- pushed variable f into context with type α
  - inferring \x . \y . f y x
  - pushed variable x into context with type β
    - inferring \y . f y x
    - pushed variable y into context with type γ
      - inferring f y x
        - inferring f y
          - inferring f
          - found monomorphic f :: α
          - inferring y
          - found monomorphic y :: γ
        - assuming f y to return δ
        - solving unification (γ -> δ) = (α)
        - substitute α := γ -> δ
        - inferred f :: γ -> δ
        - inferred f y :: δ
        - inferring x
        - found monomorphic x :: β
      - assuming f y x to return ε
      - solving unification (β -> ε) = (δ)
      - substitute δ := β -> ε
      - inferred f y :: β -> ε
      - inferred f y x :: ε
    - popped variable y from context with type γ
    - inferred \y . f y x :: γ -> ε
  - popped variable x from context with type β
  - inferred \x . \y . f y x :: β -> γ -> ε
- popped variable f from context with type γ -> β -> ε
- inferred \f . \x . \y . f y x :: (γ -> β -> ε) -> β -> γ -> ε
>>> \f . \x . \y . f y x
 :: (a -> b -> c) -> b -> a -> c
```

### Inferring `fix`

```
hm> let fix = \f.f(fix f) in fix
- inferring let fix = \f . f (fix f) in fix
- pushed rec-variable fix into context with type α
  - inferring \f . f (fix f)
  - pushed variable f into context with type β
    - inferring f (fix f)
      - inferring f
      - found monomorphic f :: β
      - inferring fix f
        - inferring fix
        - found monomorphic fix :: α
        - inferring f
        - found monomorphic f :: β
      - assuming fix f to return γ
      - solving unification (β -> γ) = (α)
      - substitute α := β -> γ
      - inferred fix :: β -> γ
      - inferred fix f :: γ
    - assuming f (fix f) to return δ
    - solving unification (γ -> δ) = (β)
    - substitute β := γ -> δ
    - inferred f :: γ -> δ
    - inferred f (fix f) :: δ
  - popped variable f from context with type γ -> δ
  - inferred \f . f (fix f) :: (γ -> δ) -> δ
- solving unification ((γ -> δ) -> δ) = ((γ -> δ) -> γ)
- substitute δ := γ
- generalized let-variable fix to type forall α . (α -> α) -> α
  - inferring fix
  - found polymorphic fix :: forall α . (α -> α) -> α
  - instantiated fix :: (ε -> ε) -> ε
- popped let-variable fix from context
- inferred let fix = \f . f (fix f) in fix :: (ε -> ε) -> ε
>>> let fix = \f . f (fix f) in fix
 :: (a -> a) -> a
```

### Inferring the dreaded `(.).(.)`

```
hm> let compose = \f.\g.\x.f(g x) in compose compose compose
- inferring let compose = \f . \g . \x . f (g x) in compose compose compose
- pushed rec-variable compose into context with type α
  - inferring \f . \g . \x . f (g x)
  - pushed variable f into context with type β
    - inferring \g . \x . f (g x)
    - pushed variable g into context with type γ
      - inferring \x . f (g x)
      - pushed variable x into context with type δ
        - inferring f (g x)
          - inferring f
          - found monomorphic f :: β
          - inferring g x
            - inferring g
            - found monomorphic g :: γ
            - inferring x
            - found monomorphic x :: δ
          - assuming g x to return ε
          - solving unification (δ -> ε) = (γ)
          - substitute γ := δ -> ε
          - inferred g :: δ -> ε
          - inferred g x :: ε
        - assuming f (g x) to return ζ
        - solving unification (ε -> ζ) = (β)
        - substitute β := ε -> ζ
        - inferred f :: ε -> ζ
        - inferred f (g x) :: ζ
      - popped variable x from context with type δ
      - inferred \x . f (g x) :: δ -> ζ
    - popped variable g from context with type δ -> ε
    - inferred \g . \x . f (g x) :: (δ -> ε) -> δ -> ζ
  - popped variable f from context with type ε -> ζ
  - inferred \f . \g . \x . f (g x) :: (ε -> ζ) -> (δ -> ε) -> δ -> ζ
- solving unification ((ε -> ζ) -> (δ -> ε) -> δ -> ζ) = (α)
- substitute α := (ε -> ζ) -> (δ -> ε) -> δ -> ζ
- generalized let-variable compose to type forall α . forall β . forall γ . (α -> β) -> (γ -> α) -> γ -> β
  - inferring compose compose compose
    - inferring compose compose
      - inferring compose
      - found polymorphic compose :: forall α . forall β . forall γ . (α -> β) -> (γ -> α) -> γ -> β
      - instantiated compose :: (η -> θ) -> (ι -> η) -> ι -> θ
      - inferring compose
      - found polymorphic compose :: forall α . forall β . forall γ . (α -> β) -> (γ -> α) -> γ -> β
      - instantiated compose :: (κ -> λ) -> (μ -> κ) -> μ -> λ
    - assuming compose compose to return ν
    - solving unification (((κ -> λ) -> (μ -> κ) -> μ -> λ) -> ν) = ((η -> θ) -> (ι -> η) -> ι -> θ)
    - substitute η := κ -> λ
    - substitute θ := (μ -> κ) -> μ -> λ
    - substitute ν := (ι -> κ -> λ) -> ι -> (μ -> κ) -> μ -> λ
    - inferred compose :: ((κ -> λ) -> (μ -> κ) -> μ -> λ) -> (ι -> κ -> λ) -> ι -> (μ -> κ) -> μ -> λ
    - inferred compose compose :: (ι -> κ -> λ) -> ι -> (μ -> κ) -> μ -> λ
    - inferring compose
    - found polymorphic compose :: forall α . forall β . forall γ . (α -> β) -> (γ -> α) -> γ -> β
    - instantiated compose :: (ξ -> ο) -> (π -> ξ) -> π -> ο
  - assuming compose compose compose to return ρ
  - solving unification (((ξ -> ο) -> (π -> ξ) -> π -> ο) -> ρ) = ((ι -> κ -> λ) -> ι -> (μ -> κ) -> μ -> λ)
  - substitute ι := ξ -> ο
  - substitute κ := π -> ξ
  - substitute λ := π -> ο
  - substitute ρ := (ξ -> ο) -> (μ -> π -> ξ) -> μ -> π -> ο
  - inferred compose compose :: ((ξ -> ο) -> (π -> ξ) -> π -> ο) -> (ξ -> ο) -> (μ -> π -> ξ) -> μ -> π -> ο
  - inferred compose compose compose :: (ξ -> ο) -> (μ -> π -> ξ) -> μ -> π -> ο
- popped let-variable compose from context
- inferred let compose = \f . \g . \x . f (g x) in compose compose compose :: (ξ -> ο) -> (μ -> π -> ξ) -> μ -> π -> ο
>>> let compose = \f . \g . \x . f (g x) in compose compose compose
 :: (a -> b) -> (c -> d -> a) -> c -> d -> b
```
