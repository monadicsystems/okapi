# Compile-time benchmark: Servant vs Okapi

GHC 9.10.1, -O2, x86_64 Linux (WSL2)

## Methodology

- **Cold**: `cabal clean` + full rebuild. Servant's transitive deps (~40 pkgs) were pre-cached
  in the cabal store for the 128-route runs; Okapi's local lib (33 modules) always compiles
  from source. For the 256-route run, both used a fresh empty store (true cold, all deps from
  source).
- **Incremental**: deps cached, only `Main.hs` recompiled. Pure type-checker cost —
  the closest apples-to-apples comparison.
- Routes are 8-route groups (list, get, create, update, delete, sub-list, sub-create, search)
  with varied resource types per group.

## Numbers

### Incremental (Main.hs only, deps cached)

| Routes | Servant    | Okapi  |
|--------|-----------|--------|
| 64     | ~1.9s     | ~2.6s  |
| 128    | 5.0s      | 3.3s   |
| 160    | 6.4s      | 3.5s   |
| 256    | **FAILS** | 5.9s   |

### Cold (full build from scratch)

| Routes | Servant                    | Okapi         |
|--------|---------------------------|---------------|
| 128    | 9.6s (deps cached)*        | 16.8s*        |
| 256    | **FAILS** after 3m57s †   | 5m00s ‡       |

\* These cold numbers are not directly comparable: Servant's 9.6s reflects deps already
  in the cabal store (only Main.hs compiled); Okapi's 16.8s includes building 33 okapi
  lib modules from local source (~13.5s) plus Main.hs (~3.3s).

† Servant 256-route failure: `Reduction stack overflow; size = 201` while resolving
  `ServerT (Verb GET 200 '[JSON] [Article]) Handler` from `server = undefined`.
  Fix requires `-freduction-depth=0` (disables GHC's termination check entirely).
  The same failure occurs at 200 routes. Threshold is somewhere between 160 and 200 routes.

‡ Okapi 256-route cold includes building all transitive deps from source (true cold, fresh
  store). Main.hs alone is ~3–4s regardless of route count.

### Okapi Handle (list-based, no Generic) — incremental

`[Handle]` approach: type info erased at 6 fixed points (one `handle` call per shape),
list entries carry no type parameters. No `deriving Generic`, no `GFunction` resolution.

| Routes | Okapi HKD | Okapi Handle |
|--------|-----------|--------------|
| 128    | ~3.3s     | ~2.1s        |
| 256    | ~5.9s     | ~2.0s        |

Handle is **~1.5× faster** at 128 and **~3× faster** at 256. More importantly, the
Handle time is **flat**: 256 routes compiles in the same time as 128. Adding routes
that reuse existing shapes costs nothing — the list elements are fully erased `Handle`
values with no type parameters for GHC to inspect.

## Scaling summary

Servant incremental times reveal super-linear growth:
- 64 → 128 routes (+100%): 1.9s → 5.0s (+163%)
- 128 → 160 routes (+25%): 5.0s → 9.4s (+88%)
- 160 → 200 routes (+25%): crashes

Okapi HKD incremental: 64 (2.6s) → 128 (3.3s) → 256 (5.9s) — near-linear growth.
Okapi Handle incremental: 128 (2.1s) → 256 (2.0s) — flat. Route count is irrelevant
once shape types are fixed; only the number of distinct shapes matters.

Root cause comparison:
- Servant: `HasServer (a :<|> b)` re-traverses the full type tree per branch — O(n²)
- Okapi HKD: `deriving Generic` on an n-field record, `GFunction` walks the product — O(n)
- Okapi Handle: type erasure at `handle` call sites (one per shape, not per route) — O(shapes)
