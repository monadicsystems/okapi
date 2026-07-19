---
title: Qualifying the HTTP DSL
subtitle: A reference for Okapi's module layout and its qualify-everything convention
author: Okapi
date: July 18, 2026
---

# Introduction

Okapi's HTTP codec DSL is spread across roughly twenty modules — a core
combinator engine, shared `Headers`/`Body` codecs, per-side `Request`/
`Response` facades, a full RFC 9651 Structured Field Values
implementation, and the value-level types each side's parser/printer
produce and consume. This document is a reference: what each import
gives you, why everything is designed to be qualified, and a worked
example putting it together.

**The rule the whole design follows: every name is qualified, always,
including field names inside record-update syntax.** This isn't a style
preference — it's load-bearing. Several types this DSL exposes genuinely
share field names and even type names across modules (`Request`'s
`path`/`headers`/`body` versus `Response`'s `headers`/`body`, or
`Okapi.Data.Request`'s `Request` versus `Okapi.HTTP.Request`'s own
`Request`). A qualified-only import never brings its fields into
unqualified scope, so it can never contribute a false candidate to GHC's
record-update disambiguation — that's the actual mechanism qualifying
everything buys you, not just readability (Part 3).

Every claim here was checked against the real library — the worked
example at the end is real, compiling code, verified against `lib/src`
with GHCi.

Generic-deriving (`Path.derived`, `Query.derived`,
`Okapi.HTTP.Headers.derived`, `GPath`/`GQuery`/`GHeaders`, `LitF`,
`ConstF`) is out of scope here — it has its own qualification story.

# Part 1 — The three-layer architecture

| Layer | Modules | What it's for |
|---|---|---|
| **`Okapi.HTTP.*`** | `Tree`, `Structured.*`, `Headers`, `Body`, `HTTP` (incl. `Morph`/`morph`), `Request`(`.Method`/`.Path`/`.Query`/`.Headers`/`.Headers.Cookie`/`.Body`), `Response`(`.Status`/`.Headers`/`.Headers.SetCookie`/`.Headers.Attributes`/`.Body`) | Describing and coding one HTTP message. |
| **`Okapi.Data.*` / `Okapi.Result.*` / `Okapi.Failure.*`** | `Data.Request`/`.Response`, `Result.Request`/`.Response`, `Failure.Request`/`.Response` | The value-level counterparts `parser`/`printer` produce and consume. No combinators of their own. |
| **`Okapi.Artifact.*`**, plus the bare `Okapi` facade | `Client`, `Endpoint`, `Function`, `Link`, `OpenApi` | Consumes an `HTTP` contract, produces something else — a server, a client function, an OpenAPI document, hyperlinks. |

`Okapi.HTTP.Tree` is the foundation the other layers build on: the
generic applicative combinator engine that `field`, `json`, `seg`, and
every other leaf combinator desugars through via `ApplicativeDo`.
`Okapi.HTTP.Structured.*` (RFC 9651) sits alongside it, equally generic.
`Morph` — the newtype lifting a shape-only type (`HTTP`,
`Okapi.Artifact.Link.Link`, `Okapi.Artifact.Client.Client`) into the
2-arg slot a heterogeneous HKD record needs — lives in `Okapi.HTTP` next
to `Shape`, the thing it exists to carry.

`Okapi.Data`/`Result`/`Failure` sit at the bare top level, not nested
under `Okapi.HTTP`: they aren't DSL machinery, and they aren't artifacts
consumed *from* an `HTTP` contract either — they're the value-level twin
of `Request`/`Response` itself. Each is self-titled the same way every
`Okapi.HTTP.*` leaf module is: `Okapi.Data.Request` exports a type named
`Request`, not `Data`.

`Okapi.Artifact.Client`/`.Endpoint`/`.Function`/`.Link`/`.OpenApi` group
together because they share one shape — each takes an `HTTP` contract and
produces something else. The bare `Okapi` facade re-exports all of it
unqualified, sourced internally from `Okapi.Artifact.*` and `Okapi.HTTP`.

# Part 2 — What each qualifier gives you

**`Okapi`** (bare, unqualified — safe, none of it is Request-or-Response-
specific): the `HTTP` contract type (`(:->)`/`(:-<)`), `Morph`/`morph`,
`Cases`/`cases`/`getResponses`/`parseResponses`/`printResponses`, the
whole `Endpoint`/`Handle`/`Client`/`Link` layer, and the generic `Tree`
surface (`SymTree`, `Leaf(..)`, `HasLeaf(..)`, `(=.)`, leaf combinators
`int`/`integer`/`bool`/`text`/`uuid`/etc.). Not here: `Request`/
`Response`, `any`/`get`/`post`/etc., method/status singletons,
`seg`/`param`/`field`/`json`/`cookie`/`setCookie`.

**`Okapi.HTTP` (qualified `HTTP.`)**: the genuinely side-free operations —
`field`/`field'`/`field_`/`contentType`/`fieldStruct`/`fieldBareItem`/
`fieldItem`/`fieldList`/`fieldDict` (headers), `json`/`jsonValue`/`none`
(body), `MediaType(..)`, `None(..)`, `IsoJson`. These resolve unqualified
in the phantom `ForRequest`/`ForResponse` tag `Headers`/`Body` carry — one
name, one binding, reached only here, never duplicated onto `Req.`/`Res.`.

```haskell
import Okapi.HTTP qualified as HTTP

reviewReqHeaders = do
    HTTP.field_ "x-service" "reviews"
    HTTP.contentType HTTP.JSON
```

**`Okapi.HTTP.Request` (qualified `Req.`)**: `Request(..)`, `any` (the
maximally generic start), `get`/`post`/`put`/`delete`/`patch`/`head`/
`options`/`connect`/`trace`, narrowing functions `method`/`path`/`query`/
`headers`/`body`, all 9 method singletons, `seg`/`seg_`/`lit`/`segs`,
`param`/`param'`/`param_`/`flag`/`flag'`/`list`/`list'`/`ArrayStyle(..)`,
and the side-pinned `cookie`/`cookie'`/`form`. `get`/`post`/`.../head`
deliberately shadow `Prelude` when qualified as `Req.` — exactly the case
qualified importing exists for (`any`/`head` collide even unqualified in
their own defining module, so it hides them explicitly, same as
`Method.hs`/`Status.hs` hide `Prelude.print`).

**`Okapi.HTTP.Response` (qualified `Res.`)**: `Response(..)`, `any`, all
47 status-named response codecs (`continue` .. `ok` .. `notFound` ..
`networkAuthenticationRequired`, full list in Part 4), `headers`/`body`
narrowing, all 47 status singletons, the side-pinned `setCookie`, and
`attr`/`attr'`/`secure`/`httpOnly` (4 of 9 `Attributes` combinators — the
rest via `Attr.`, Part 5).

A module building only one side still needs two imports — its facade plus
`Okapi.HTTP` for the shared combinators. That's the real cost of every
name meaning exactly one thing regardless of which qualifier reaches it.

**`Okapi.Data.Request`/`.Response`, `Okapi.Result.Request`/`.Response`,
`Okapi.Failure.Request`/`.Response`**: the decoded value, the
intermediate per-field parse, and the accumulated per-field error, one
leaf module per kind per side, self-titled. Reached with their own
qualified import, distinct from `Req.`/`Res.`.

# Part 3 — The qualification mechanism

`Request` and `Response` share field names (`headers`, `body`);
`Okapi.Data.Request`'s `Request` shares a type *name* with
`Okapi.HTTP.Request`'s `Request`. Neither is a problem, because of one
mechanism, applied consistently: **a qualified-only import never brings
its fields into unqualified scope**, so it can never contribute a
candidate to GHC's record-update field-set resolution.

**Same-module case.** An update touching a field unique to one side
resolves for free:

```haskell
Req.any { path = reviewPath, query = reviewQuery, headers = ..., body = ... }
```

An update touching *only* shared field names has nothing to disambiguate
with, and is a hard "ambiguous record update" error:

```haskell
-- ambiguous: `headers` and `body` both exist on Request and Response
Res.ok { headers = reviewOkHeaders, body = HTTP.json @Review }
```

Two fixes, both plain:

```haskell
-- 1. Narrowing functions instead of record update.
reviewOk = Res.body (HTTP.json @Review) (Res.headers reviewOkHeaders Res.ok)

-- 2. Qualify the field name itself, keep record-update syntax --
--    `Res.headers` inside `{ }` is already a resolved field reference,
--    nothing left to guess.
reviewOk = Res.ok { Res.headers = reviewOkHeaders, Res.body = HTTP.json @Review }
```

**Cross-module case.** The same rule applies when two *different* modules
share a field name — e.g. `Okapi.Artifact.Link`'s
`URI { path :: Text, query :: Text }` versus `Request`'s own `path`. With
both imported qualified-only, a bare-field update isn't even ambiguous,
just not in scope, and GHC's suggestion points straight at the fix:

```haskell
import Okapi.HTTP.Request qualified as Req
import Okapi.Artifact.Link qualified as Link

test = Req.delete { path = deleteReviewPath }
-- error: Not in scope: record field 'path'
--   Suggested fix: Perhaps use one of these:
--     record field of URI 'Link.path' (imported from Okapi.Artifact.Link),
--     record field of Request 'Req.path' (imported from Okapi.HTTP.Request)
```

Importing the *other* module unqualified breaks the convention and hides
the problem differently — the bare field silently resolves to whichever
type's fields are unqualified-visible, and fails as a type mismatch
instead of a scope error:

```haskell
import Okapi.HTTP.Request qualified as Req
import Okapi.Artifact.Link  -- unqualified

test = Req.delete { path = deleteReviewPath }
-- error: Couldn't match expected type 'Req.Request ...' with actual type 'URI'
```

**The practical rule, unconditionally**: qualify the field name in every
record update, `Req.any { Req.path = ... }`, not just when GHC complains.

The same "qualification decides scope" logic recurs elsewhere in the DSL:
`param`/`flag`/`list` mean different things at different levels
(query-string parameters, RFC 9651 item parameters, `Set-Cookie` flags),
and `item`/`list`/`dict` exist both as `Structured`'s wrapping combinators
and as the inner `Item`/`List`/`Dictionary` modules' own primitives — each
needs its own qualifier the moment a function touches more than one.

# Part 4 — Naming conventions

1. **Types and constructors stay fully spelled out.** `Request`,
   `Response`, `Headers`, `Structured`, `Attributes`, `Dictionary`,
   `Parameters`, `KnownMethod`, `KnownStatus`.
2. **Self-titling.** A leaf module's own name is also its exported type's
   name: `Okapi.HTTP.Request.Method` exports `Method`, `Okapi.Data.Request`
   exports `Request`.
3. **Term-level combinators abbreviate a recognized short form when the
   full word is long enough to matter** (`segment`/`segment_`/`segments` →
   `seg`/`seg_`/`segs`, `attribute`/`attribute'` → `attr`/`attr'`,
   `dictionary` → `dict`), and stay as-is otherwise (`field`, `param`,
   `flag`, `list`, `item`, `member`, `status`, `method`, `raw`, `json`,
   `none`, `form`). `field_`/`param_`/`seg_`/`bareItem_` all use the same
   underscore suffix for "assert a fixed, known value."
4. **A name never restates its own qualifier.** `Req.any`/`Res.any`, not
   `req`/`res`; `Req.get`/`Req.post`/`...`, not `reqGET`/`reqPOST`. Names
   colliding with `Prelude` when unqualified (`any`, `head`) are hidden
   explicitly at their defining site, same as `Method.hs`/`Status.hs` hide
   `Prelude.print`.

**Status codes.** All 47 are spelled-out names, reused verbatim from
`http-types`'s own reason-phrase-derived aliases, minus the trailing
number — `Res.` qualification already disambiguates, so `Res.ok` reads
better than `Res.ok200`. Reusing rather than inventing matters for
correctness too: a few codes have shifted reason phrases across RFC
revisions (413/414/416 in particular), and `Status.hs` already sources its
reason phrases from `http-types`, so matching its names keeps everything
consistent with one source of truth.

`continue` `switchingProtocols` `ok` `created` `accepted`
`nonAuthoritative` `noContent` `resetContent` `partialContent`
`multipleChoices` `movedPermanently` `found` `seeOther` `notModified`
`useProxy` `temporaryRedirect` `permanentRedirect` `badRequest`
`unauthorized` `paymentRequired` `forbidden` `notFound`
`methodNotAllowed` `notAcceptable` `proxyAuthenticationRequired`
`requestTimeout` `conflict` `gone` `lengthRequired` `preconditionFailed`
`requestEntityTooLarge` `requestURITooLong` `unsupportedMediaType`
`requestedRangeNotSatisfiable` `expectationFailed` `imATeapot`
`unprocessableEntity` `preconditionRequired` `tooManyRequests`
`requestHeaderFieldsTooLarge` `internalServerError` `notImplemented`
`badGateway` `serviceUnavailable` `gatewayTimeout`
`httpVersionNotSupported` `networkAuthenticationRequired`

Each is reachable in a type signature via the matching `SNNN` alias from
`Okapi.HTTP.Response.Status`: `ok :: Response S200 Types.ResponseHeaders
(IO LBS.ByteString)`.

`noContent` (204) is also the shared body-emptiness combinator's name in
spirit — resolved by naming *that* one `none` instead (`Okapi.HTTP.Body`,
matching its own result type `None`, the same way `json`/`jsonValue` are
named after what they produce). `Res.noContent` for 204 and `HTTP.none`
for an empty body now both mean exactly what they say.

# Part 5 — Reference: the rest

Functions that exist and are real, but don't naturally show up in a
typical contract:

| Name | Module | Purpose |
|---|---|---|
| `raw` | `Path`, `Query`, `Headers`, `Structured.Item`/`.List`/`.Dictionary`/`.Parameters`, `Response.Headers.Attributes`, `Okapi.HTTP.Body` | Pass the whole context through unconstrained. |
| `parser` / `printer` | same modules, plus `Okapi.HTTP.Tree` | The low-level codec functions every combinator is built from. |
| `parseExact` | `Path`, `Query`, `Headers`, `Structured`, `.Item`, `.List`, `.Dictionary` | Require full consumption instead of tolerating leftover. |
| `Method.method`/`.raw`/`.parse`/`.print` | `Okapi.HTTP.Request.Method` | Build/inspect a `Method` codec directly (`method`/`print` shadow `Prelude.print` if unqualified). |
| `Status.status`/`.raw`/`.parse`/`.print` | `Okapi.HTTP.Response.Status` | Build/inspect a `Status` codec directly. |
| `Attr.maxAge`/`.domain`/`.path`/`.flag`/`.flag'` | `Okapi.HTTP.Response.Headers.Attributes` | The 5 `Attributes` combinators beyond the 4 curated onto `Res.`. |
| `Struct.item`/`.list`/`.dict` standalone | `Okapi.HTTP.Structured` | The escape hatch `fieldStruct` sugars over. |
| `BareItem.displayString`/`DisplayString` | `Okapi.HTTP.Structured.BareItem` | RFC 9651 §3.3.8 Display String — percent-encoded, non-ASCII-safe text. |
| `BareItem.byteSequence`/`ByteSequence` | `Okapi.HTTP.Structured.BareItem` | RFC 9651 §3.3.5 — base64, colon-delimited bytes. |
| `BareItem.hasNonCanonicalInteger` | `Okapi.HTTP.Structured.BareItem` | Flags legal-but-non-canonical integer syntax. |
| `knownMethodToStd`/`extractMethod`/`knownStatusToHTTP`/`extractStatus` | `Method`, `Status` | Convert to/extract from `http-types`. |

# Part 6 — Worked example

A three-endpoint "product reviews" API, using as much of the DSL as
naturally fits in one place. Compiles as-is against this library.

```haskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module MaxQual where

import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.ByteString (ByteString)
import Network.HTTP.Types qualified as Types
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

-- The generic mode/framework layer, plus the fully generic Tree engine --
-- HTTP/:->/:-</Cases/cases/getResponses/parseResponses/printResponses,
-- and int/text/uuid/bool/integer/(=.)/Leaf. None of this is
-- Request-or-Response-specific, so it's always safe to leave bare.
import Okapi

-- The genuinely shared HTTP DSL -- field/contentType/json/etc. -- one
-- polymorphic definition each, its own honest qualifier.
import Okapi.HTTP qualified as HTTP

-- Everything genuinely Request-specific.
import Okapi.HTTP.Request qualified as Req

-- Everything genuinely Response-specific.
import Okapi.HTTP.Response qualified as Res

-- Method.method itself (the smart constructor) only lives here.
import Okapi.HTTP.Request.Method qualified as Method

-- Only for the ArrayStyle constructors not re-exported onto Req.
import Okapi.HTTP.Request.Query qualified as Query

-- Set-Cookie attributes beyond the curated 4 reached via Res.
import Okapi.HTTP.Response.Headers.Attributes qualified as Attr

-- Structured Field Values (RFC 9651) -- none of this is
-- Request-or-Response-specific, so none of it needs Req./Res./HTTP.
import Okapi.HTTP.Structured qualified as Struct
import Okapi.HTTP.Structured.BareItem qualified as BareItem
import Okapi.HTTP.Structured.Item qualified as Item
import Okapi.HTTP.Structured.List qualified as SList
import Okapi.HTTP.Structured.Dictionary qualified as Dict
import Okapi.HTTP.Structured.Parameters qualified as Params

-- For the parseResponses/printResponses demo -- neither Wai.Response nor
-- the decoded-value type is reachable via Okapi or Res.
import Network.Wai qualified as Wai
import Okapi.Data.Response qualified as Data

data Review = Review
    { reviewId :: Int
    , stars    :: Int
    , comment  :: Text
    }
    deriving (Generic, Show, Aeson.FromJSON, Aeson.ToJSON, ToSchema)

clientToken :: BareItem.Token
clientToken = fromMaybe (error "bad literal token") (BareItem.mkToken "okapi-v2")

-- Endpoint 1: GET /v2/products/{productId}/reviews/{reviewId}
--
-- `&` chains every narrowing function left to right; `BlockArguments`
-- lets each take its `do` block directly. Projection helpers (`hp1` etc.)
-- are a leading `let`, since `where` can't attach to a bare argument.
getReview = Req.any
    & Req.path do
        Req.seg_ int 2
        Req.lit "products"
        pid <- fst =. Req.seg "productId" uuid
        Req.lit "reviews"
        rid <- snd =. Req.seg "reviewId" int
        pure (pid, rid)
    & Req.query do
        let qp1 (a,_,_,_) = a
            qp2 (_,b,_,_) = b
            qp3 (_,_,c,_) = c
            qp4 (_,_,_,d) = d
        verbose <- qp1 =. Req.flag' "verbose"
        limit <- qp2 =. Req.param' "limit" int
        tagFilter <- qp3 =. Req.list' Query.Exploded "tag" text
        Req.param_ "api" int 2
        fixedFmt <- qp4 =. Req.param "format" text
        pure (verbose, limit, tagFilter, fixedFmt)
    & Req.headers do
        let hp1 (a,_,_,_,_,_) = a
            hp2 (_,b,_,_,_,_) = b
            hp3 (_,_,c,_,_,_) = c
            hp4 (_,_,_,d,_,_) = d
            hp5 (_,_,_,_,e,_) = e
            hp6 (_,_,_,_,_,f) = f
        -- Field-family combinators are all `HTTP.` -- only
        -- `cookie`/`cookie'` (request-only) stay `Req.`.
        HTTP.field_ "x-service" "reviews"
        HTTP.contentType HTTP.JSON
        apiKey <- hp1 =. HTTP.fieldBareItem "x-api-key" (BareItem.token :: Leaf BareItem.BareItem BareItem.Token)
        -- A fixed marker token plus a real parameter riding with it --
        -- `bareItem_` composed with a value-producing sibling.
        buildNum <- hp2 =. HTTP.fieldItem "x-client" do
            Item.bareItem_ (BareItem.unToken clientToken)
            n <- Item.params (Params.param "build" integer)
            pure n
        traceId <- hp3 =. HTTP.field' "x-trace-id" uuid
        prefs <- hp4 =. HTTP.fieldDict "prefer" do
            a <- fst =. Dict.member "compact" (Item.bareItem bool)
            b <- snd =. Dict.member' "notes" (Item.bareItem bool)
            pure (a, b)
        sid <- hp5 =. Req.cookie' "sid" uuid
        lang <- hp6 =. Req.cookie "lang" text
        pure (apiKey, buildNum, traceId, prefs, sid, lang)
    & Req.body HTTP.none

-- Endpoint 1's responses -- three List-shaped headers, showing a flat
-- list of scalars and the general innerListOf heterogeneous-composition
-- shape (RFC 9651 §3.1.1).
cacheTags = SList.items (Item.bareItem text)

relatedIds = do
    a <- fst =. SList.item (Item.bareItem integer)
    b <- snd =. SList.item (Item.bareItem integer)
    pure (a, b)

batchGroups = do
    a <- fst =. SList.innerListOf
            (SList.innerItem (Item.bareItem text))
            (Params.param "lvl" integer)
    b <- snd =. SList.innerListOf
            (SList.innerItems (Item.bareItem integer))
            (Params.param' "lvl" integer)
    pure (a, b)

-- Same `&`/`BlockArguments` treatment on the response side, reusing the
-- exact same `HTTP.` functions -- only `setCookie` stays `Res.`.
reviewOk = Res.ok
    & Res.headers do
        let rp1 _ = ()
            rp2 (a,_,_,_,_,_) = a
            rp3 (_,b,_,_,_,_) = b
            rp4 (_,_,c,_,_,_) = c
            rp5 (_,_,_,d,_,_) = d
            rp6 (_,_,_,_,e,_) = e
            rp7 (_,_,_,_,_,f) = f
        rp1 =. HTTP.contentType HTTP.JSON
        limit <- rp2 =. HTTP.field "x-ratelimit-limit" int
        rateItem <- rp3 =. HTTP.fieldItem "x-ratelimit" do
            n <- fst =. Item.bareItem integer
            params <- snd =. Item.params do
                w <- fst =. Params.param "window" integer
                b <- snd =. Params.flag' "burst"
                pure (w, b)
            pure (n, params)
        tags <- rp4 =. HTTP.fieldList "cache-tags" cacheTags
        related <- rp5 =. HTTP.fieldList "x-related-ids" relatedIds
        groups <- rp6 =. HTTP.fieldList "x-batch-groups" batchGroups
        -- Set-Cookie attributes inlined too.
        session <- rp7 =. Res.setCookie "session" uuid do
            let ap1 (a,_,_,_) = a
                ap2 (_,b,_,_) = b
                ap3 (_,_,c,_) = c
                ap4 _ = ()
                ap5 _ = ()
                ap6 (_,_,_,d) = d
                ap7 _ = ()
            age <- ap1 =. Attr.maxAge
            dom <- ap2 =. Attr.domain
            pth <- ap3 =. Attr.path
            ap4 =. Res.secure
            ap5 =. Res.httpOnly
            sameSite <- ap6 =. Res.attr "SameSite" (leaf :: Leaf Attr.Attributes ByteString)
            ap7 =. Attr.flag "Partitioned"
            pure (age, dom, pth, sameSite)
        pure (limit, rateItem, tags, related, groups, session)
    & Res.body (HTTP.json @Review)

getReviewContract = getReview :-> reviewOk

-- Endpoint 2: GET /products/{productId}/reviews/tags/{tag}+ -- `segs`,
-- and `Req.method Method.GET` used directly instead of a pre-built
-- starting point (`method` takes the bare singleton, not `Method.method`,
-- which builds a full codec).
listReviews = Req.any
    & Req.method Method.GET
    & Req.path do
        Req.lit "products"
        pid <- fst =. Req.seg "productId" uuid
        Req.lit "reviews"
        Req.lit "tags"
        tags <- snd =. Req.segs text
        pure (pid, tags)
    & Req.body HTTP.none

listOk = Res.ok & Res.body (HTTP.json @[Review])

listReviewsContract = listReviews :-> listOk

-- Endpoint 3: a `Cases` contract with two response alternatives.
--
-- Two equally valid fixes for the `path`/`URI` collision (Part 3):
-- qualify the field name in record-update syntax, or `&`-pipe into the
-- narrowing function directly, shown here.
deleteReviewReq = Req.delete
    & Req.path do
        Req.lit "products"
        pid <- fst =. Req.seg "productId" uuid
        Req.lit "reviews"
        rid <- snd =. Req.seg "reviewId" int
        pure (pid, rid)

deletedRes = Res.noContent
notFoundRes = Res.notFound & Res.body (HTTP.json @Text)

data DeleteReviewResponses f
    = Deleted (f Res.S204 Types.ResponseHeaders (IO HTTP.None))
    | ReviewNotFound (f Res.S404 Types.ResponseHeaders (IO Text))
    deriving (Generic, Cases)

deleteReviewCases = cases @DeleteReviewResponses deletedRes notFoundRes

deleteReviewContract = deleteReviewReq :-< deleteReviewCases

-- | How many branches this `Cases` value actually has right now.
deleteReviewBranchCount :: Int
deleteReviewBranchCount = length (getResponses deleteReviewCases)

-- | Render the `Deleted` branch to a real 'Wai.Response', and parse one
--   back into whichever branch it matches. `Data` (`Okapi.Data.Response`)
--   and `Wai.Response` both need their own import.
demoPrintDeleted :: IO Wai.Response
demoPrintDeleted = printResponses deleteReviewCases (Deleted (Data.Response Res.S204 [] (pure HTTP.None)))

demoParseIncoming waiResponse = parseResponses deleteReviewCases waiResponse
```

- **Endpoint 1** is the dense one: a path mixing a literal-integer version
  segment, literal segments, and typed segments; a query using every
  combinator; headers spanning a fixed-value assertion, a `BareItem`
  token, a composed `Item`, a `Dictionary`, and both request cookies.
  The response reuses the same `HTTP.` names, builds a parameterized
  `Item`, two `List`-shaped headers (flat and the full
  `innerListOf`/`innerItem`/`innerItems` composition), and a `Set-Cookie`
  combining `Attr.maxAge`/`.domain`/`.path`, `Res.secure`/`.httpOnly`,
  `Res.attr`, and `Attr.flag`.
- **Endpoint 2** exists mainly for `segs` (a trailing catch-all) and
  `Req.method Method.GET` used directly.
- **Endpoint 3** is the `Cases` demo: two response alternatives, the
  `path`/`URI` collision fix from Part 3, `getResponses`, and both
  directions of `printResponses`/`parseResponses` against a real
  `Wai.Response`.

A module that only builds one side still pays a two-import minimum (its
facade plus `HTTP`) — no configuration gets back to a single unqualified
import for the HTTP DSL, and that's the deliberate tradeoff this whole
design makes (Part 2).
