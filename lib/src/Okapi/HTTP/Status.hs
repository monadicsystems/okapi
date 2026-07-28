
module Okapi.HTTP.Status (
    KnownStatus (..),
    S100, S101, S200, S201, S202, S203, S204, S205, S206,
    S300, S301, S302, S303, S304, S305, S307, S308,
    S400, S401, S402, S403, S404, S405, S406, S407, S408, S409,
    S410, S411, S412, S413, S414, S415, S416, S417, S418,
    S422, S428, S429, S431,
    S500, S501, S502, S503, S504, S505, S511,
    SomeKnownStatus (..),
    Status (..),
    Base,
    ParseError (..),
    parse,
    print,
    base,
    status,
    knownStatusToHTTP,
    extractStatus,
    allKnownStatuses,
) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Nat, KnownNat, natVal)
import Network.HTTP.Types qualified as Types
import Prelude hiding (print)

-- $setup
-- >>> import Okapi.HTTP.Status qualified as Status
-- >>> import Network.HTTP.Types qualified as Types
-- >>> import Okapi.Tree (leafPrintParse, leafParsePrint)
-- >>> import Test.QuickCheck.Instances ()

{- | Evidence that @s@ is a known, valid HTTP status code (every @statusNNN@
  @http-types@ ships — see 'statusTable', not the full IANA registry). One
  named constructor per recognized code, mirroring
  "Okapi.HTTP.Method"'s @KnownMethod@ exactly — carries no runtime
  data of its own beyond which constructor was used. Also constructible via
  the numeral literal itself: @200 :: KnownStatus 200@, through the 'Num'
  instance below — @status 200@ reads the same way a plain status-code
  literal would anywhere else; both routes produce the same value.
-}
data KnownStatus (s :: Nat) where
    S100 :: KnownStatus 100
    S101 :: KnownStatus 101
    S200 :: KnownStatus 200
    S201 :: KnownStatus 201
    S202 :: KnownStatus 202
    S203 :: KnownStatus 203
    S204 :: KnownStatus 204
    S205 :: KnownStatus 205
    S206 :: KnownStatus 206
    S300 :: KnownStatus 300
    S301 :: KnownStatus 301
    S302 :: KnownStatus 302
    S303 :: KnownStatus 303
    S304 :: KnownStatus 304
    S305 :: KnownStatus 305
    S307 :: KnownStatus 307
    S308 :: KnownStatus 308
    S400 :: KnownStatus 400
    S401 :: KnownStatus 401
    S402 :: KnownStatus 402
    S403 :: KnownStatus 403
    S404 :: KnownStatus 404
    S405 :: KnownStatus 405
    S406 :: KnownStatus 406
    S407 :: KnownStatus 407
    S408 :: KnownStatus 408
    S409 :: KnownStatus 409
    S410 :: KnownStatus 410
    S411 :: KnownStatus 411
    S412 :: KnownStatus 412
    S413 :: KnownStatus 413
    S414 :: KnownStatus 414
    S415 :: KnownStatus 415
    S416 :: KnownStatus 416
    S417 :: KnownStatus 417
    S418 :: KnownStatus 418
    S422 :: KnownStatus 422
    S428 :: KnownStatus 428
    S429 :: KnownStatus 429
    S431 :: KnownStatus 431
    S500 :: KnownStatus 500
    S501 :: KnownStatus 501
    S502 :: KnownStatus 502
    S503 :: KnownStatus 503
    S504 :: KnownStatus 504
    S505 :: KnownStatus 505
    S511 :: KnownStatus 511

-- | Every value of @KnownStatus s@ (for a fixed @s@) is the same value —
--   there's only one, so this is trivially always 'True'.
instance Eq (KnownStatus s) where
    _ == _ = True

-- | Shows as the bare code number, e.g. @200@, not a constructor name —
--   matches how it's constructed (a numeral literal).
instance KnownNat s => Show (KnownStatus s) where
    show _ = show (natVal (Proxy @s))

-- | @+@\/@*@\/etc. are all no-ops here — there's only one @KnownStatus s@
--   value for a given @s@, so any operation on it just returns that same
--   value. 'fromInteger' ignores its argument for the same reason: what
--   actually determines the status code is the type @s@ (fixed by
--   whatever numeral literal and type annotation/inference the caller
--   wrote, e.g. @200 :: KnownStatus 200@), not the runtime 'Integer'.
--
--   One instance per recognized code, deliberately — not a single
--   @KnownNat s => Num (KnownStatus s)@ instance for every @s@. That
--   generic form would let a nonsense code like @999@ compile fine (any
--   @KnownNat@ satisfies it) and only fail later, at runtime, in
--   'knownStatusToHTTP'. Scoping 'Num' to exactly these 47 means
--   @status 999@ is a compile error — no @Num (KnownStatus 999)@ instance
--   exists — the same guarantee the old one-GADT-constructor-per-code
--   design had, without the 47 named constructors.
instance Num (KnownStatus 100) where
    fromInteger _ = S100
    _ + _ = S100
    _ * _ = S100
    abs = id
    signum _ = S100
    negate = id

instance Num (KnownStatus 101) where
    fromInteger _ = S101
    _ + _ = S101
    _ * _ = S101
    abs = id
    signum _ = S101
    negate = id

instance Num (KnownStatus 200) where
    fromInteger _ = S200
    _ + _ = S200
    _ * _ = S200
    abs = id
    signum _ = S200
    negate = id

instance Num (KnownStatus 201) where
    fromInteger _ = S201
    _ + _ = S201
    _ * _ = S201
    abs = id
    signum _ = S201
    negate = id

instance Num (KnownStatus 202) where
    fromInteger _ = S202
    _ + _ = S202
    _ * _ = S202
    abs = id
    signum _ = S202
    negate = id

instance Num (KnownStatus 203) where
    fromInteger _ = S203
    _ + _ = S203
    _ * _ = S203
    abs = id
    signum _ = S203
    negate = id

instance Num (KnownStatus 204) where
    fromInteger _ = S204
    _ + _ = S204
    _ * _ = S204
    abs = id
    signum _ = S204
    negate = id

instance Num (KnownStatus 205) where
    fromInteger _ = S205
    _ + _ = S205
    _ * _ = S205
    abs = id
    signum _ = S205
    negate = id

instance Num (KnownStatus 206) where
    fromInteger _ = S206
    _ + _ = S206
    _ * _ = S206
    abs = id
    signum _ = S206
    negate = id

instance Num (KnownStatus 300) where
    fromInteger _ = S300
    _ + _ = S300
    _ * _ = S300
    abs = id
    signum _ = S300
    negate = id

instance Num (KnownStatus 301) where
    fromInteger _ = S301
    _ + _ = S301
    _ * _ = S301
    abs = id
    signum _ = S301
    negate = id

instance Num (KnownStatus 302) where
    fromInteger _ = S302
    _ + _ = S302
    _ * _ = S302
    abs = id
    signum _ = S302
    negate = id

instance Num (KnownStatus 303) where
    fromInteger _ = S303
    _ + _ = S303
    _ * _ = S303
    abs = id
    signum _ = S303
    negate = id

instance Num (KnownStatus 304) where
    fromInteger _ = S304
    _ + _ = S304
    _ * _ = S304
    abs = id
    signum _ = S304
    negate = id

instance Num (KnownStatus 305) where
    fromInteger _ = S305
    _ + _ = S305
    _ * _ = S305
    abs = id
    signum _ = S305
    negate = id

instance Num (KnownStatus 307) where
    fromInteger _ = S307
    _ + _ = S307
    _ * _ = S307
    abs = id
    signum _ = S307
    negate = id

instance Num (KnownStatus 308) where
    fromInteger _ = S308
    _ + _ = S308
    _ * _ = S308
    abs = id
    signum _ = S308
    negate = id

instance Num (KnownStatus 400) where
    fromInteger _ = S400
    _ + _ = S400
    _ * _ = S400
    abs = id
    signum _ = S400
    negate = id

instance Num (KnownStatus 401) where
    fromInteger _ = S401
    _ + _ = S401
    _ * _ = S401
    abs = id
    signum _ = S401
    negate = id

instance Num (KnownStatus 402) where
    fromInteger _ = S402
    _ + _ = S402
    _ * _ = S402
    abs = id
    signum _ = S402
    negate = id

instance Num (KnownStatus 403) where
    fromInteger _ = S403
    _ + _ = S403
    _ * _ = S403
    abs = id
    signum _ = S403
    negate = id

instance Num (KnownStatus 404) where
    fromInteger _ = S404
    _ + _ = S404
    _ * _ = S404
    abs = id
    signum _ = S404
    negate = id

instance Num (KnownStatus 405) where
    fromInteger _ = S405
    _ + _ = S405
    _ * _ = S405
    abs = id
    signum _ = S405
    negate = id

instance Num (KnownStatus 406) where
    fromInteger _ = S406
    _ + _ = S406
    _ * _ = S406
    abs = id
    signum _ = S406
    negate = id

instance Num (KnownStatus 407) where
    fromInteger _ = S407
    _ + _ = S407
    _ * _ = S407
    abs = id
    signum _ = S407
    negate = id

instance Num (KnownStatus 408) where
    fromInteger _ = S408
    _ + _ = S408
    _ * _ = S408
    abs = id
    signum _ = S408
    negate = id

instance Num (KnownStatus 409) where
    fromInteger _ = S409
    _ + _ = S409
    _ * _ = S409
    abs = id
    signum _ = S409
    negate = id

instance Num (KnownStatus 410) where
    fromInteger _ = S410
    _ + _ = S410
    _ * _ = S410
    abs = id
    signum _ = S410
    negate = id

instance Num (KnownStatus 411) where
    fromInteger _ = S411
    _ + _ = S411
    _ * _ = S411
    abs = id
    signum _ = S411
    negate = id

instance Num (KnownStatus 412) where
    fromInteger _ = S412
    _ + _ = S412
    _ * _ = S412
    abs = id
    signum _ = S412
    negate = id

instance Num (KnownStatus 413) where
    fromInteger _ = S413
    _ + _ = S413
    _ * _ = S413
    abs = id
    signum _ = S413
    negate = id

instance Num (KnownStatus 414) where
    fromInteger _ = S414
    _ + _ = S414
    _ * _ = S414
    abs = id
    signum _ = S414
    negate = id

instance Num (KnownStatus 415) where
    fromInteger _ = S415
    _ + _ = S415
    _ * _ = S415
    abs = id
    signum _ = S415
    negate = id

instance Num (KnownStatus 416) where
    fromInteger _ = S416
    _ + _ = S416
    _ * _ = S416
    abs = id
    signum _ = S416
    negate = id

instance Num (KnownStatus 417) where
    fromInteger _ = S417
    _ + _ = S417
    _ * _ = S417
    abs = id
    signum _ = S417
    negate = id

instance Num (KnownStatus 418) where
    fromInteger _ = S418
    _ + _ = S418
    _ * _ = S418
    abs = id
    signum _ = S418
    negate = id

instance Num (KnownStatus 422) where
    fromInteger _ = S422
    _ + _ = S422
    _ * _ = S422
    abs = id
    signum _ = S422
    negate = id

instance Num (KnownStatus 428) where
    fromInteger _ = S428
    _ + _ = S428
    _ * _ = S428
    abs = id
    signum _ = S428
    negate = id

instance Num (KnownStatus 429) where
    fromInteger _ = S429
    _ + _ = S429
    _ * _ = S429
    abs = id
    signum _ = S429
    negate = id

instance Num (KnownStatus 431) where
    fromInteger _ = S431
    _ + _ = S431
    _ * _ = S431
    abs = id
    signum _ = S431
    negate = id

instance Num (KnownStatus 500) where
    fromInteger _ = S500
    _ + _ = S500
    _ * _ = S500
    abs = id
    signum _ = S500
    negate = id

instance Num (KnownStatus 501) where
    fromInteger _ = S501
    _ + _ = S501
    _ * _ = S501
    abs = id
    signum _ = S501
    negate = id

instance Num (KnownStatus 502) where
    fromInteger _ = S502
    _ + _ = S502
    _ * _ = S502
    abs = id
    signum _ = S502
    negate = id

instance Num (KnownStatus 503) where
    fromInteger _ = S503
    _ + _ = S503
    _ * _ = S503
    abs = id
    signum _ = S503
    negate = id

instance Num (KnownStatus 504) where
    fromInteger _ = S504
    _ + _ = S504
    _ * _ = S504
    abs = id
    signum _ = S504
    negate = id

instance Num (KnownStatus 505) where
    fromInteger _ = S505
    _ + _ = S505
    _ * _ = S505
    abs = id
    signum _ = S505
    negate = id

instance Num (KnownStatus 511) where
    fromInteger _ = S511
    _ + _ = S511
    _ * _ = S511
    abs = id
    signum _ = S511
    negate = id

-- | Short type-level names for each recognized status code, mirroring
--   "Okapi.HTTP.Method"'s @GET@\/@POST@\/etc. type synonyms.
type S100 = KnownStatus 100
type S101 = KnownStatus 101
type S200 = KnownStatus 200
type S201 = KnownStatus 201
type S202 = KnownStatus 202
type S203 = KnownStatus 203
type S204 = KnownStatus 204
type S205 = KnownStatus 205
type S206 = KnownStatus 206
type S300 = KnownStatus 300
type S301 = KnownStatus 301
type S302 = KnownStatus 302
type S303 = KnownStatus 303
type S304 = KnownStatus 304
type S305 = KnownStatus 305
type S307 = KnownStatus 307
type S308 = KnownStatus 308
type S400 = KnownStatus 400
type S401 = KnownStatus 401
type S402 = KnownStatus 402
type S403 = KnownStatus 403
type S404 = KnownStatus 404
type S405 = KnownStatus 405
type S406 = KnownStatus 406
type S407 = KnownStatus 407
type S408 = KnownStatus 408
type S409 = KnownStatus 409
type S410 = KnownStatus 410
type S411 = KnownStatus 411
type S412 = KnownStatus 412
type S413 = KnownStatus 413
type S414 = KnownStatus 414
type S415 = KnownStatus 415
type S416 = KnownStatus 416
type S417 = KnownStatus 417
type S418 = KnownStatus 418
type S422 = KnownStatus 422
type S428 = KnownStatus 428
type S429 = KnownStatus 429
type S431 = KnownStatus 431
type S500 = KnownStatus 500
type S501 = KnownStatus 501
type S502 = KnownStatus 502
type S503 = KnownStatus 503
type S504 = KnownStatus 504
type S505 = KnownStatus 505
type S511 = KnownStatus 511

type Status :: Type -> Type
data Status a where
    Base    :: Status Base
    Status :: KnownNat s => KnownStatus s -> Status (KnownStatus s)

-- | What 'base' decodes\/encodes to — the maximally unconstrained status slot.
type Base = Types.Status

data ParseError = ParseError deriving (Eq, Show)

parse :: Status status -> Types.Status -> Either ParseError status
parse Base         s = Right s
parse (Status ks) s
    | s == knownStatusToHTTP ks = Right ks
    | otherwise                 = Left ParseError

print :: Status status -> status -> Types.Status
print Base         s  = s
print (Status ks) _  = knownStatusToHTTP ks

-- | Pass the raw HTTP status straight through, unconstrained.
--
-- >>> parse base (Types.mkStatus 200 "OK")
-- Right (Status {statusCode = 200, statusMessage = "OK"})
-- >>> Status.print base (Types.mkStatus 200 "OK")
-- Status {statusCode = 200, statusMessage = "OK"}
--
-- prop> \code msg -> leafPrintParse (parse base) (Status.print base) (Types.mkStatus code msg)
-- prop> \code msg -> leafParsePrint (parse base) (Status.print base) (Types.mkStatus code msg)
base :: Status Types.Status
base = Base

-- | Match against a statically known HTTP status code. Standalone uses
--   (like these examples) need the numeral literal annotated with its own
--   type, since nothing else pins it — but wherever @status 200@ appears
--   inside a signature that already says @KnownStatus 200@ (e.g.
--   'Okapi.HTTP.Response.ok'), it just works, no annotation
--   needed, because the surrounding type does that job instead.
--
-- >>> parse (status (200 :: KnownStatus 200)) Types.status200
-- Right 200
-- >>> parse (status (200 :: KnownStatus 200)) Types.status404
-- Left ParseError
-- >>> Status.print (status (200 :: KnownStatus 200)) 200
-- Status {statusCode = 200, statusMessage = "OK"}
status :: KnownNat s => KnownStatus s -> Status (KnownStatus s)
status = Status

-- | Every status code recognized here, paired with @http-types@'s own
--   pre-built 'Types.Status' value (so the reason phrase, e.g. @"OK"@,
--   @"Not Found"@, always matches @http-types@ exactly).
statusTable :: [(Integer, Types.Status)]
statusTable =
    [ (100, Types.status100)
    , (101, Types.status101)
    , (200, Types.status200)
    , (201, Types.status201)
    , (202, Types.status202)
    , (203, Types.status203)
    , (204, Types.status204)
    , (205, Types.status205)
    , (206, Types.status206)
    , (300, Types.status300)
    , (301, Types.status301)
    , (302, Types.status302)
    , (303, Types.status303)
    , (304, Types.status304)
    , (305, Types.status305)
    , (307, Types.status307)
    , (308, Types.status308)
    , (400, Types.status400)
    , (401, Types.status401)
    , (402, Types.status402)
    , (403, Types.status403)
    , (404, Types.status404)
    , (405, Types.status405)
    , (406, Types.status406)
    , (407, Types.status407)
    , (408, Types.status408)
    , (409, Types.status409)
    , (410, Types.status410)
    , (411, Types.status411)
    , (412, Types.status412)
    , (413, Types.status413)
    , (414, Types.status414)
    , (415, Types.status415)
    , (416, Types.status416)
    , (417, Types.status417)
    , (418, Types.status418)
    , (422, Types.status422)
    , (428, Types.status428)
    , (429, Types.status429)
    , (431, Types.status431)
    , (500, Types.status500)
    , (501, Types.status501)
    , (502, Types.status502)
    , (503, Types.status503)
    , (504, Types.status504)
    , (505, Types.status505)
    , (511, Types.status511)
    ]

-- | 'error's on a code outside 'statusTable' — every 'KnownStatus' actually
--   constructed through ordinary use (a numeral literal matching a real
--   status code) is always in range; this only matters for a
--   deliberately-contrived mismatched literal\/type-annotation pair.
knownStatusToHTTP :: forall s. KnownNat s => KnownStatus s -> Types.Status
knownStatusToHTTP _ = case lookup code statusTable of
    Just s  -> s
    Nothing -> error ("KnownStatus: " <> show code <> " is not a recognized status code")
  where
    code = natVal (Proxy @s)

extractStatus :: KnownNat s => Status (KnownStatus s) -> Types.Status
extractStatus (Status ks) = knownStatusToHTTP ks

-- | A 'KnownStatus' with its code hidden — lets every known status be
--   listed in one plain value ('allKnownStatuses') rather than needing one
--   doctest example per code.
data SomeKnownStatus where
    SomeKnownStatus :: KnownNat s => KnownStatus s -> SomeKnownStatus

-- | Every 'KnownStatus' this module recognizes, for exhaustive testing —
--   each one really does round-trip through 'parse'\/'Status.print', not
--   just the handful with worked examples above:
--
-- >>> all (\(SomeKnownStatus ks) -> parse (status ks) (knownStatusToHTTP ks) == Right ks) allKnownStatuses
-- True
-- >>> length allKnownStatuses
-- 47
allKnownStatuses :: [SomeKnownStatus]
allKnownStatuses =
    [ SomeKnownStatus (100 :: KnownStatus 100), SomeKnownStatus (101 :: KnownStatus 101)
    , SomeKnownStatus (200 :: KnownStatus 200), SomeKnownStatus (201 :: KnownStatus 201)
    , SomeKnownStatus (202 :: KnownStatus 202), SomeKnownStatus (203 :: KnownStatus 203)
    , SomeKnownStatus (204 :: KnownStatus 204), SomeKnownStatus (205 :: KnownStatus 205)
    , SomeKnownStatus (206 :: KnownStatus 206), SomeKnownStatus (300 :: KnownStatus 300)
    , SomeKnownStatus (301 :: KnownStatus 301), SomeKnownStatus (302 :: KnownStatus 302)
    , SomeKnownStatus (303 :: KnownStatus 303), SomeKnownStatus (304 :: KnownStatus 304)
    , SomeKnownStatus (305 :: KnownStatus 305), SomeKnownStatus (307 :: KnownStatus 307)
    , SomeKnownStatus (308 :: KnownStatus 308), SomeKnownStatus (400 :: KnownStatus 400)
    , SomeKnownStatus (401 :: KnownStatus 401), SomeKnownStatus (402 :: KnownStatus 402)
    , SomeKnownStatus (403 :: KnownStatus 403), SomeKnownStatus (404 :: KnownStatus 404)
    , SomeKnownStatus (405 :: KnownStatus 405), SomeKnownStatus (406 :: KnownStatus 406)
    , SomeKnownStatus (407 :: KnownStatus 407), SomeKnownStatus (408 :: KnownStatus 408)
    , SomeKnownStatus (409 :: KnownStatus 409), SomeKnownStatus (410 :: KnownStatus 410)
    , SomeKnownStatus (411 :: KnownStatus 411), SomeKnownStatus (412 :: KnownStatus 412)
    , SomeKnownStatus (413 :: KnownStatus 413), SomeKnownStatus (414 :: KnownStatus 414)
    , SomeKnownStatus (415 :: KnownStatus 415), SomeKnownStatus (416 :: KnownStatus 416)
    , SomeKnownStatus (417 :: KnownStatus 417), SomeKnownStatus (418 :: KnownStatus 418)
    , SomeKnownStatus (422 :: KnownStatus 422), SomeKnownStatus (428 :: KnownStatus 428)
    , SomeKnownStatus (429 :: KnownStatus 429), SomeKnownStatus (431 :: KnownStatus 431)
    , SomeKnownStatus (500 :: KnownStatus 500), SomeKnownStatus (501 :: KnownStatus 501)
    , SomeKnownStatus (502 :: KnownStatus 502), SomeKnownStatus (503 :: KnownStatus 503)
    , SomeKnownStatus (504 :: KnownStatus 504), SomeKnownStatus (505 :: KnownStatus 505)
    , SomeKnownStatus (511 :: KnownStatus 511)
    ]
