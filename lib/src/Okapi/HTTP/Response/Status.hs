
module Okapi.HTTP.Response.Status (
    KnownStatus,
    SomeKnownStatus (..),
    Status (..),
    ParseError (..),
    parse,
    print,
    raw,
    status,
    knownStatusToHTTP,
    extractStatus,
    allKnownStatuses,
) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Nat, KnownNat, natVal)
import Network.HTTP.Types qualified as HTTP
import Okapi.Tree (Failure, Context)
import Prelude hiding (print)

-- $setup
-- >>> import Okapi.HTTP.Response.Status qualified as Status
-- >>> import Network.HTTP.Types qualified as HTTP
-- >>> import Okapi.Tree (leafPrintParse, leafParsePrint)
-- >>> import Test.QuickCheck.Instances ()

-- | Evidence that @s@ is a known, valid HTTP status code (every @statusNNN@
--   @http-types@ ships — see 'statusTable', not the full IANA registry) —
--   carries no runtime data of its own, since the code lives entirely in
--   the type. Constructed via the numeral literal itself: @200 ::
--   KnownStatus 200@, through the 'Num' instance below — @status 200@ reads
--   the same way a plain status-code literal would anywhere else.
data KnownStatus (s :: Nat) = KnownStatus

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
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 101) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 200) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 201) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 202) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 203) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 204) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 205) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 206) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 300) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 301) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 302) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 303) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 304) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 305) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 307) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 308) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 400) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 401) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 402) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 403) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 404) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 405) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 406) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 407) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 408) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 409) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 410) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 411) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 412) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 413) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 414) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 415) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 416) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 417) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 418) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 422) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 428) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 429) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 431) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 500) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 501) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 502) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 503) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 504) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 505) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

instance Num (KnownStatus 511) where
    fromInteger _ = KnownStatus
    _ + _ = KnownStatus
    _ * _ = KnownStatus
    abs = id
    signum _ = KnownStatus
    negate = id

type Status :: Type -> Type
data Status a where
    Raw    :: Status HTTP.Status
    Status :: KnownNat s => KnownStatus s -> Status (KnownStatus s)

data ParseError = ParseError deriving (Eq, Show)

type instance Context Status = HTTP.Status
type instance Failure Status = ParseError

parse :: Status status -> HTTP.Status -> Either ParseError status
parse Raw         s = Right s
parse (Status ks) s
    | s == knownStatusToHTTP ks = Right ks
    | otherwise                 = Left ParseError

print :: Status status -> status -> HTTP.Status
print Raw         s  = s
print (Status ks) _  = knownStatusToHTTP ks

-- | Pass the raw HTTP status straight through, unconstrained.
--
-- >>> parse raw (HTTP.mkStatus 200 "OK")
-- Right (Status {statusCode = 200, statusMessage = "OK"})
-- >>> Status.print raw (HTTP.mkStatus 200 "OK")
-- Status {statusCode = 200, statusMessage = "OK"}
--
-- prop> \code msg -> leafPrintParse (parse raw) (Status.print raw) (HTTP.mkStatus code msg)
-- prop> \code msg -> leafParsePrint (parse raw) (Status.print raw) (HTTP.mkStatus code msg)
raw :: Status HTTP.Status
raw = Raw

-- | Match against a statically known HTTP status code. Standalone uses
--   (like these examples) need the numeral literal annotated with its own
--   type, since nothing else pins it — but wherever @status 200@ appears
--   inside a signature that already says @KnownStatus 200@ (e.g.
--   'Okapi.HTTP.Response.response200'), it just works, no annotation
--   needed, because the surrounding type does that job instead.
--
-- >>> parse (status (200 :: KnownStatus 200)) HTTP.status200
-- Right 200
-- >>> parse (status (200 :: KnownStatus 200)) HTTP.status404
-- Left ParseError
-- >>> Status.print (status (200 :: KnownStatus 200)) 200
-- Status {statusCode = 200, statusMessage = "OK"}
status :: KnownNat s => KnownStatus s -> Status (KnownStatus s)
status = Status

-- | Every status code recognized here, paired with @http-types@'s own
--   pre-built 'HTTP.Status' value (so the reason phrase, e.g. @"OK"@,
--   @"Not Found"@, always matches @http-types@ exactly).
statusTable :: [(Integer, HTTP.Status)]
statusTable =
    [ (100, HTTP.status100)
    , (101, HTTP.status101)
    , (200, HTTP.status200)
    , (201, HTTP.status201)
    , (202, HTTP.status202)
    , (203, HTTP.status203)
    , (204, HTTP.status204)
    , (205, HTTP.status205)
    , (206, HTTP.status206)
    , (300, HTTP.status300)
    , (301, HTTP.status301)
    , (302, HTTP.status302)
    , (303, HTTP.status303)
    , (304, HTTP.status304)
    , (305, HTTP.status305)
    , (307, HTTP.status307)
    , (308, HTTP.status308)
    , (400, HTTP.status400)
    , (401, HTTP.status401)
    , (402, HTTP.status402)
    , (403, HTTP.status403)
    , (404, HTTP.status404)
    , (405, HTTP.status405)
    , (406, HTTP.status406)
    , (407, HTTP.status407)
    , (408, HTTP.status408)
    , (409, HTTP.status409)
    , (410, HTTP.status410)
    , (411, HTTP.status411)
    , (412, HTTP.status412)
    , (413, HTTP.status413)
    , (414, HTTP.status414)
    , (415, HTTP.status415)
    , (416, HTTP.status416)
    , (417, HTTP.status417)
    , (418, HTTP.status418)
    , (422, HTTP.status422)
    , (428, HTTP.status428)
    , (429, HTTP.status429)
    , (431, HTTP.status431)
    , (500, HTTP.status500)
    , (501, HTTP.status501)
    , (502, HTTP.status502)
    , (503, HTTP.status503)
    , (504, HTTP.status504)
    , (505, HTTP.status505)
    , (511, HTTP.status511)
    ]

-- | 'error's on a code outside 'statusTable' — every 'KnownStatus' actually
--   constructed through ordinary use (a numeral literal matching a real
--   status code) is always in range; this only matters for a
--   deliberately-contrived mismatched literal\/type-annotation pair.
knownStatusToHTTP :: forall s. KnownNat s => KnownStatus s -> HTTP.Status
knownStatusToHTTP _ = case lookup code statusTable of
    Just s  -> s
    Nothing -> error ("KnownStatus: " <> show code <> " is not a recognized status code")
  where
    code = natVal (Proxy @s)

extractStatus :: KnownNat s => Status (KnownStatus s) -> HTTP.Status
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
