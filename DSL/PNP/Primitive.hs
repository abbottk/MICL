module Primitive
       ( OpN(..)
       , Give(..), opGive
       , Offer(..), opOffer
       , Take(..), opTake
       , Request(..), opRequest
       ) where


--
-- * Primitive operators
--

-- | Primitive negotiation operators by type.
data OpN
  = Give Give
  | Offer Offer
  | Take Take
  | Request Request
  deriving(Show)

-- | Unary give operators.
data Give = NilG
          deriving(Show)

-- | Unary offer operators.
data Offer = AcceptO | DeclineO | NilO
           deriving(Show)

-- | Unary take operators.
data Take = NilT
          deriving(Show)

-- | Unary request operators.
data Request = AcceptR | DeclineR | NilR
             deriving(Show)

-- | Lookup unary give operator.
opGive :: Token t => Give -> t
opGive NilG = accept

-- | Lookup unary offer operator.
opOffer :: Token t => Offer -> t
opOffer AcceptO  = accept
opOffer DeclineO = decline
opOffer NilO     = decline

-- | Lookup unary take operator.
opTake :: Token t => Take -> t
opTake NilT = accept

-- | Lookup unary request operator.
opRequest :: Token t => Request -> t
opRequest AcceptR  = accept
opRequest DeclineR = decline
opRequest NilR     = decline

class Token t where
  accept :: t
  decline :: t
