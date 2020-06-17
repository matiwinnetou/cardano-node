{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Protocol.Types
  (
    -- * The enumeration of supported protocols
    Protocol(..)

    -- * Node support
    -- | Support for the context needed to run a node with a protocol
  , SomeConsensusProtocol(..)
  , TraceConstraints
  , ProtocolInstantiationError(..)
  , renderProtocolInstantiationError

    -- * Client support
    -- | Support for the context needed to run a client of a node that is using
    -- a protocol.
  , CardanoEra(..)
  , SomeNodeClientProtocol(..)
  , mkNodeClientProtocol

    -- * Errors
  , RealPBFTError(..)
  , ShelleyProtocolInstantiationError
  , renderRealPBFTError
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text

import           Cardano.Config.Types
                   (Protocol(..), SomeConsensusProtocol(..),
                    SomeNodeClientProtocol(..), TraceConstraints)
import           Cardano.Chain.Slotting (EpochSlots(..))

import           Cardano.Api.Protocol.Byron
import           Cardano.Api.Protocol.Cardano
import           Cardano.Api.Protocol.Shelley

import qualified Ouroboros.Consensus.Cardano as Consensus


mkNodeClientProtocol :: Protocol -> SomeNodeClientProtocol
mkNodeClientProtocol protocol =
    case protocol of
{-
      --TODO
      -- Mock protocols
      NodeProtocolConfigurationMock config ->
        case npcMockProtocol config of
          BFT      -> mkNodeClientProtocolMockBFT
          MockPBFT -> mkNodeClientProtocolMockPBFT
          Praos    -> mkNodeClientProtocolMockPraos
-}
      MockProtocol _ ->
        panic "TODO: mkNodeClientProtocol NodeProtocolConfigurationMock"

      -- Real protocols
      ByronProtocol ->
        mkSomeNodeClientProtocolByron
          --TODO: this is only the correct value for mainnet
          -- not for Byron testnets. This value is needed because
          -- to decode legacy EBBs one needs to know how many
          -- slots there are per-epoch. This info comes from
          -- the genesis file, but we don't have that in the
          -- client case.
          (EpochSlots 21600)
          (Consensus.SecurityParam 2160)

      ShelleyProtocol ->
        mkSomeNodeClientProtocolShelley

      CardanoProtocol ->
        mkSomeNodeClientProtocolCardano
          --TODO: this is only the correct value for mainnet
          -- not for Byron testnets. This value is needed because
          -- to decode legacy EBBs one needs to know how many
          -- slots there are per-epoch. This info comes from
          -- the genesis file, but we don't have that in the
          -- client case.
          (EpochSlots 21600)
          (Consensus.SecurityParam 2160)


-- | Many commands have variants or file formats that depend on the era.
--

data CardanoEra = ByronEraLegacy | ByronEra | ShelleyEra
  deriving Show


------------------------------------------------------------------------------
-- Errors
--

data ProtocolInstantiationError =
    ByronProtocolInstantiationError   ByronProtocolInstantiationError
  | ShelleyProtocolInstantiationError ShelleyProtocolInstantiationError
  | CardanoProtocolInstantiationError CardanoProtocolInstantiationError
  deriving Show


renderProtocolInstantiationError :: ProtocolInstantiationError -> Text
renderProtocolInstantiationError pie =
  case pie of
    ByronProtocolInstantiationError bpie ->
      renderByronProtocolInstantiationError bpie

    ShelleyProtocolInstantiationError spie ->
      renderShelleyProtocolInstantiationError spie

    CardanoProtocolInstantiationError cpie ->
      renderCardanoProtocolInstantiationError cpie


data RealPBFTError
  = IncorrectProtocolSpecified !Protocol
  | FromProtocolError !ProtocolInstantiationError
  | InvariantViolation !Text
  | TransactionTypeNotHandledYet !Text
  deriving Show

renderRealPBFTError :: RealPBFTError -> Text
renderRealPBFTError err =
  case err of
    IncorrectProtocolSpecified ptcl -> "Incorrect protocol specified: " <> (Text.pack $ show ptcl)
    FromProtocolError ptclInstErr -> renderProtocolInstantiationError ptclInstErr
    InvariantViolation invErr -> "Invariant violation: " <> invErr
    TransactionTypeNotHandledYet err' -> "Transaction type not handled yet: " <> err'
