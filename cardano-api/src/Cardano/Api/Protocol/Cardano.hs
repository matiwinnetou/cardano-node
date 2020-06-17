{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Api.Protocol.Cardano
  ( -- * Client support
    mkNodeClientProtocolCardano
  , mkSomeNodeClientProtocolCardano

    -- * Errors
  , CardanoProtocolInstantiationError(..)
  , renderCardanoProtocolInstantiationError
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text

import           Cardano.Api.Protocol.Byron (ByronProtocolInstantiationError,
                   renderByronProtocolInstantiationError)
import           Cardano.Api.Protocol.Shelley (ShelleyProtocolInstantiationError,
                   renderShelleyProtocolInstantiationError)

import           Cardano.Chain.Slotting (EpochSlots)

import           Ouroboros.Consensus.Cardano hiding (Protocol)
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()

import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Cardano.Condense ()

import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Cardano.Config.Types
                   (SomeNodeClientProtocol(..),
                    HasKESMetricsData(..), KESMetricsData(..))

import           Cardano.TracingOrphanInstances.Byron ()
import           Cardano.TracingOrphanInstances.Shelley ()
import           Cardano.TracingOrphanInstances.HardFork ()


--TODO: move ToObject tracing instances to Cardano.TracingOrphanInstances.Consensus
--      and do them generically for the hard fork combinator
instance HasKESMetricsData (CardanoBlock c) where
    getKESMetricsData _protoInfo _forgeState = NoKESMetricsData
    --TODO distinguish on the era and use getKESMetricsData on the appropriate era


------------------------------------------------------------------------------
-- Real Cardano protocol, client support
--

mkNodeClientProtocolCardano :: EpochSlots
                            -> SecurityParam
                            -> ProtocolClient (CardanoBlock TPraosStandardCrypto)
                                              ProtocolCardano
mkNodeClientProtocolCardano epochSlots securityParam =
    ProtocolClientCardano epochSlots securityParam


mkSomeNodeClientProtocolCardano :: EpochSlots
                                -> SecurityParam
                                -> SomeNodeClientProtocol
mkSomeNodeClientProtocolCardano epochSlots securityParam =
    SomeNodeClientProtocol
      (mkNodeClientProtocolCardano epochSlots securityParam)

------------------------------------------------------------------------------
-- Cardano Errors
--

data CardanoProtocolInstantiationError =
       CardanoProtocolInstantiationErrorByron
         ByronProtocolInstantiationError

     | CardanoProtocolInstantiationErrorShelley
         ShelleyProtocolInstantiationError
  deriving Show

renderCardanoProtocolInstantiationError :: CardanoProtocolInstantiationError
                                        -> Text.Text
renderCardanoProtocolInstantiationError
  (CardanoProtocolInstantiationErrorByron err) =
    renderByronProtocolInstantiationError err

renderCardanoProtocolInstantiationError
  (CardanoProtocolInstantiationErrorShelley err) =
    renderShelleyProtocolInstantiationError err
