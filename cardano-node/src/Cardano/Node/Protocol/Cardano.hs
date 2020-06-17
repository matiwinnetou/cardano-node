{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Protocol.Cardano
  ( -- * Protocol exposing the specific type
    -- | Use this when you need the specific instance
    mkConsensusProtocolCardano

    -- * Protocols hiding the specific type
    -- | Use this when you want to handle protocols generically
  , mkSomeConsensusProtocolCardano


    -- * Errors
  , CardanoProtocolInstantiationError(..)
  , renderCardanoProtocolInstantiationError

  ) where

import           Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Cardano.Chain.Update as Byron

import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()

import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Cardano.Condense ()

import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import qualified Shelley.Spec.Ledger.PParams as Shelley

import           Cardano.Config.Types
                   (NodeByronProtocolConfiguration(..),
                    NodeShelleyProtocolConfiguration(..),
                    ProtocolFilepaths(..), SomeConsensusProtocol(..))

import           Cardano.TracingOrphanInstances.Byron ()
import           Cardano.TracingOrphanInstances.Shelley ()
import           Cardano.TracingOrphanInstances.HardFork ()

import qualified Cardano.Node.Protocol.Byron as Byron
import qualified Cardano.Node.Protocol.Shelley as Shelley
import           Cardano.Api.Protocol.Cardano (CardanoProtocolInstantiationError(..),
                   renderCardanoProtocolInstantiationError)

------------------------------------------------------------------------------
-- Real Cardano protocol
--

-- | Make 'SomeConsensusProtocol' using the Cardano instance.
--
-- The Cardano protocol instance is currently the sequential composition of
-- the Byron and Shelley protocols, and will likely be extended in future
-- with further sequentially composed protocol revisions.
--
-- The use of 'SomeConsensusProtocol' lets us handle multiple protocols in a
-- generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolCardano
  :: NodeByronProtocolConfiguration
  -> NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT CardanoProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolCardano ncb ncs files =

    -- Applying the SomeConsensusProtocol here is a check that
    -- the type of mkConsensusProtocolCardano fits all the class
    -- constraints we need to run the protocol.
    SomeConsensusProtocol <$> mkConsensusProtocolCardano ncb ncs files


-- | Instantiate 'Consensus.Protocol' for Byron specifically.
--
-- Use this when you need to run the consensus with this specific protocol.
--
mkConsensusProtocolCardano
  :: NodeByronProtocolConfiguration
  -> NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT CardanoProtocolInstantiationError IO
             (Consensus.Protocol IO (CardanoBlock TPraosStandardCrypto)
                                    ProtocolCardano)
mkConsensusProtocolCardano NodeByronProtocolConfiguration {
                             npcByronGenesisFile,
                             npcByronReqNetworkMagic,
                             npcByronPbftSignatureThresh,
                             npcByronApplicationName,
                             npcByronApplicationVersion,
                             npcByronSupportedProtocolVersionMajor,
                             npcByronSupportedProtocolVersionMinor,
                             npcByronSupportedProtocolVersionAlt
                           }
                           NodeShelleyProtocolConfiguration {
                             npcShelleyGenesisFile,
                             npcShelleySupportedProtocolVersionMajor,
                             npcShelleySupportedProtocolVersionMinor,
                             npcShelleyMaxSupportedProtocolVersion
                           }
                           files = do
    byronGenesis <-
      firstExceptT CardanoProtocolInstantiationErrorByron $
        Byron.readGenesis npcByronGenesisFile npcByronReqNetworkMagic

    byronLeaderCredentials <-
      firstExceptT CardanoProtocolInstantiationErrorByron $
        Byron.readLeaderCredentials byronGenesis files

    shelleyGenesis <-
      firstExceptT CardanoProtocolInstantiationErrorShelley $
        Shelley.readGenesis npcShelleyGenesisFile

    shelleyLeaderCredentials <-
      firstExceptT CardanoProtocolInstantiationErrorShelley $
        Shelley.readLeaderCredentials files

    return $!
      Consensus.ProtocolCardano
        -- Byron parameters
        byronGenesis
        (PBftSignatureThreshold <$> npcByronPbftSignatureThresh)
        (Byron.ProtocolVersion npcByronSupportedProtocolVersionMajor
                               npcByronSupportedProtocolVersionMinor
                               npcByronSupportedProtocolVersionAlt)
        (Byron.SoftwareVersion npcByronApplicationName
                               npcByronApplicationVersion)
        byronLeaderCredentials

        -- Shelley parameters
        shelleyGenesis
        (Shelley.ProtVer npcShelleySupportedProtocolVersionMajor
                         npcShelleySupportedProtocolVersionMinor)
        npcShelleyMaxSupportedProtocolVersion
        shelleyLeaderCredentials

        -- Hard fork parameters
        Consensus.NoHardCodedTransition --TODO
