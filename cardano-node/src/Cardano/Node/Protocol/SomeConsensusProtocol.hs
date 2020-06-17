module Cardano.Node.Protocol.SomeConsensusProtocol
  ( mkConsensusProtocol
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as Text

import           Cardano.Config.Types
                   (NodeConfiguration(..), NodeProtocolConfiguration(..),
                    NodeMockProtocolConfiguration(..), ProtocolFilepaths(..),
                    Protocol(..), MockProtocol(..), SomeConsensusProtocol(..),
                    SomeNodeClientProtocol(..), TraceConstraints)
import           Cardano.Chain.Slotting (EpochSlots(..))

import           Cardano.Api.Protocol.Byron
import           Cardano.Api.Protocol.Cardano
import           Cardano.Api.Protocol.Mock
import           Cardano.Api.Protocol.Shelley

import qualified Ouroboros.Consensus.Cardano as Consensus


------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--

mkConsensusProtocol
  :: NodeConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocol NodeConfiguration{ncProtocolConfig} files =
    case ncProtocolConfig of

      -- Mock protocols
      NodeProtocolConfigurationMock config ->
        case npcMockProtocol config of
          MockBFT   -> pure $ mkSomeConsensusProtocolMockBFT   config
          MockPBFT  -> pure $ mkSomeConsensusProtocolMockPBFT  config
          MockPraos -> pure $ mkSomeConsensusProtocolMockPraos config

      -- Real protocols
      NodeProtocolConfigurationByron config ->
        firstExceptT ByronProtocolInstantiationError $
          mkSomeConsensusProtocolByron config files

      NodeProtocolConfigurationShelley config ->
        firstExceptT ShelleyProtocolInstantiationError $
          mkSomeConsensusProtocolShelley config files

      NodeProtocolConfigurationCardano byronConfig shelleyConfig ->
        firstExceptT CardanoProtocolInstantiationError $
          mkSomeConsensusProtocolCardano byronConfig shelleyConfig files
