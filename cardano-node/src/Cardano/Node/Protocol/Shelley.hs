{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Shelley
  ( -- * Protocol exposing the specific type
    -- | Use this when you need the specific instance
    mkConsensusProtocolShelley

    -- * Protocols hiding the specific type
    -- | Use this when you want to handle protocols generically
  , mkSomeConsensusProtocolShelley
    -- * Reusable parts
  , readGenesis
  , readLeaderCredentials

  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Data.Aeson as Aeson

import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol
                   (TPraosStandardCrypto, TPraosIsCoreNode(..))
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (HotKey (..))
import           Ouroboros.Consensus.Shelley.Node
                   (TPraosLeaderCredentials(..))

import           Shelley.Spec.Ledger.PParams (ProtVer(..))

import           Cardano.Api.Protocol.Shelley (ShelleyProtocolInstantiationError(..))

import           Cardano.Config.Types
                   (NodeShelleyProtocolConfiguration(..),
                    ProtocolFilepaths(..), GenesisFile (..),
                    SomeConsensusProtocol(..))
import           Cardano.Config.Shelley.OCert
import           Cardano.Config.Shelley.VRF
import           Cardano.Config.Shelley.KES
import           Cardano.TracingOrphanInstances.Shelley ()


------------------------------------------------------------------------------
-- Shelley protocol
--

-- | Make 'SomeConsensusProtocol' using the Shelley instance.
--
-- This lets us handle multiple protocols in a generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolShelley
  :: NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolShelley nc files =

    -- Applying the SomeConsensusProtocol here is a check that
    -- the type of mkConsensusProtocolShelley fits all the class
    -- constraints we need to run the protocol.
    SomeConsensusProtocol <$> mkConsensusProtocolShelley nc files


-- | Instantiate 'Consensus.Protocol' for Shelley specifically.
--
-- Use this when you need to run the consensus with this specific protocol.
--
mkConsensusProtocolShelley
  :: NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO
             (Consensus.Protocol IO (ShelleyBlock TPraosStandardCrypto)
                                 ProtocolRealTPraos)
mkConsensusProtocolShelley NodeShelleyProtocolConfiguration {
                            npcShelleyGenesisFile,
                            npcShelleySupportedProtocolVersionMajor,
                            npcShelleySupportedProtocolVersionMinor,
                            npcShelleyMaxSupportedProtocolVersion
                          }
                          files = do
    genesis <- readGenesis npcShelleyGenesisFile
    optionalLeaderCredentials <- readLeaderCredentials files

    return $
      ProtocolRealTPraos
        genesis
        (ProtVer npcShelleySupportedProtocolVersionMajor
                 npcShelleySupportedProtocolVersionMinor)
        npcShelleyMaxSupportedProtocolVersion
        optionalLeaderCredentials


readGenesis :: GenesisFile
            -> ExceptT ShelleyProtocolInstantiationError IO
                       (ShelleyGenesis TPraosStandardCrypto)
readGenesis (GenesisFile file) =
    firstExceptT (GenesisReadError file) $
      ExceptT $ handle (\(e :: IOException) -> return $ Left $ show e) $
        Aeson.eitherDecodeFileStrict' file


readLeaderCredentials :: Maybe ProtocolFilepaths
                      -> ExceptT ShelleyProtocolInstantiationError IO
                                 (Maybe (TPraosLeaderCredentials TPraosStandardCrypto))

-- It's ok to supply none of the files
readLeaderCredentials Nothing = return Nothing
readLeaderCredentials (Just ProtocolFilepaths {
                              shelleyCertFile = Nothing,
                              shelleyVRFFile  = Nothing,
                              shelleyKESFile  = Nothing
                            }) = return Nothing

-- Or to supply all of the files
readLeaderCredentials (Just ProtocolFilepaths {
                              shelleyCertFile = Just certFile,
                              shelleyVRFFile  = Just vrfFile,
                              shelleyKESFile  = Just kesFile
                            }) = do

    (opcert, vkey) <- firstExceptT OCertError $ readOperationalCert certFile
    vrfKey <- firstExceptT VRFError $ readVRFSigningKey vrfFile
    kesKey <- firstExceptT KESError $ HotKey 0 <$> readKESSigningKey kesFile

    return $ Just TPraosLeaderCredentials {
               tpraosLeaderCredentialsIsCoreNode =
                 TPraosIsCoreNode {
                   tpraosIsCoreNodeOpCert     = opcert,
                   tpraosIsCoreNodeColdVerKey = vkey,
                   tpraosIsCoreNodeSignKeyVRF = vrfKey
                 },
               tpraosLeaderCredentialsSignKey = kesKey
             }

-- But not ok to supply some of the files without the others.
readLeaderCredentials (Just ProtocolFilepaths {shelleyCertFile = Nothing}) =
    throwError OCertNotSpecified
readLeaderCredentials (Just ProtocolFilepaths {shelleyVRFFile = Nothing}) =
    throwError VRFKeyNotSpecified
readLeaderCredentials (Just ProtocolFilepaths {shelleyKESFile = Nothing}) =
    throwError KESKeyNotSpecified
