{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Protocol.Byron
  (
    -- * Client support
    mkNodeClientProtocolByron
  , mkSomeNodeClientProtocolByron

    -- * Errors
  , ByronProtocolInstantiationError(..)
  , renderByronProtocolInstantiationError
  ) where

import           Cardano.Prelude

import           Codec.CBOR.Read (DeserialiseFailure)
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochSlots)

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Cardano hiding (Protocol)

import           Cardano.Config.Types (SomeNodeClientProtocol(..))
import           Cardano.TracingOrphanInstances.Byron ()


------------------------------------------------------------------------------
-- Real Byron protocol, client support
--

mkNodeClientProtocolByron :: EpochSlots
                          -> SecurityParam
                          -> ProtocolClient ByronBlock ProtocolRealPBFT
mkNodeClientProtocolByron epochSlots securityParam =
    ProtocolClientRealPBFT epochSlots securityParam


mkSomeNodeClientProtocolByron :: EpochSlots
                              -> SecurityParam
                              -> SomeNodeClientProtocol
mkSomeNodeClientProtocolByron epochSlots securityParam =
    SomeNodeClientProtocol
      (mkNodeClientProtocolByron epochSlots securityParam)

------------------------------------------------------------------------------
-- Byron Errors
--

data ByronProtocolInstantiationError =
    CanonicalDecodeFailure !FilePath !Text
  | DelegationCertificateFilepathNotSpecified
  | GenesisConfigurationError !FilePath !Genesis.ConfigurationError
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | PbftError !PBftLeaderCredentialsError
  | SigningKeyDeserialiseFailure !FilePath !DeserialiseFailure
  | SigningKeyFilepathNotSpecified
  deriving Show


renderByronProtocolInstantiationError :: ByronProtocolInstantiationError -> Text
renderByronProtocolInstantiationError pie =
  case pie of
    CanonicalDecodeFailure fp failure -> "Canonical decode failure in " <> toS fp
                                         <> " Canonical failure: " <> failure
    DelegationCertificateFilepathNotSpecified -> "Delegation certificate filepath not specified"
    --TODO: Implement configuration error render function in cardano-ledger
    GenesisConfigurationError fp genesisConfigError -> "Genesis configuration error in: " <> toS fp
                                                       <> " Error: " <> (Text.pack $ show genesisConfigError)
    GenesisReadError fp err ->  "There was an error parsing the genesis file: " <> toS fp
                                <> " Error: " <> (Text.pack $ show err)
    -- TODO: Implement PBftLeaderCredentialsError render function in ouroboros-network
    PbftError pbftLeaderCredentialsError -> "PBFT leader credentials error: " <> (Text.pack $ show pbftLeaderCredentialsError)
    SigningKeyDeserialiseFailure fp deserialiseFailure -> "Signing key deserialisation error in: " <> toS fp
                                                           <> " Error: " <> (Text.pack $ show deserialiseFailure)
    SigningKeyFilepathNotSpecified -> "Signing key filepath not specified"
