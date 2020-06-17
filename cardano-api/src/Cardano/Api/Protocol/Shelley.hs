{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Protocol.Shelley
  ( -- * Client support
    mkNodeClientProtocolShelley
  , mkSomeNodeClientProtocolShelley

    -- * Errors
  , ShelleyProtocolInstantiationError(..)
  , renderShelleyProtocolInstantiationError
  ) where

import           Cardano.Prelude
import           Prelude (String)

import qualified Data.Text as T

import           Ouroboros.Consensus.Cardano hiding (Protocol)

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Cardano.Config.Types (SomeNodeClientProtocol(..))
import           Cardano.Config.Shelley.OCert
import           Cardano.Config.Shelley.VRF
import           Cardano.Config.Shelley.KES
import           Cardano.TracingOrphanInstances.Shelley ()


------------------------------------------------------------------------------
-- Shelley protocol, client support
--

mkNodeClientProtocolShelley :: ProtocolClient (ShelleyBlock TPraosStandardCrypto)
                                              ProtocolRealTPraos
mkNodeClientProtocolShelley = ProtocolClientRealTPraos


mkSomeNodeClientProtocolShelley :: SomeNodeClientProtocol
mkSomeNodeClientProtocolShelley =
    SomeNodeClientProtocol mkNodeClientProtocolShelley




------------------------------------------------------------------------------
-- Shelley Errors
--

data ShelleyProtocolInstantiationError = GenesisReadError !FilePath !String
                                       | OCertError OperationalCertError
                                       | VRFError VRFError
                                       | KESError KESError

                                       | OCertNotSpecified
                                       | VRFKeyNotSpecified
                                       | KESKeyNotSpecified
                                       deriving Show


renderShelleyProtocolInstantiationError :: ShelleyProtocolInstantiationError
                                        -> Text
renderShelleyProtocolInstantiationError pie =
  case pie of
    GenesisReadError fp err ->
        "There was an error parsing the genesis file: "
     <> toS fp <> " Error: " <> (T.pack $ show err)

    KESError   err -> renderKESError err
    VRFError   err -> renderVRFError err
    OCertError err -> T.pack $ show err --TODO: renderOperationalCertError

    OCertNotSpecified  -> missingFlagMessage "shelley-operational-certificate"
    VRFKeyNotSpecified -> missingFlagMessage "shelley-vrf-key"
    KESKeyNotSpecified -> missingFlagMessage "shelley-kes-key"
  where
    missingFlagMessage flag =
      "To create blocks, the --" <> flag <> " must also be specified"
