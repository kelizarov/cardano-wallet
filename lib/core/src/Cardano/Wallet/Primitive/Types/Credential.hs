{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.Credential
    ( Credential (..)
    , credentialFromAddress
    )
    where

import Prelude

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Wallet.Primitive.Types.Hash (Hash(..))
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Crypto.Hash.Class as Crypto

-- This is used to bridge between cardano-ledger and cardano-node types
data Credential
    = KeyHash (Hash "Key")
    | ScriptHash (Hash "Script")
    deriving (Read, Show, Generic, Eq, Ord)

credentialFromAddress :: Address -> Maybe Credential
credentialFromAddress (Address bs) =
    case Ledger.deserialiseAddr @StandardCrypto bs of
        Just (Ledger.Addr _ cred _) -> Just $ case cred of
            Ledger.KeyHashObj (Ledger.KeyHash kh) -> newCred KeyHash kh
            Ledger.ScriptHashObj (Ledger.ScriptHash sh) -> newCred ScriptHash sh
        _ -> Nothing
  where
    newCred = (. Hash . Crypto.hashToBytes)
