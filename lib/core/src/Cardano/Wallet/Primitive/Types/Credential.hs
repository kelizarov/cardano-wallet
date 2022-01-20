{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.Credential
    ( Credential (..)
    , credentialFromAddress
    , fromLedgerCredential
    )
    where

import Prelude

import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger

-- This is used to bridge between cardano-ledger and cardano-node types
data Credential
    = KeyHash (Hash "Key")
    | ScriptHash (Hash "Script")
    deriving (Read, Show, Generic, Eq, Ord)

credentialFromAddress :: Address -> Maybe Credential
credentialFromAddress (Address bs) =
    fromLedgerCredential <$> case Ledger.deserialiseAddr @StandardCrypto bs of
        Just (Ledger.Addr _ cred _) -> Just cred
        _ -> Nothing

fromLedgerCredential :: Ledger.Credential kr crypto -> Credential
fromLedgerCredential cred =
    case cred of
        Ledger.KeyHashObj (Ledger.KeyHash kh) -> newCred KeyHash kh
        Ledger.ScriptHashObj (Ledger.ScriptHash sh) -> newCred ScriptHash sh
  where
    newCred = (. Hash . Crypto.hashToBytes)
