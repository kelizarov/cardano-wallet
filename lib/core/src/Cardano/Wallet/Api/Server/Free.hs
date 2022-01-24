{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api.Server.Free
    ( freeBalanceTransaction
    ) where

import Prelude

import Cardano.Wallet
    ( HasNetworkLayer, genesisData, networkLayer )
import Cardano.Wallet.Api
    ( ApiLayer (..)
    , HasDBFactory
    , HasWorkerRegistry
    , dbFactory
    , workerRegistry
    )
import Cardano.Wallet.Api.Types
    ( ApiFreeBalanceTransactionPostData
    , ApiSerialisedTransaction (..)
    , ApiT (..)
    , getApiT
    )
import Cardano.Wallet.DB
    ( DBFactory (..) )
import Cardano.Wallet.Network
    ( timeInterpreter )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( GenChange (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet (..), currentTip, getState )
import Cardano.Wallet.Primitive.Slotting
    ( snapshot )
import Cardano.Wallet.Primitive.Types
    ( WalletId (..), header )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..)
    , MkWorker (..)
    , WorkerLog (..)
    , defaultWorkerAfter
    , workerResource
    )
import Control.Monad.Except
    ( MonadError, throwError )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Crypto.Hash
    ( hash )
import Data.Bits
    ( shiftL )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.Maybe
    ( fromJust )
import Data.Word
    ( Word8 )
import Servant.Server
    ( Handler (..), ServerError (..) )

import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Network as NW
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Registry as Registry
import qualified Data.ByteString as BS

import Cardano.Wallet.Api.Server
    ( ErrCreateWallet (..)
    , IsServerError (..)
    , fromApiRedeemer
    , fromExternalInput
    , idleWorker
    , liftHandler
    )

data Stateless = Stateless
instance GenChange Stateless where
    type ArgGenChange Stateless = Address
    genChange address = (address,)

-- TODO: Stop wallet from syncing?
-- Since we don't really need to track anything...

freeBalanceTransaction
    :: forall ctx s k (n :: NetworkDiscriminant).
        ( ctx ~ ApiLayer s k
        , HasNetworkLayer IO ctx
        )
    => ctx
    -> ApiFreeBalanceTransactionPostData n
    -> Handler ApiSerialisedTransaction
freeBalanceTransaction ctx body = do
    pp <- liftIO $ NW.currentProtocolParameters nl
    nodePParams <- fromJust <$> liftIO (NW.currentNodeProtocolParameters nl)
    let partialTx = W.PartialTx
            (getApiT $ body ^. #transaction)
            (fromExternalInput <$> body ^. #presetInputs)
            (fromApiRedeemer <$> body ^. #redeemers)
        availableInputs = toUtxoIndex $ body ^. #availableInputs
        collateralInputs = toUtxoIndex <$> body ^. #collateralInputs
        (ApiT changeAddress, _) = body ^. #changeAddress
    withFreeWorkerCtx ctx changeAddress $ \wrk -> do
        ti <- liftIO $ snapshot $ timeInterpreter $ ctx ^. networkLayer
        liftHandler $ ApiSerialisedTransaction . ApiT <$>
            W.balanceTransaction @IO @Stateless @k
                wrk
                changeAddress
                (pp, nodePParams)
                ti
                (availableInputs, collateralInputs)
                -- pendingTxs is only used for checking pending withdrawals,
                -- so it's safe to use mempty here
                (wallet, mempty)
                partialTx
  where
    nl = ctx ^. networkLayer
    (block0, _, _) = ctx ^. genesisData
    wallet = Wallet
        -- selectAssets doesn't use utxo & currentTip
        { utxo = UTxO.empty
        , currentTip = header block0
        , getState = Stateless
        }
    toUtxoIndex = W.utxoIndexFromInputs . fmap fromExternalInput

-- TODO: Make this a configurable parameter
walletPoolSize :: Word8
walletPoolSize = 16

walletIdShardFromAddress :: Address -> WalletId
walletIdShardFromAddress (Address a) =
    let f i b = (i `shiftL` 8 + fromIntegral b) `mod` walletPoolSize
     in WalletId . hash . BS.singleton $ BS.foldl' f 0 a

-- TODO: Remove DB layer, since we don't actually use it
withFreeWorkerCtx
    :: forall ctx s k m a.
        ( ctx ~ ApiLayer s k
        , HasWorkerRegistry s k ctx
        , HasDBFactory s k ctx
        , MonadIO m
        , MonadError ServerError m
        )
    => ctx
    -> Address
    -> (WorkerCtx ctx -> m a)
    -> m a
withFreeWorkerCtx ctx address action = do
    worker <- Registry.lookup re wid >>= \case
         Nothing ->
            liftIO (Registry.register @_ @ctx re ctx wid workerConfig) >>= \case
                Nothing -> throwError . toServerError $
                    ErrCreateWalletFailedToCreateWorker
                Just w -> pure w
         Just w -> pure w
    action $ hoistResource (workerResource worker) (MsgFromWorker wid) ctx
  where
    re = ctx ^. workerRegistry @s @k
    df = ctx ^. dbFactory @s @k
    wid = walletIdShardFromAddress address
    -- FIXME: ErrCheckWalletIntegrityNoSuchWallet will be raised on startup
    -- if not used with in-memory database. However it doesn't have any
    -- negative impact on the actual logic.
    workerConfig = MkWorker
        { workerAcquire = withDatabase df wid
        , workerBefore = \_ _ -> pure ()
        , workerAfter = defaultWorkerAfter
        , workerMain = idleWorker
        }
