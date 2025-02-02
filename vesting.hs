{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module VestingContract where

import           PlutusTx
import           PlutusTx.Prelude
import           Ledger
import           Ledger.Ada
import           Ledger.Value
import           Ledger.Typed.Scripts
import           Ledger.TimeSlot
import           Plutus.Contract
import           Plutus.V1.Ledger.Api
import           Plutus.V1.Ledger.Contexts
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.Interval
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Prelude (IO, Show)
import qualified Prelude

-- Define the vesting parameters
data VestingParams = VestingParams
    { lockToken    :: AssetClass  -- Token to lock
    , lockPeriod   :: POSIXTime   -- Lock period in seconds
    } deriving (Show, Generic, ToJSON, FromJSON)

-- Define the vesting action
data VestingAction
    = LockTokens
    | ClaimTokens
    deriving (Show, Generic, ToJSON, FromJSON)

-- Define the vesting datum
data VestingDatum = VestingDatum
    { locker       :: PubKeyHash  -- User who locked the tokens
    , lockedAmount :: Integer     -- Amount of tokens locked
    , lockTime     :: POSIXTime   -- Time when tokens were locked
    } deriving (Show, Generic, ToJSON, FromJSON)

-- Define the vesting redeemer
data VestingRedeemer = VestingRedeemer
    { action       :: VestingAction
    } deriving (Show, Generic, ToJSON, FromJSON)

-- Define the vesting validator
vestingValidator :: VestingParams -> VestingDatum -> VestingRedeemer -> ScriptContext -> Bool
vestingValidator params datum redeemer ctx =
    case action redeemer of
        LockTokens ->
            -- Validate token locking
            traceIfFalse "Invalid token lock" (validateLock params datum ctx)
        ClaimTokens ->
            -- Validate token claiming
            traceIfFalse "Invalid token claim" (validateClaim params datum ctx)
  where
    info = scriptContextTxInfo ctx

-- Helper function to validate token locking
validateLock :: VestingParams -> VestingDatum -> ScriptContext -> Bool
validateLock params datum ctx =
    let lockAmount = lockedAmount datum
        lockTime = lockTime datum
    in lockAmount > 0
    && traceIfFalse "Invalid lock amount" (lockAmount > 0)
    && traceIfFalse "Invalid lock time" (lockTime >= txInfoValidRange info)

-- Helper function to validate token claiming
validateClaim :: VestingParams -> VestingDatum -> ScriptContext -> Bool
validateClaim params datum ctx =
    let currentTime = txInfoValidRange info
        lockEndTime = lockTime datum + lockPeriod params
    in intervalContains (interval lockEndTime (lockEndTime + 1)) currentTime
    && traceIfFalse "Lock period not expired" (intervalContains (interval lockEndTime (lockEndTime + 1)) currentTime)

-- Compile the validator
vestingValidatorCompiled :: VestingParams -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
vestingValidatorCompiled params = $$(compile [|| \d r ctx -> vestingValidator params (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData ctx) ||])

-- Define the vesting script
vestingScript :: VestingParams -> Script
vestingScript params = mkValidatorScript (vestingValidatorCompiled params)

-- Define the vesting address
vestingAddress :: VestingParams -> Address
vestingAddress params = scriptHashAddress (validatorHash (vestingScript params))

-- Define the vesting contract
vestingContract :: VestingParams -> Contract () VestingSchema Text ()
vestingContract params = do
    -- Lock tokens action
    handleLockTokens <- endpoint @"lockTokens" $ \(locker, lockedAmount, lockTime) -> do
        let datum = VestingDatum { locker = locker, lockedAmount = lockedAmount, lockTime = lockTime }
        let tx = mustPayToTheScript datum (assetClassValue (lockToken params) lockedAmount)
        submitTxConstraints (vestingScript params) tx
        awaitTxConfirmed (getCardanoTxId tx)
        logInfo @Text $ "Tokens locked: " <> show lockedAmount <> " " <> show (lockToken params)

    -- Claim tokens action
    handleClaimTokens <- endpoint @"claimTokens" $ \(locker, lockedAmount) -> do
        let redeemer = VestingRedeemer { action = ClaimTokens }
        let tx = mustSpendScriptOutput (vestingScript params) redeemer
        submitTxConstraints (vestingScript params) tx
        awaitTxConfirmed (getCardanoTxId tx)
        logInfo @Text $ "Tokens claimed: " <> show lockedAmount <> " " <> show (lockToken params)

    -- Combine the handlers
    selectList [handleLockTokens, handleClaimTokens]

-- Define the schema
type VestingSchema =
    Endpoint "lockTokens" (PubKeyHash, Integer, POSIXTime)
    .\/ Endpoint "claimTokens" (PubKeyHash, Integer)

-- Define the main function
main :: IO ()
main = do
    -- Define the vesting parameters
    let params = VestingParams
            { lockToken = assetClass "BOTLY" "BOTLY"
            , lockPeriod = 86400  -- 1 day in seconds
            }

    -- Run the vesting contract
    runPlutusApp $ vestingContract params
