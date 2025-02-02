# Vesting Contract

## Components

### VestingParams
Defines the token to lock and the lock period.

### VestingDatum
Stores the locker's public key hash, the locked amount, and the lock time.

### VestingRedeemer
Defines the actions (**LockTokens** or **ClaimTokens**) that can be performed on the contract.

### vestingValidator
Contains the logic to validate token locking and claiming.

### Helper Functions
Functions to validate the lock period and ensure tokens cannot be claimed prematurely.

###  Deployment:

    Compile the contract using the Plutus compiler.

    Deploy the contract on the Cardano blockchain using cardano-cli or a Plutus Application Backend (PAB).

    Integrate with a front-end interface for user interaction.

  ###  Notes:

    Time-Lock Token: The time-lock token can be implemented as a custom token minted when tokens are locked and burned when tokens are claimed.
