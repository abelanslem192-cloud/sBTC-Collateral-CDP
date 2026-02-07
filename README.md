# 🏦 sBTC Collateral CDP

> Lock **sBTC**, mint **Stablecoin**, and manage your debt positions on Stacks! 🚀

## 📜 Overview

This project implements a **Minimum Viable Product (MVP)** for a **Collateralized Debt Position (CDP)** system. Users can lock their sBTC tokens as collateral to mint a stablecoin (STBL). The system ensures solvency through over-collateralization and liquidation mechanisms.

## ✨ Features

- **🔐 Collateral Locking**: Securely lock sBTC in a smart contract vault.
- **💸 Mint Stablecoin**: Borrow against your collateral by minting `STBL` tokens.
- **📉 Repay & Burn**: Repay your debt to unlock your collateral.
- **⚖️ Loan-to-Value (LTV) Protection**: Maintains a safe liquidation ratio (150%).
- **🌊 Liquidation**: Keepers can liquidate unsafe vaults to protect the protocol.
- **🔮 Mock Oracle**: Admin-controlled price feed for sBTC/USD.

## 🏗 Contracts

| Contract | Description |
|----------|-------------|
| `sbtc-token` | Mock **sBTC** implementation (SIP-010). Includes a faucet! 🚰 |
| `stable-token` | The stablecoin **STBL** (SIP-010). Mintable/Burnable by the Vault only. |
| `cdp-vault` | Core logic for managing vaults, debt, and liquidations. 🏦 |
| `sip-010-trait` | Standard Token Trait definition. |

## 🚀 Usage

### Requirements
- [Clarinet](https://github.com/hirosystems/clarinet) installed.

### 🧪 Testing & Simulation

1. **Initialize Console**
   ```bash
   clarinet console
   ```

2. **Get sBTC (Faucet)**
   ```clarity
   (contract-call? .sbtc-token faucet-mint u100000000) ;; Mint 1 sBTC
   ```

3. **Open Vault & Deposit**
   ```clarity
   (contract-call? .cdp-vault deposit-collateral u100000000)
   ```

4. **Borrow Stablecoin**
   *Assuming Price = $50,000. 1 sBTC = $50,000 collateral value.*
   *Max Safe Debt (150% Ratio) = $33,333.*
   ```clarity
   (contract-call? .cdp-vault borrow u10000000) ;; Borrow 10,000 STBL
   ```

5. **Check Ratio**
   ```clarity
   (contract-call? .cdp-vault calculate-current-ratio tx-sender)
   ```

## 🛠 Development

**Check contracts for errors:**
```bash
clarinet check
```

**Run tests:**
```bash
clarinet test
```

## ⚠️ Disclaimer
This is an MVP for educational purposes. **Not audited** and should not be used in production without further security validation. 🛡️
