# 🤝 Coopshare - Cooperative Ownership Smart Contract

A Clarity smart contract that enables decentralized cooperative ownership with member shares, voting rights, and dividend distribution on the Stacks blockchain.

## ✨ Features

- 👥 **Member Management**: Join the cooperative and manage membership
- 📈 **Share System**: Purchase, transfer, and track ownership shares
- 🗳️ **Democratic Voting**: Create proposals and vote with share-weighted power
- 💰 **Dividend Distribution**: Automatic profit sharing based on ownership percentage
- 🔒 **Secure Governance**: Transparent and immutable cooperative management

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Basic understanding of Clarity smart contracts
- STX tokens for transactions

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Deploy using Clarinet:

```bash
clarinet deploy
```

## 📋 Usage

### Joining the Cooperative

```clarity
(contract-call? .Coopshare join-coop u10)
```

Join the cooperative with an initial number of shares (10 in this example).

### Purchasing Additional Shares

```clarity
(contract-call? .Coopshare purchase-shares u5)
```

Purchase additional shares at 1 STX per share.

### Creating Proposals

```clarity
(contract-call? .Coopshare create-proposal 
    "New Equipment Purchase" 
    "Proposal to buy new manufacturing equipment for $50,000" 
    u1440)
```

Create a proposal with a title, description, and voting duration (in blocks).

### Voting on Proposals

```clarity
(contract-call? .Coopshare vote-on-proposal u1 true)
```

Vote on proposal #1 with `true` for yes, `false` for no. Voting power is weighted by share ownership.

### Claiming Dividends

```clarity
(contract-call? .Coopshare claim-dividend)
```

Claim your share of distributed dividends based on ownership percentage.

### Transferring Shares

```clarity
(contract-call? .Coopshare transfer-shares 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 u3)
```

Transfer 3 shares to another cooperative member.

## 🔍 Read-Only Functions

### Get Member Information
```clarity
(contract-call? .Coopshare get-member-info 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

### Check Proposal Status
```clarity
(contract-call? .Coopshare get-proposal-status u1)
```

### View Dividend Information
```clarity
(contract-call? .Coopshare get-member-dividend-info 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

### Get Cooperative Statistics
```clarity
(contract-call? .Coopshare get-total-shares)
(contract-call? .Coopshare get-total-members)
(contract-call? .Coopshare get-dividend-pool)
```

## 🏗️ Contract Architecture

### Data Structures

- **Members Map**: Stores member information including shares, join date, and dividend claims
- **Proposals Map**: Contains all governance proposals with voting data
- **Votes Map**: Tracks individual member votes on proposals
- **Global Variables**: Total shares, members, dividend pool, and counters

### Key Functions

1. **Membership Functions**: `join-coop`, `purchase-shares`, `transfer-shares`
2. **Governance Functions**: `create-proposal`, `vote-on-proposal`
3. **Financial Functions**: `distribute-dividends`, `claim-dividend`, `add-to-dividend-pool`
4. **Query Functions**: Various read-only functions for data retrieval

## 🛡️ Security Features

- ✅ Member-only operations
- ✅ Share balance validation
- ✅ Voting period enforcement
- ✅ Double-voting prevention
- ✅ Owner-only dividend distribution
- ✅ Comprehensive error handling

## 📊 Error Codes

| Code | Description |
|------|-------------|
| u100 | Unauthorized access |
| u101 | Already a member |
| u102 | Not a member |
| u103 | Insufficient shares |
| u104 | Invalid amount |
| u105 | Proposal not found |
| u106 | Already voted |
| u107 | Voting period ended |
| u108 | Voting still active |

