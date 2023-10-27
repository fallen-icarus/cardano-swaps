use aiken/transaction/value.{PolicyId,AssetName}

// Allows creating unique minting policies for testing purposes.
pub const app_name = @""

// An extra parameter so that every offer asset gets its own beacon for UniSwaps
// and every trading pair gets its own beacon for BiSwaps.
pub type AssetConfig {
  asset_id: PolicyId,
  asset_name: AssetName
}
