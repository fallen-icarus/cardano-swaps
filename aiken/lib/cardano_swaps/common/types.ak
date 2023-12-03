use aiken/transaction/value.{PolicyId,AssetName}

// Allows creating unique minting policies for testing purposes.
pub const app_name = @""

pub type AssetConfig {
  asset_id: PolicyId,
  asset_name: AssetName
}

pub type Rational {
  numerator: Int,
  denominator: Int
}
