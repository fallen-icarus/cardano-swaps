use aiken/transaction/value.{AssetName, PolicyId}

// Allows creating unique minting policies for testing purposes.
pub const app_name = @""

pub type AssetConfig {
  asset_id: PolicyId,
  asset_name: AssetName,
}

pub type Rational {
  numerator: Int,
  denominator: Int,
}
