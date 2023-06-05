use aiken/hash.{Hash,Blake2b_224}
use aiken/transaction.{Input, Output, ScriptContext,Datum,InlineDatum,Transaction} as tx
use aiken/transaction/credential.{Address,VerificationKeyCredential,Inline,ScriptCredential,Script}
use aiken/transaction/value.{PolicyId,AssetName,Value}
use aiken/list.{all,has}
use aiken/dict.{has_key,Dict}
use aiken/option.{is_some}
use aiken/string
use aiken/interval
// use aiken/cbor
use cardano_swaps/ratio.{Rational,unsafe_ratio,from_integer}

const app_name = @"testing"

type SwapConfig {
  offer: (PolicyId,AssetName),
  ask: (PolicyId,AssetName)
}

type SwapDatum {
  BeaconSymbol { symbol: PolicyId }
  SwapPrice { price: Rational }
}

type SwapRedeemer {
  Close
  Update
  Swap
}

type BeaconRedeemer {
  MintBeacon
  BurnBeacon
}

// The datum must be an inline SwapDatum.
fn parse_datum(raw_datum: Datum) -> SwapDatum {
  when raw_datum is {
    InlineDatum(d) -> {
      expect swap_datum: SwapDatum = d // This will error if datum is not SwapDatum.
      swap_datum
    }
    _ -> error @"All swap datums must be inline datums"
  }
}

test parse_datum_test1() {
  let swap_datum = SwapPrice(unsafe_ratio(1,4))
  let datum = InlineDatum(swap_datum)
  parse_datum(datum) == swap_datum
}

// This function is only used for checking SwapPrice datums. All prices must be > 0.
// It will either return True or crash with an error.
fn valid_swap_datum(maybe_price: Option<Rational>, datum: SwapDatum) -> Bool {
  when datum is {
    SwapPrice(price) -> {
      if ratio.gt(price,ratio.zero()) {
        when maybe_price is {
          Some(req_price) -> {
            if req_price == price {
              True
            } else {
              error @"SwapPrice /= weighted avg"
            }
          }
          None -> True
        }
      } else {
        error @"All prices must be positve"
      }
    }
    BeaconSymbol(_) -> error @"All normal output datums must be SwapPrice datums"
  }
}

// Tests 1 and 2 should succeed.
test valid_swap_datum_test1() {
  valid_swap_datum(None,SwapPrice(unsafe_ratio(1,10)))
}

test valid_swap_datum_test2() {
  valid_swap_datum(Some(unsafe_ratio(1,10)),SwapPrice(unsafe_ratio(1,10)))
}

// Tests 3,4,5,6,7 should throw an error and therefore always fail.
// test valid_swap_datum_test3() {
//   valid_swap_datum(Some(unsafe_ratio(4,10)),SwapPrice(unsafe_ratio(1,10)))
// }

// test valid_swap_datum_test4() {
//   !valid_swap_datum(None,SwapPrice(unsafe_ratio(-1,10)))
// }

// test valid_swap_datum_test5() {
//   !valid_swap_datum(None,BeaconSymbol(#"00"))
// }

// test valid_swap_datum_test6() {
//   valid_swap_datum(Some(unsafe_ratio(4,10)),SwapPrice(unsafe_ratio(-1,10)))
// }

// test valid_swap_datum_test7() {
//   valid_swap_datum(Some(unsafe_ratio(4,10)),BeaconSymbol(#"00"))
// }

// This checks that all outputs to the swap address have a valid SwapDatum.
fn valid_update_outputs(outs: List<Output>, swap_addr: Address) -> Bool {
  let has_proper_datum = fn (x: Output) {
    let Output(addr,_,datum,_) = x
    if addr == swap_addr {
      parse_datum(datum) |> valid_swap_datum(None,_)
    } else {
      True
    }
  }
  all(outs,has_proper_datum)
}

// Tests 1 and 2 should pass.
test valid_update_outputs_tests1() {
  let swap_addr = Address(VerificationKeyCredential("00"),None)
  let swap_datum = InlineDatum(SwapPrice(unsafe_ratio(1,10)))
  let outs = [Output(swap_addr,value.zero(),swap_datum,None)]
  valid_update_outputs(outs,swap_addr)
}

test valid_update_outputs_tests2() {
  let swap_addr = Address(credential.ScriptCredential("00"),None)
  let user_addr = Address(VerificationKeyCredential("ff"),None)
  let swap_datum = InlineDatum(SwapPrice(unsafe_ratio(1,10)))
  let user_datum = tx.NoDatum
  let outs = 
    [ Output(swap_addr,value.zero(),swap_datum,None)
    , Output(user_addr,value.zero(),user_datum,None)
    ]
  valid_update_outputs(outs,swap_addr)
}

// Tests 3,4,5 should all throw errors and therefore always fail.
// test valid_update_outputs_tests3() {
//   let swap_addr = Address(credential.ScriptCredential("00"),None)
//   let user_addr = Address(VerificationKeyCredential("ff"),None)
//   let user_datum = tx.NoDatum
//   let outs = 
//     [ Output(swap_addr,value.zero(),user_datum,None)
//     , Output(user_addr,value.zero(),user_datum,None)
//     ]
//   valid_update_outputs(outs,swap_addr)
// }

// test valid_update_outputs_tests4() {
//   let swap_addr = Address(VerificationKeyCredential("00"),None)
//   let swap_datum = InlineDatum(SwapPrice(unsafe_ratio(-1,10)))
//   let outs = [Output(swap_addr,value.zero(),swap_datum,None)]
//   valid_update_outputs(outs,swap_addr)
// }

// test valid_update_outputs_tests5() {
//   let swap_addr = Address(VerificationKeyCredential("00"),None)
//   let swap_datum = InlineDatum(BeaconSymbol("00"))
//   let outs = [Output(swap_addr,value.zero(),swap_datum,None)]
//   valid_update_outputs(outs,swap_addr)
// }

fn staking_credential_approves(swap_addr: Address, tx_info: Transaction) -> Bool {
  let Address(_,staking_cred) = swap_addr
  let approved = when staking_cred is {
    // This is to prevent permanent locking of funds. Beacons can never be minted to an address
    // without a valid staking credential.
    None -> True

    // If the address uses a staking pubkey, it must have signed the tx.
    Some(Inline(VerificationKeyCredential(skey))) -> {
      let Transaction(_,_,_,_,_,_,_,_,extra_signatories,_,_,_) = tx_info
      has(extra_signatories,skey)
    }

    // If the address uses a staking script, it must have been executed in the tx.
    Some(svh) -> {
      let Transaction(_,_,_,_,_,_,withdrawals,_,_,_,_,_) = tx_info
      has_key(withdrawals,svh)
    }
  }

  if approved {
    approved 
  } else {
    error @"Staking credential did not approve"
  }
}

test staking_credential_approves_test1() {
  let addr = Address(ScriptCredential("00"),Some(Inline(VerificationKeyCredential("00"))))
  let key = "00"
  let tx = 
    Transaction([],[],[],value.zero(),value.zero(),[],dict.new()
               ,interval.empty(),[key],dict.new(),dict.new(),tx.TransactionId("00"))
  staking_credential_approves(addr,tx)
}

test staking_credential_approves_test2() {
  let addr = Address(ScriptCredential("00"),Some(Inline(ScriptCredential("00"))))
  let script = Inline(ScriptCredential("00"))
  let tx = 
    Transaction([],[],[],value.zero(),value.zero(),[],dict.insert(dict.new(),script,0,fn(_,_) {Less})
               ,interval.empty(),[],dict.new(),dict.new(),tx.TransactionId("00"))
  staking_credential_approves(addr,tx)
}

test staking_credential_approves_test3() {
  let addr = Address(ScriptCredential("00"),None)
  let tx = 
    Transaction([],[],[],value.zero(),value.zero(),[],dict.new()
               ,interval.empty(),[],dict.new(),dict.new(),tx.TransactionId("00"))
  staking_credential_approves(addr,tx)
}

// Tests 4,5 should both throw an error and therefore always fail.
// test staking_credential_approves_test4() {
//   let addr = Address(ScriptCredential("00"),Some(Inline(VerificationKeyCredential("00"))))
//   let key = "01"
//   let tx = 
//     Transaction([],[],[],value.zero(),value.zero(),[],dict.new()
//                ,interval.empty(),[key],dict.new(),dict.new(),tx.TransactionId("00"))
//   staking_credential_approves(addr,tx)
// }

// test staking_credential_approves_test5() {
//   let addr = Address(ScriptCredential("00"),Some(Inline(ScriptCredential("00"))))
//   let script = Inline(ScriptCredential("01"))
//   let tx = 
//     Transaction([],[],[],value.zero(),value.zero(),[],dict.insert(dict.new(),script,0,fn(_,_) {Less})
//                ,interval.empty(),[],dict.new(),dict.new(),tx.TransactionId("00"))
//   staking_credential_approves(addr,tx)
// }

fn total_input_value(ins: List<Input>) -> Value {
  let foo = fn (i: Input ,acc: Value) {
    let Input(_,Output(_,val,_,_)) = i
    value.add(acc,val)
  }
  list.foldr(ins,foo,value.zero())
}

test total_input_value_test1() {
  let addr = Address(VerificationKeyCredential("00"),None)
  let datum = tx.NoDatum
  let value1 = value.from_lovelace(1000000)
  let input1 = Input(tx.OutputReference(tx.TransactionId("00"),0),Output(addr,value1,datum,None))
  total_input_value([input1]) == value.from_lovelace(1000000)
}

test total_input_value_test2() {
  let addr = Address(VerificationKeyCredential("00"),None)
  let datum = tx.NoDatum
  let value1 = value.from_lovelace(1000000)
  let value2 = value.from_lovelace(4000000)
  let input1 = Input(tx.OutputReference(tx.TransactionId("00"),0),Output(addr,value1,datum,None))
  let input2 = Input(tx.OutputReference(tx.TransactionId("00"),1),Output(addr,value2,datum,None))
  total_input_value([input1,input2]) == value.from_lovelace(5000000)
}

fn all_beacons_burned(beacon_symbol: PolicyId, tx_info: Transaction) -> Bool {
  let Transaction(inputs,_,_,_,mint,_,_,_,_,_,_,_) = tx_info
  let ival = total_input_value(inputs)
  if value.quantity_of(ival,beacon_symbol,"") == -value.quantity_of(mint,beacon_symbol,"") {
    True
  } else {
    error @"Beacons not burned"
  }
}

test all_beacons_burned_test1() {
  let tx = 
    Transaction([],[],[],value.zero(),value.zero(),[],dict.new()
               ,interval.empty(),[],dict.new(),dict.new(),tx.TransactionId("00"))
  all_beacons_burned("00",tx)
}

// The total input value from this swap address.
// The Rational returned is the weighted avg price off all the inputs from this address.
fn swap_input_info(
  ins: List<Input>, 
  swap_addr: Address, 
  sym: PolicyId, 
  tn: AssetName
) -> (Value,Rational) {
  let foo = fn (i: Input, acc: (Value,Int,Rational)) {
    let Input(_,Output(addr,ival,d,_)) = i
    let (val,taken,wp) = acc
    // Check if the input is from this swap address.
    if addr == swap_addr {
      // If it is, update the total input value from this address and update the
      // weighted avg price of all the inputs.
      let itaken = value.quantity_of(ival,sym,tn)
      expect SwapPrice(price) = parse_datum(d) // All swap inputs must have a SwapPrice datum.
      let new_taken = itaken + taken // Update total offered asset taken.
      let new_weighted_price =
        ratio.add( ratio.multiply(unsafe_ratio(taken,new_taken),wp)
                 , ratio.multiply(unsafe_ratio(itaken,new_taken),price)
        )
      let new_val = value.add(val,ival) // Update total input value from this address.
      (new_val,new_taken,new_weighted_price)
    } else {
      acc // This output can be skipped.
    }
  }
  let (val,_,wp) = list.foldl(ins,foo,(value.zero(),0,ratio.zero()))
  (val,wp)
}

test swap_input_info_test1() {
  let addr = Address(VerificationKeyCredential("00"),None)
  let datum = InlineDatum(SwapPrice(unsafe_ratio(1,10)))
  let value1 = value.from_lovelace(1000000)
  let value2 = value.from_lovelace(4000000)
  let input1 = Input(tx.OutputReference(tx.TransactionId("00"),0),Output(addr,value1,datum,None))
  let input2 = Input(tx.OutputReference(tx.TransactionId("00"),1),Output(addr,value2,datum,None))
  swap_input_info([input1,input2],addr,#"",#"") == (value.from_lovelace(5000000),unsafe_ratio(1,10))
}

test swap_input_info_test2() {
  let addr = Address(VerificationKeyCredential("00"),None)
  let datum = InlineDatum(SwapPrice(unsafe_ratio(1,10)))
  let value1 = value.from_lovelace(1000000)
  let input1 = Input(tx.OutputReference(tx.TransactionId("00"),0),Output(addr,value1,datum,None))
  swap_input_info([input1],addr,#"",#"") == (value.from_lovelace(1000000),unsafe_ratio(1,10))
}

// This function subtracts the output value from the input value passed in. This gives a value
// representing the value flux for this address: negative values mean the address gained that asset
// while positive values mean the address lost that asset.
fn address_value_diff(
  outs: List<Output>, 
  ival: Value, 
  req_price: Option<Rational>, 
  swap_addr: Address
) -> Value {
  let foo = fn (o: Output, acc: Value) {
    let Output(addr,oval,d,_) = o
    // If the output is to the swap address.
    if addr == swap_addr {
      // Check that the datum is valid and has the weighted avg price of the inputs.
      if valid_swap_datum(req_price,parse_datum(d)) {
        // Subtract the output value from the accumulator.
        value.add(acc,value.negate(oval))
      } else {
        // This is not necessary since valid_swap_datum will error when not true but something
        // needed to go here. Having it error with a msg of where the error occurred seemed like
        // a good idea in case an unexpected execution occurs.
        error @"Swap ouptut datum is invalid" 
      }
    } else {
      acc // This input can be skipped.
    }
  }
  list.foldl(outs,foo,ival)
}

test address_value_diff_test1() {
  let addr = Address(VerificationKeyCredential("00"),None)
  let price = unsafe_ratio(1,10)
  let datum = InlineDatum(SwapPrice(unsafe_ratio(1,10)))
  let value1 = value.from_lovelace(1000000)
  let value2 = value.from_lovelace(4000000)
  let input_val = value.from_lovelace(6000000)
  let output1 = Output(addr,value1,datum,None)
  let output2 = Output(addr,value2,datum,None)
  address_value_diff([output1,output2],input_val,Some(price),addr) == value.from_lovelace(1000000)
}

test address_value_diff_test2() {
  let addr1 = Address(VerificationKeyCredential("00"),None)
  let addr2 = Address(VerificationKeyCredential("01"),None)
  let price = unsafe_ratio(1,10)
  let datum = InlineDatum(SwapPrice(unsafe_ratio(1,10)))
  let value1 = value.from_lovelace(1000000)
  let value2 = value.from_lovelace(4000000)
  let input_val = value.from_lovelace(6000000)
  let output1 = Output(addr1,value1,datum,None)
  let output2 = Output(addr2,value2,tx.NoDatum,None)
  address_value_diff([output1,output2],input_val,Some(price),addr1) == value.from_lovelace(5000000)
}

test address_value_diff_test3() {
  let addr1 = Address(VerificationKeyCredential("00"),None)
  let addr2 = Address(VerificationKeyCredential("01"),None)
  let datum = InlineDatum(SwapPrice(unsafe_ratio(3,10)))
  let value1 = value.from_lovelace(1000000)
  let value2 = value.from_lovelace(4000000)
  let input_val = value.from_lovelace(6000000)
  let output1 = Output(addr1,value1,datum,None)
  let output2 = Output(addr2,value2,tx.NoDatum,None)
  address_value_diff([output1,output2],input_val,None,addr1) == value.from_lovelace(5000000)
}

fn only_offered_leaves(val: Value, offered_id: PolicyId, offered_name: AssetName) -> Bool {
  let xs = value.flatten(val) // Zero diff amounts removed.
  let foo = fn (i: (PolicyId,AssetName,Int), acc: Bool) {
    let (cn,tn,n) = i
    if n > 0 {
      acc && cn == offered_id && tn == offered_name
    } else {
      acc
    }
  }
  list.foldl(xs,foo,True)
}

test only_offered_leaves_test1() {
  let v = value.from_lovelace(1000000)
  only_offered_leaves(v,"","")
}

test only_offered_leaves_test2() {
  let v = value.from_lovelace(1000000)
  !only_offered_leaves(v,"00","")
}

// Check if the proper amount of the asked asset is given for the offered asset being taken. Also
// checks:
// 1) Only the offered asset leaves the address.
// 2) All outputs to the swap address have the weighted avg price for the inline SwapPrice datum.
// It will error unless True.
fn valid_swap(swap_addr: Address, tx_info: Transaction, config: SwapConfig) -> Bool {
  let Transaction(inputs,_,outputs,..) = tx_info
  let SwapConfig((offer_sym,offer_name),(asked_sym,asked_name)) = config
  let (script_input_value,weighted_price) = swap_input_info(inputs,swap_addr,offer_sym,offer_name)
  let script_value_diff = 
    address_value_diff(outputs,script_input_value,Some(weighted_price),swap_addr)
  let asked_given = value.quantity_of(script_value_diff,asked_sym,asked_name)
                 |> fn(x) {-x} // Must negate it since value received will be negative.
                 |> from_integer
  let offered_taken = value.quantity_of(script_value_diff,offer_sym,offer_name)
                   |> from_integer
  let enough_asked_given = ratio.multiply(offered_taken,weighted_price)
                        |> ratio.lte(_,asked_given)
  if !only_offered_leaves(script_value_diff,offer_sym,offer_name) {
    error @"Only the offered asset can leave swap address"
  } else if !enough_asked_given {
    error @"Fail: offered_taken * weighted_avg_price <= asked_given"
  } else {
    True
  }
}

test valid_swap_test1() {
  let addr = Address(ScriptCredential("00"),None)
  let datum = InlineDatum(SwapPrice(unsafe_ratio(1,1000000)))
  let input_val = value.from_lovelace(5000000)
  let output_val = value.from_asset(#"c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d",#"",5)
  let input = Input(tx.OutputReference(tx.TransactionId("00"),0),Output(addr,input_val,datum,None))
  let output = Output(addr,output_val,datum,None)
  let cfg = SwapConfig((#"",#""),(#"c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d",#""))
  let tx = 
    Transaction([input],[],[output],value.zero(),value.zero(),[],dict.new()
               ,interval.empty(),[],dict.new(),dict.new(),tx.TransactionId("00"))
  valid_swap(addr,tx,cfg)
}

// This is a universal validator that is parameterized by the trading pair. This dApp is only meant
// for trading currencies. While NFTs can also be trading, it is not as ergonomic for that niche.
//
// This validator enforces that whenever a beacon UTxO is consumed, all beacons must be burned. This
// is to ensure that beacons can never be found in the wrong location or paired with the wrong datum.
validator(config: SwapConfig) {
  fn spend(datum: SwapDatum, redeemer: SwapRedeemer, ctx: ScriptContext) -> Bool {
    let ScriptContext(tx_info,purpose) = ctx
    expect tx.Spend(out_ref) = purpose
    expect Some(Input(_, Output(addr, _, _, _))) = tx_info |> tx.find_input(out_ref)
    when redeemer is {
      Close -> {
        // Requirements:
        // 1) All beacons in inputs must be burned. Beacons will always be stored with a
        //    BeaconSymbol datum.
        // 2) Staking credential must signal approval.
        when datum is {
          SwapPrice(_) -> staking_credential_approves(addr,tx_info)
          BeaconSymbol(sym) -> 
            staking_credential_approves(addr,tx_info) &&
            all_beacons_burned(sym,tx_info)
        }
      }
      Update -> {
        // Requirements:
        // 1) The datum must be a SwapPrice datum. Beacon utxos can only be consumed using
        //    the Close redeemer.
        // 2) All outputs to this address must contain a SwapPrice inline datum.
        // 3) Staking credential must signal approval.
        when datum is {
          BeaconSymbol(_) -> error @"Beacon UTxOs cannot be updated, only closed."
          SwapPrice(_) -> 
            valid_update_outputs(tx_info.outputs,addr) && 
            staking_credential_approves(addr,tx_info)
        }
      }
      Swap -> {
        // Requirements:
        // 1) The datum must be a SwapPrice datum. This ensures that the beacon utxo is not
        //    included in the swap.
        // 2) All inputs prices used in the swap must be > 0.
        // 3) All outputs to address contain proper datum.
        // 4) Offered asset taken * price <= asked asset given
        // 5) Only the offered asset leaves the address.
        when datum is {
          SwapPrice(price) -> {
            if ratio.gt(price, ratio.zero()) {
              valid_swap(addr,tx_info,config)
            } else {
              error @"All input prices must be > 0"
            }
          }
          BeaconSymbol(_) -> error @"All swap inputs must have a SwapPrice datum"
        }
      }
    }
  }
}

// This function either throws an error or returns True.
fn mint_check(redeemer: BeaconRedeemer, mint: Dict<AssetName,Int>) -> Bool {
  when redeemer is {
    MintBeacon -> {
      when dict.to_list(mint) is {
        [(beacon_name,mint_number)] -> {
          if beacon_name == "" && mint_number == 1 {
            True
          } else {
            error @"Only one beacon can be minted and it must have the empty token name"
          }
        }
        _ -> error @"Only one beacon can be minted and it must have the empty token name"
      }
      
    }
    BurnBeacon -> {
      if dict.fold(mint,fn (_,v,r) {r && v < 0},True) {
        True
      } else {
        error @"This redeemer can only be used to burn"
      }
    }
  }
}

test mint_check_test1() {
  let mint = dict.new() |> dict.insert(_,"",1,fn(_,_) {Less} )
  mint_check(MintBeacon,mint)
}

test mint_check_test2() {
  let mint = dict.new() |> dict.insert(_,"",-1,fn(_,_) {Less} )
  mint_check(BurnBeacon,mint)
}

// Test 3,4,5 will throw errors and therefore will always fail.
// test mint_check_test3() {
//   let mint = dict.new() |> dict.insert(_,"",10,fn(_,_) {Less} )
//   !mint_check(MintBeacon,mint)
// }

// test mint_check_test4() {
//   let mint = dict.new() 
//         |> dict.insert(_,"",1,fn(_,_) {Less} )
//         |> dict.insert(_,"00",1,fn(_,_) {Less} )
//   !mint_check(MintBeacon,mint)
// }

// test mint_check_test5() {
//   let mint = dict.new() |> dict.insert(_,"",1,fn(_,_) {Less} )
//   mint_check(BurnBeacon,mint)
// }

// Check if the beacon is minted to the proper location and stored with the proper value and datum.
fn destination_check(
  dapp_hash: Hash<Blake2b_224,Script>, 
  sym: PolicyId,
  tx_info: Transaction
) -> Bool {
  let Transaction(_,_,outputs,..) = tx_info
  let foo = fn (out: Output, acc: Bool) {
    let Output(Address(pay,stake),val,d,_) = out
    if value.quantity_of(val,sym,"") > 0 {
      when (pay,value.lovelace_of(val)) is {
        (ScriptCredential(vh),n) -> {
          if vh == dapp_hash && is_some(stake) && n >= 20000000 {
            expect BeaconSymbol(datum_sym) = parse_datum(d)
            if datum_sym == sym {
              acc && True
            } else {
              error @"Beacon not stored with proper datum"
            }
          } else if vh != dapp_hash {
            let msg = string.concat(@"Beacon must go to a ",app_name) 
                   |> string.concat(_,@" dApp address")
            error msg
          } else if n < 20000000 {
            error @"Beacon stored without minimum deposit"
          } else {
            error @"Beacon must go to a dApp address with a staking credential"
          }
        }
        _ -> error @"Beacon must go to a dApp address and be stored with a BeaconSymbol datum"
      }
    } else {
      acc
    }
  }
  list.foldl(outputs,foo,True)
}

validator(dapp_hash: Hash<Blake2b_224, Script>) {
  fn mint(redeemer: BeaconRedeemer, ctx: ScriptContext) -> Bool {
    let ScriptContext(tx_info,purpose) = ctx
    expect tx.Mint(policy_id) = purpose
    when redeemer is {
      MintBeacon -> {
        // Requirements:
        // 1) Only one beacon can be minted and it must use the empty token name.
        // 2) The beacon must go to an address protected by the dApp validator script.
        // 3) The beacon must go to an address using a valid staking credential.
        // 4) The beacon must be stored with a minimum deposit.
        // 5) The beacon must be stored with a BeaconSymbol datum with this policy id.
        let Transaction(_,_,_,_,mint,..) = tx_info
        mint_check(redeemer,value.tokens(mint,policy_id)) &&
        destination_check(dapp_hash,policy_id,tx_info)
      }
      BurnBeacon -> {
        // Always allowed as long as this redeemer is only used to burn.
        let Transaction(_,_,_,_,mint,..) = tx_info
        mint_check(redeemer,value.tokens(mint,policy_id))
      }
    }
  }
}

// test serialise_test1() {
//   let config = SwapConfig((#"",#""),(#"c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d",#""))
//   cbor.serialise(config) == #""
// }

// test serialise_test2() {
//   let config = SwapConfig((#"",#""),(#"c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d",#""))
//   cbor.diagnostic(config) == @""
// }

// test serialise_test3() {
//   cbor.serialise("testing") == #""
// }

// test serialise_test4() {
//   cbor.serialise(#"a663f735d8803bf784286f59870bc5be44f6833cfa6aa77fc670ea81") == #""
// }