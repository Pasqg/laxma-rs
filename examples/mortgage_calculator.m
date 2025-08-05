#!../target/debug/laxma-rs

/load ../lib/builtins
/load ../lib/math

fn to_pct x:Float -> /(x 100.0)

# Calculates the monthly repayment of a mortgage
fn mortgage price:Float years:Float interest:Float ->
    with
        rate = to_pct(interest)
        one_year_df = exp(*(-1.0 *(rate years)))
        yearly_repayment = /(*(rate price) -(1.0 one_year_df))
        /(yearly_repayment 12.0)

fn main price:Float years:Float interest:Float ->
    print(mortgage(price years interest))