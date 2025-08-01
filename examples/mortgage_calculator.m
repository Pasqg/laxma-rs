#!../target/debug/laxma-rs

/load ../lib/builtins

fn to_pct x:Float -> fdiv(x 100.0)

# Calculates the monthly repayment of a mortgage
fn mortgage price:Float years:Float interest:Float ->
    with
        rate = to_pct(interest)
        one_year_df = exp(fmul(-1.0 fmul(rate years)))
        yearly_repayment = fdiv(fmul(rate price) fsub(1.0 one_year_df))
        fdiv(yearly_repayment 12.0)

fn main price:Float years:Float interest:Float ->
    print(mortgage(price years interest))