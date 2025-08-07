#!../target/debug/laxma-rs

import ../lib/builtins
import ../lib/math

type Unit -> Celsius | Fahrenheit | Kelvin

fn string_to_unit str:String =
    "C" -> Unit::Celsius()
    "Celsius" -> Unit::Celsius()
    "F" -> Unit::Fahrenheit()
    "Fahrenheit" -> Unit::Fahrenheit()
    "K" -> Unit::Kelvin()
    "Kelvin" -> Unit::Kelvin()

fn convert value:Float from:Unit to:Unit =
    _, Celsius, Fahrenheit -> +(32.0 *(value 1.8))
    _, Fahrenheit, Celsius -> *(/(1.0 1.8) -(value 32.0))
    _, Celsius, Kelvin -> +(value 273.15)
    _, Kelvin, Celsius -> -(value 273.15)

# Example: main(23.3333 "Celsius" "Fahrenheit") = 74
fn main value:Float from:String to:String ->
    print(convert(value string_to_unit(from) string_to_unit(to)))
