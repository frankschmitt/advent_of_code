#[macro_use(c)]
extern crate cute;
extern crate regex;

mod helpers;
mod a01_report_repair;
mod a02_password_philosophy;
mod a03_tobbogan_trajectory;
mod a04_passport_processing;
mod a05_binary_boarding;
mod a06_custom_customs;
mod a07_handy_haversacks;
mod a08_handheld_halting;
mod a09_encoding_error;
mod a10_adapter_array;
mod a11_seating_system;

// use crate::a03_tobbogan_trajectory;
fn main() {
    a01_report_repair::solve();
    a02_password_philosophy::solve();
    a03_tobbogan_trajectory::solve();
    a04_passport_processing::solve();
    a05_binary_boarding::solve();
    a06_custom_customs::solve();
    a07_handy_haversacks::solve();
    a08_handheld_halting::solve();
    a09_encoding_error::solve();
    a10_adapter_array::solve();
    a11_seating_system::solve();
}
