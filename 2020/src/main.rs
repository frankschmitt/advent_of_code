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
mod a09_encoding_error;

// use crate::a03_tobbogan_trajectory;
fn main() {
    a01_report_repair::solve();
    a02_password_philosophy::solve();
    a03_tobbogan_trajectory::solve();
    a04_passport_processing::solve();
    a05_binary_boarding::solve();
    a06_custom_customs::solve();
    a09_encoding_error::solve();
}
