#[macro_use(c)]
extern crate cute;
extern crate regex;

mod helpers;
mod a01_report_repair;
mod a02_password_philosophy;
mod a03_tobbogan_trajectory;
mod a04_passport_processing;
mod a09_encoding_error;

// use crate::a03_tobbogan_trajectory;
fn main() {
    a01_report_repair::solve();
    a02_password_philosophy::solve();
    a03_tobbogan_trajectory::solve();
    a04_passport_processing::solve();
    a09_encoding_error::solve();
}
