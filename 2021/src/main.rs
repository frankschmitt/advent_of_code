// we use the "cute" crate in a01, but due to Rust limitations, it has to be included at the crate root
#[macro_use(c)]
extern crate cute;

mod helpers;
mod a01_sonar_sweep;
mod a02_dive;
mod a03_binary_diagnostic;

fn main() {
    a01_sonar_sweep::solve();
    a02_dive::solve();
    a03_binary_diagnostic::solve();
}