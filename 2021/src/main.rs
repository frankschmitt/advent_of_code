// we use the "cute" crate in a01, but due to Rust limitations, it has to be included at the crate root
#[macro_use(c)]
extern crate cute;

mod helpers;
mod a01_sonar_sweep;
mod a02_dive;
mod a03_binary_diagnostic;
mod a05_hydrothermal_venture;
mod a06_lanternfish;
mod a07_the_treachery_of_whales;
mod a09_smoke_basin;
mod a10_syntax_scoring;

fn main() {
    //a01_sonar_sweep::solve();
    //a02_dive::solve();
    //a03_binary_diagnostic::solve();
    //a05_hydrothermal_venture::solve();
    //a06_lanternfish::solve();
    //a07_the_treachery_of_whales::solve();
    a09_smoke_basin::solve();
    //a10_syntax_scoring::solve();
}
