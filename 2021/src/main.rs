// we use the "cute" crate in a01, but due to Rust limitations, it has to be included at the crate root
#[macro_use(c)]
extern crate cute;

mod helpers;
mod a01_sonar_sweep;
mod a02_dive;
mod a03_binary_diagnostic;
mod a04_giant_squid;
mod a05_hydrothermal_venture;
mod a06_lanternfish;
mod a07_the_treachery_of_whales;
mod a08_seven_segment_search;
mod a09_smoke_basin;
mod a10_syntax_scoring;
mod a12_passage_pathing;
mod a13_transparent_origami;
mod a14_extended_polymerization;

fn main() {
    //a01_sonar_sweep::solve();
    //a02_dive::solve();
    //a03_binary_diagnostic::solve();
    //a04_giant_squid::solve();
    //a05_hydrothermal_venture::solve();
    //a06_lanternfish::solve();
    //a07_the_treachery_of_whales::solve();
    //a08_seven_segment_search::solve();
    //a09_smoke_basin::solve();
    //a10_syntax_scoring::solve();
    //a12_passage_pathing::solve();
    //a13_transparent_origami::solve();
    a14_extended_polymerization::solve();
}
