#[macro_export]
macro_rules! debugln {
    ($( $args:expr ),*) => { 
        #[cfg(debug_assertions)]
        println!( $( $args ),* ); 
    }
}