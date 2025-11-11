//! Testing utilities for JavaScript transformation projects.
//!
//! This module provides helpers for testing JavaScript transformations using the SWC parser.
//! It's designed to be used both internally and by downstream projects that build on top of
//! this library (like Weevy, Jsaw, Jsxx, DumbJS, etc.).
//!
//! # Examples
//!
//! ## Basic Usage
//!
//! Load and parse a JavaScript module for testing:
//!
//! ```rust
//! # #[cfg(feature = "test")]
//! # {
//! use portal_solutions_swibb::test::*;
//! use swc_common::sync::Lrc;
//! use swc_common::SourceMap;
//!
//! let source_map = Lrc::new(SourceMap::default());
//! let module = test_load(&source_map, "my_test", "const x = 42;");
//! // Now you can apply transformations to the module
//! # }
//! ```
//!
//! ## Using the `map_load!` Macro
//!
//! The `map_load!` macro simplifies test setup by creating the SourceMap and loading code:
//!
//! ```rust
//! # #[cfg(feature = "test")]
//! # {
//! use portal_solutions_swibb::map_load;
//!
//! map_load!("test_name", "const x = 1 + 1;" => |source_map, mut module| {
//!     // Access the source_map and module here
//!     // Apply transformations and assertions
//! });
//! # }
//! ```
//!
//! ## Creating a Simple Test
//!
//! Use the `simple_module_test!` macro to create a complete test function:
//!
//! ```rust
//! # #[cfg(feature = "test")]
//! # {
//! use portal_solutions_swibb::simple_module_test;
//! use swc_ecma_visit::VisitMutWith;
//!
//! simple_module_test!(test_constant_propagation [
//!     "const x = 1; const y = x + 1;"
//! ] => |_source_map, mut module| {
//!     // Apply your transformation
//!     module.visit_mut_with(&mut portal_solutions_swibb::CondFolding::default());
//!     // Add assertions here
//! });
//! # }
//! ```
//!
//! ## Testing Transformations
//!
//! Example of testing a transformation with assertions:
//!
//! ```rust
//! # #[cfg(feature = "test")]
//! # {
//! use portal_solutions_swibb::{simple_module_test, ConstCollector};
//! use swc_ecma_visit::VisitMutWith;
//!
//! simple_module_test!(collect_consts [
//!     "const PI = 3.14; const TAU = PI * 2;"
//! ] => |_sm, mut module| {
//!     let mut collector = ConstCollector::default();
//!     module.visit_mut_with(&mut collector);
//!     assert!(collector.map.len() >= 1, "Should collect at least one constant");
//! });
//! # }
//! ```

use crate::*;
use sha3::Digest;
use swc_ecma_ast::Module;

/// Loads and parses JavaScript code into a SWC AST Module for testing.
///
/// This function creates a source file from the provided JavaScript code string,
/// parses it using SWC's JavaScript parser, and applies resolver transforms to
/// set up proper scoping. It's the foundation for all testing utilities in this module.
///
/// # Arguments
///
/// * `cm` - A reference-counted `SourceMap` used to track source positions
/// * `testname` - A descriptive name for this test case (used in error messages and file naming)
/// * `fm` - The JavaScript source code as a string
///
/// # Returns
///
/// A parsed and resolved `Module` AST ready for transformation and testing.
///
/// # Panics
///
/// This function will panic if:
/// - The JavaScript code cannot be parsed
/// - There are unrecoverable syntax errors in the input
///
/// # Examples
///
/// ```rust
/// # #[cfg(feature = "test")]
/// # {
/// use portal_solutions_swibb::test::test_load;
/// use swc_common::sync::Lrc;
/// use swc_common::SourceMap;
///
/// let cm = Lrc::new(SourceMap::default());
/// let module = test_load(&cm, "simple_test", "const greeting = 'Hello';");
/// // module is now ready for transformations
/// # }
/// ```
pub fn test_load(cm: &Lrc<SourceMap>, testname: &str, fm: &str) -> Module {
    let fm = cm.new_source_file(
        Lrc::new(FileName::Custom(format!(
            "testcase {testname} with hash {}",
            hex::encode(sha3::Sha3_256::digest(fm))
        ))),
        fm.to_owned(),
    );
    // let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));
    // Real usage
    // let fm = cm
    //     .load_file(Path::new("test.js"))
    //     .expect("failed to load test.js");
    // let fm = cm.new_source_file(
    //     FileName::Custom("test.js".into()).into(),
    //     "function foo() {}".into(),
    // );
    let lexer = Lexer::new(
        // We want to parse ecmascript
        Syntax::Es(Default::default()),
        // EsVersion defaults to es5
        Default::default(),
        StringInput::from(&*fm),
        None,
    );
    let mut parser = Parser::new_from(lexer);
    for e in parser.take_errors() {
        e.into_diagnostic(&handler).emit();
    }
    let mut module = parser
        .parse_module()
        .map_err(|mut e| {
            // Unrecoverable fatal error occurred
            e.into_diagnostic(&handler).emit()
        })
        .expect("failed to parser module");
    module.visit_mut_with(&mut swc_ecma_transforms_base::resolver(
        Mark::root(),
        Mark::new(),
        false,
    ));
    return module;
}

/// Re-export of `swc_common` for use in macros.
///
/// This is hidden from documentation as it's an implementation detail.
#[doc(hidden)]
pub use swc_common;

/// Loads JavaScript code and provides access to both the SourceMap and parsed Module.
///
/// This macro creates a `SourceMap`, loads and parses the JavaScript code using `test_load`,
/// and then executes the provided closure with both the source map and module as arguments.
///
/// # Syntax
///
/// ```text
/// map_load!("test_name", "javascript_code" => |source_map_pattern, module_pattern| {
///     // your code here
/// });
/// ```
///
/// # Arguments
///
/// * `$name` - A string literal naming this test case
/// * `$data` - A string literal containing the JavaScript code to parse
/// * `$a` - Pattern for binding the SourceMap (typically `_sm` or `source_map`)
/// * `$b` - Pattern for binding the parsed Module (typically `module` or `mut module`)
/// * `$e` - The expression/block to execute with the bindings
///
/// # Examples
///
/// ```rust
/// # #[cfg(feature = "test")]
/// # {
/// use portal_solutions_swibb::map_load;
/// use swc_ecma_visit::VisitMutWith;
///
/// map_load!("transform_test", "const x = 1 + 2;" => |_sm, mut module| {
///     // Apply transformations
///     module.visit_mut_with(&mut portal_solutions_swibb::CondFolding::default());
///     // Access both source_map (as _sm) and module
/// });
/// # }
/// ```
#[macro_export]
macro_rules! map_load {
    ($name:expr, $data:expr => |$a:pat_param,$b:pat_param|$e:expr) => {
        match $crate::test::swc_common::sync::Lrc::new(
            $crate::test::swc_common::SourceMap::default(),
        ) {
            x => match $crate::test::test_load(&x, $name, $data) {
                $b => match x {
                    $a => $e,
                },
            },
        }
    };
}

/// Creates a complete test function that loads JavaScript code and runs transformations.
///
/// This macro combines test function creation with the `map_load!` macro to provide
/// a convenient way to write tests. It's particularly useful for creating multiple
/// test cases that follow the same pattern.
///
/// # Syntax
///
/// ```text
/// simple_module_test!(test_function_name [
///     "javascript_code"
/// ] => |source_map_pattern, module_pattern| {
///     // test body
/// });
/// ```
///
/// # Arguments
///
/// * `$name` - An identifier for the test function name (must be a valid Rust identifier)
/// * `$data` - A string literal containing the JavaScript code to parse
/// * `$a` - Pattern for binding the SourceMap (typically `_sm`)
/// * `$b` - Pattern for binding the parsed Module (typically `mut module`)
/// * `$e` - The test body expression/block
///
/// # Examples
///
/// ## Basic Transformation Test
///
/// ```rust
/// # #[cfg(feature = "test")]
/// # {
/// use portal_solutions_swibb::{simple_module_test, CondFolding};
/// use swc_ecma_visit::VisitMutWith;
///
/// simple_module_test!(test_conditional_folding [
///     "const result = true ? 1 : 2;"
/// ] => |_sm, mut module| {
///     let mut folder = CondFolding::default();
///     module.visit_mut_with(&mut folder);
///     // Add assertions about the transformed module
/// });
/// # }
/// ```
///
/// ## Test with Constant Collection
///
/// ```rust
/// # #[cfg(feature = "test")]
/// # {
/// use portal_solutions_swibb::{simple_module_test, ConstCollector};
/// use swc_ecma_visit::VisitMutWith;
///
/// simple_module_test!(test_const_collection [
///     "const x = 42; const y = x * 2;"
/// ] => |_sm, mut module| {
///     let mut collector = ConstCollector::default();
///     module.visit_mut_with(&mut collector);
///     assert!(!collector.map.is_empty());
/// });
/// # }
/// ```
///
/// ## Test with Inlining
///
/// ```rust
/// # #[cfg(feature = "test")]
/// # {
/// use portal_solutions_swibb::{simple_module_test, inline::CollectConstsAndInline, inline::InlineFlags};
/// use swc_ecma_visit::VisitMutWith;
///
/// simple_module_test!(test_inlining [
///     "const NUM = 5; const DOUBLE = NUM * 2; console.log(DOUBLE);"
/// ] => |_sm, mut module| {
///     module.collet_consts_and_linine(InlineFlags::default());
///     // Verify constants were inlined
/// });
/// # }
/// ```
#[macro_export]
macro_rules! simple_module_test {
    ($name:ident [$data:expr] => |$a:pat_param,$b:pat_param|$e:expr) => {
      #[test]
      fn $name(){
        $crate::test::map_load!(stringify!($name),$data => |$a,$b|$e);
      }
    };
}
