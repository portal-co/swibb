# portal-solutions-swibb

Utilities for rewriting and transforming JavaScript code using the [SWC](https://swc.rs/) compiler infrastructure.

[![Crates.io](https://img.shields.io/crates/v/portal-solutions-swibb.svg)](https://crates.io/crates/portal-solutions-swibb)
[![License: MPL-2.0](https://img.shields.io/badge/License-MPL%202.0-blue.svg)](https://opensource.org/licenses/MPL-2.0)

## Overview

`portal-solutions-swibb` provides a collection of AST transformers and utilities for JavaScript code manipulation. It's designed to support tools like Weevy, Jsaw, Jsxx, DumbJS, and other JavaScript processing tools that need to perform advanced code transformations.

## Features

- **Conditional Expression Folding**: Normalize and simplify ternary expressions (`? :`)
- **Constant Collection and Inlining**: Track constant declarations and inline their values
- **Arrow Function Inflation**: Convert arrow functions to regular functions while preserving lexical `this`
- **With Statement Transformation**: Remove JavaScript `with` statements by making scope lookups explicit
- **Variable Declaration Optimization**: Convert `var` declarations to `const` when variables are never reassigned
- **Hot Module Replacement (HMR)**: Utilities for working with HMR in module bundlers
- **Scope Analysis**: Determine which variables are captured from outer scopes
- **Identifier Mangling**: Rename identifiers for obfuscation or scope disambiguation

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
portal-solutions-swibb = "0.5.0"
```

For testing utilities, enable the `test` feature:

```toml
[dependencies]
portal-solutions-swibb = { version = "0.5.0", features = ["test"] }
```

## Quick Start

### Folding Conditional Expressions

```rust
use portal_solutions_swibb::CondFolding;
use swc_ecma_visit::VisitMutWith;

// Parse your JavaScript code into a module
let mut module = /* ... */;

// Apply conditional folding
let mut folder = CondFolding::default();
module.visit_mut_with(&mut folder);

// The module now has simplified conditional expressions
```

### Collecting and Inlining Constants

```rust
use portal_solutions_swibb::inline::CollectConstsAndInline;
use portal_solutions_swibb::inline::InlineFlags;
use swc_ecma_visit::VisitMutWith;

let mut module = /* ... */;

// Collect constants and inline them in one pass
module.collet_consts_and_linine(InlineFlags {
    global_this_inlining: true,
    global_fetching: false,
    canon: Some("globalThis".into()),
});
```

### Transforming Arrow Functions

```rust
use portal_solutions_swibb::inflate::Inflate;
use swc_ecma_visit::VisitMutWith;

let mut module = /* ... */;

// Convert arrow functions to regular functions
let mut inflater = Inflate::default();
module.visit_mut_with(&mut inflater);
```

## Testing Utilities

The crate includes comprehensive testing utilities (available with the `test` feature):

```rust
use portal_solutions_swibb::simple_module_test;
use portal_solutions_swibb::CondFolding;
use swc_ecma_visit::VisitMutWith;

simple_module_test!(test_conditional_folding [
    "const x = true ? 1 : 2;"
] => |_sm, mut module| {
    let mut folder = CondFolding::default();
    module.visit_mut_with(&mut folder);
    // Add your assertions here
});
```

See the [test module documentation](src/test.rs) for more examples.

## Module Overview

### Core Modules

- **`consts`**: Constant collection, variable declaration splitting, and const optimization
- **`folding`**: Conditional expression normalization and simplification
- **`inflate`**: Arrow function to regular function transformation
- **`inline`**: Constant inlining and global object canonicalization
- **`module`**: Hot module replacement utilities and import management
- **`scope`**: Scope analysis and free variable detection
- **`wither`**: JavaScript `with` statement transformation

### Main Library (`lib.rs`)

The main library module provides:

- Purity and idempotency traits for analyzing expressions
- Syntax context to mark conversion utilities
- Code cleansing for removing unnecessary patterns
- Identifier renaming strategies (mangling, garbling)
- Re-resolution utilities for rebuilding scope information

## Use Cases

### JavaScript Minification

Use constant inlining, conditional folding, and variable optimization to reduce code size:

```rust
use portal_solutions_swibb::{CondFolding, inline::CollectConstsAndInline, inline::InlineFlags};
use swc_ecma_visit::VisitMutWith;

let mut module = /* ... */;

// Inline constants
module.collet_consts_and_linine(InlineFlags::default());

// Fold conditional expressions
module.visit_mut_with(&mut CondFolding::default());
```

### Code Obfuscation

Combine identifier renaming with transformations to make code harder to read:

```rust
use portal_solutions_swibb::Garbler;
use rand::thread_rng;
use swc_ecma_transforms_base::rename::renamer;

let garbler = Garbler::new(thread_rng());
module.visit_mut_with(&mut renamer(Default::default(), &garbler));
```

### Hot Module Replacement

Generate HMR-compatible code for development workflows:

```rust
use portal_solutions_swibb::module::ImportManifest;

let mut manifest = ImportManifest::default();
let id = manifest.get("./my-module".into(), "default".into());

// Generate import statements with HMR support
let items = manifest.render(span, Some(&["hot"]));
```

### Legacy Code Modernization

Transform `with` statements and other legacy constructs:

```rust
use portal_solutions_swibb::wither::Wither;

let mut wither = Wither {
    with_stack: vec![],
    ident_prefix: "with".into(),
};
module.visit_mut_with(&mut wither);
```

## Architecture

This library is built on [SWC](https://swc.rs/), a fast JavaScript/TypeScript compiler written in Rust. It uses SWC's visitor pattern to traverse and transform Abstract Syntax Trees (ASTs).

All transformers implement SWC's `VisitMut` trait, allowing them to be composed and applied in sequence.

## API Stability

The crate uses `#[non_exhaustive]` on many structs to allow for future additions without breaking changes. The public API is designed to be stable, but internal implementations may change between minor versions.

## Contributing

Contributions are welcome! This library is designed to be extended with new transformations and utilities.

When adding new transformations:
1. Implement the `VisitMut` trait from `swc_ecma_visit`
2. Add comprehensive documentation with examples
3. Consider adding test utilities if the transformation is commonly used downstream

## License

This project is licensed under the Mozilla Public License 2.0 (MPL-2.0). See the LICENSE file for details.

## Related Projects

- [SWC](https://swc.rs/) - The underlying compiler infrastructure
- [Weevy](https://github.com/portal-co/weevy) - WebAssembly runtime for JavaScript
- [Jsaw](https://github.com/portal-co/jsaw) - JavaScript to WebAssembly compiler
- [Jsxx](https://github.com/portal-co/jsxx) - JavaScript transformation toolkit
- [DumbJS](https://github.com/portal-co/dumbjs) - JavaScript simplification tool

## Support

For bugs, feature requests, or questions:
- Open an issue on [GitHub](https://github.com/portal-co/swibb/issues)
- Check existing issues for similar problems

## Acknowledgments

Built with [SWC](https://swc.rs/), an extremely fast JavaScript/TypeScript compiler.
