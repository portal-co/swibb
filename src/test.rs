use sha3::Digest;
use swc_ecma_ast::Module;

use crate::*;
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

    let module = parser
        .parse_module()
        .map_err(|mut e| {
            // Unrecoverable fatal error occurred
            e.into_diagnostic(&handler).emit()
        })
        .expect("failed to parser module");
    return module;
}
#[doc(hidden)]
pub use swc_common;
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
#[macro_export]
macro_rules! simple_module_test {
    ($name:ident [$data:expr] => |$a:pat_param,$b:pat_param|$e:expr) => {
      #[test]
      fn $name(){
        $crate::test::map_load!(stringify!($name),$data => |$a,$b|$e);
      }
    };
}
