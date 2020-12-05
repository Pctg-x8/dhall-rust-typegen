
use std::collections::HashMap;

#[derive(Debug)]
pub enum ElementTree {
    BoolType, NaturalType, IntegerType, DoubleType, TextType,
    OptionalType(Box<ElementTree>),
    ListType(Box<ElementTree>),
    RecordType(HashMap<String, ElementTree>),
    UnionType(HashMap<String, Option<ElementTree>>),
    RecordLiteral(HashMap<String, ElementTree>),
    UnionValue(String, Option<Box<ElementTree>>)
}
impl ElementTree {
    pub fn from_nir(nir: &dhall::semantics::Nir) -> Self {
        use dhall::semantics::NirKind::*;
        use dhall::builtins::Builtin;

        match nir.kind() {
            BuiltinType(Builtin::Bool) => Self::BoolType,
            BuiltinType(Builtin::Natural) => Self::NaturalType,
            BuiltinType(Builtin::Integer) => Self::IntegerType,
            BuiltinType(Builtin::Double) => Self::DoubleType,
            BuiltinType(Builtin::Text) => Self::TextType,
            OptionalType(t) => Self::OptionalType(Box::new(Self::from_nir(t))),
            ListType(t) => Self::ListType(Box::new(Self::from_nir(t))),
            RecordType(es) => Self::RecordType(
                es.iter().map(|(k, v)| (k.into(), Self::from_nir(v))).collect()
            ),
            UnionType(es) => Self::UnionType(
                es.iter().map(|(k, v)| (k.into(), v.as_ref().map(Self::from_nir))).collect()
            ),
            RecordLit(es) => Self::RecordLiteral(
                es.iter().map(|(k, v)| (k.into(), Self::from_nir(v))).collect()
            ),
            UnionConstructor(f, _) => Self::UnionValue(f.into(), None),
            UnionLit(f, x, _) => Self::UnionValue(f.into(), Some(Box::new(Self::from_nir(x)))),
            _ => unimplemented!("Unhandled Nir: {:?}", nir)
        }
    }

    pub fn emit_type_code(&self, type_name: Option<&str>) -> String {
        match self {
            Self::BoolType => String::from("bool"),
            Self::NaturalType => String::from("u64"),
            Self::IntegerType => String::from("i64"),
            Self::DoubleType => String::from("f64"),
            Self::TextType => String::from("String"),
            Self::RecordType(es) => format!(
                "pub struct {} {{ {} }}",
                type_name.unwrap_or("AnonStructure"),
                es.iter().map(|(k, v)| format!("pub {}: {}", k, Self::emit_type_code(v, None)))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::UnionType(es) => format!(
                "pub enum {} {{ {} }}",
                type_name.unwrap_or("AnonEnum"),
                es.iter().map(|(k, v)| if let Some(v) = v {
                    format!("{}({})", k, v.emit_type_code(None))
                } else { String::from(k) }).collect::<Vec<_>>().join(", ")
            ),
            Self::RecordLiteral(es) => if let Some(ty) = es.get("Type") {
                ty.emit_type_code(type_name)
            } else {
                panic!("No `Type` entry in record")
            },
            _ => unimplemented!("Unhandled Element: {:?}", self)
        }
    }
}

fn main() {
    let path = std::env::args().nth(1).expect("missing path");
    let parsed = dhall::Parsed::parse_file(path.as_ref()).expect("Failed to parse schema file");
    let resolved = dbg!(parsed).resolve().expect("Failed to resolve parsed schema");
    let typed = dbg!(resolved).typecheck().expect("Failed to check type in schema");
    let normalized = dbg!(typed).normalize();
    println!("hir: {:?}", normalized.as_nir().to_hir_noenv());

    let tree = ElementTree::from_nir(normalized.as_nir());
    println!("ty: {:?}", tree);

    let tycode = match tree {
        ElementTree::RecordLiteral(es) => {
            // toplevel record
            es.iter().map(|(k, v)| v.emit_type_code(Some(k))).collect()
        },
        _ => tree.emit_type_code(None)
    };
    println!("tycode: {}", tycode);
}
