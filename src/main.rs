
use std::collections::HashMap;

#[derive(Debug)]
pub enum ElementTree {
    BoolType, NaturalType, IntegerType, DoubleType, TextType,
    OptionalType(Box<ElementTree>),
    ListType(Box<ElementTree>),
    RecordType(Vec<(String, ElementTree)>),
    UnionType(Vec<(String, Option<ElementTree>)>),
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

    pub fn emit_inline_type<'s>(
        &'s self, type_ids: &mut HashMap<ItemType<'s>, usize>, type_name_slots: &mut Vec<Option<&'s str>>
    ) -> InlineType {
        match self {
            Self::BoolType => InlineType::Bool,
            Self::NaturalType => InlineType::Nat,
            Self::IntegerType => InlineType::Int,
            Self::DoubleType => InlineType::Dbl,
            Self::TextType => InlineType::Txt,
            Self::OptionalType(x) => InlineType::Opt(Box::new(x.emit_inline_type(type_ids, type_name_slots))),
            Self::ListType(x) => InlineType::List(Box::new(x.emit_inline_type(type_ids, type_name_slots))),
            Self::RecordType(_) | Self::RecordLiteral(_) | Self::UnionType(_) => {
                let ty = self.emit_item_type(type_ids, type_name_slots);
                let id = *type_ids.entry(ty)
                    .or_insert_with(|| { type_name_slots.push(None); type_name_slots.len() - 1 });
                InlineType::TypeRef(id)
            },
            _ => unimplemented!("Unhandled subtree: {:?}", self)
        }
    }
    pub fn emit_item_type<'s>(
        &'s self, type_ids: &mut HashMap<ItemType<'s>, usize>, type_name_slots: &mut Vec<Option<&'s str>>
    ) -> ItemType<'s> {
        match self {
            Self::RecordType(es) => ItemType::Struct(
                es.iter().map(|(k, v)| (k as _, v.emit_inline_type(type_ids, type_name_slots))).collect()
            ),
            Self::UnionType(es) => ItemType::Enum(
                es.iter().map(|(k, v)| (k as _, v.as_ref().map(|x| x.emit_inline_type(type_ids, type_name_slots)))).collect()
            ),
            Self::RecordLiteral(es) => if let Some(ty) = es.get("Type") {
                ty.emit_item_type(type_ids, type_name_slots)
            } else {
                panic!("No `Type` entry in record")
            },
            _ => unimplemented!("Unhandled Element: {:?}", self)
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum InlineType {
    Bool, Nat, Int, Dbl, Txt, Opt(Box<InlineType>), List(Box<InlineType>), TypeRef(usize)
}
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum ItemType<'s> {
    Struct(Vec<(&'s str, InlineType)>),
    Enum(Vec<(&'s str, Option<InlineType>)>)
}
impl InlineType {
    pub fn emit_code<'s>(&self, type_names: &[Option<&'s str>]) -> std::borrow::Cow<'s, str> {
        match self {
            Self::Bool => From::from("bool"),
            Self::Nat => From::from("u64"),
            Self::Int => From::from("i64"),
            Self::Dbl => From::from("f64"),
            Self::Txt => From::from("String"),
            Self::Opt(x) => From::from(format!("Option<{}>", x.emit_code(type_names))),
            Self::List(x) => From::from(format!("Vec<{}>", x.emit_code(type_names))),
            &Self::TypeRef(tyid) => type_names.get(tyid).and_then(|x| x.map(From::from))
                .unwrap_or_else(|| From::from(format!("AnonType{}", tyid)))
        }
    }
}
impl<'s> ItemType<'s> {
    pub fn emit_code(&self, name: &str, type_names: &[Option<&'s str>]) -> String {
        match self {
            Self::Struct(es) => format!(
                "pub struct {} {{ {} }}",
                name,
                es.iter().map(|(k, v)| format!("pub {}: {}", k, v.emit_code(type_names))).collect::<Vec<_>>().join(", ")
            ),
            Self::Enum(es) => format!(
                "pub enum {} {{ {} }}",
                name,
                es.iter().map(|(k, v)| match v {
                    Some(ty) => format!("{}({})", k, ty.emit_code(type_names)),
                    None => String::from(*k)
                }).collect::<Vec<_>>().join(", ")
            )
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

    let mut type_ids = HashMap::new();
    let mut type_name_slots = Vec::new();
    let tycode = match tree {
        ElementTree::RecordLiteral(ref es) => {
            // toplevel is record
            es.iter().map(|(k, v)| {
                let xty = v.emit_item_type(&mut type_ids, &mut type_name_slots);
                let tyid = *type_ids.entry(xty.clone())
                    .or_insert_with(|| { type_name_slots.push(None); type_name_slots.len() - 1});
                type_name_slots[tyid] = Some(k);
                (tyid, xty)
            }).collect()
        },
        _ => vec![(0, tree.emit_item_type(&mut type_ids, &mut type_name_slots))]
    };
    println!("tycode: {:?}", tycode);
    println!("type names: {:?}", type_name_slots);

    let tycode = type_ids.into_iter().map(|(ty, tx)| match type_name_slots.get(tx).and_then(|x| x.as_deref()) {
        Some(n) => ty.emit_code(n, &type_name_slots),
        None => ty.emit_code(&format!("AnonType{}", tx), &type_name_slots)
    }).collect::<Vec<_>>().join("\n");
    println!("generated: {}", tycode);
}
