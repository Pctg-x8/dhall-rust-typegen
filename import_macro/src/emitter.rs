
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

    pub fn emit_inline_type<'s>(&'s self, ctx: &mut CodeEmissionContext<'s>) -> InlineType {
        match self {
            Self::BoolType => InlineType::Bool,
            Self::NaturalType => InlineType::Nat,
            Self::IntegerType => InlineType::Int,
            Self::DoubleType => InlineType::Dbl,
            Self::TextType => InlineType::Txt,
            Self::OptionalType(x) => InlineType::Opt(Box::new(x.emit_inline_type(ctx))),
            Self::ListType(x) => InlineType::List(Box::new(x.emit_inline_type(ctx))),
            Self::RecordType(_) | Self::RecordLiteral(_) | Self::UnionType(_) => InlineType::TypeRef({
                let ty = self.emit_item_type(ctx);
                ctx.query_type_index(ty)
            }),
            _ => unimplemented!("Unhandled subtree: {:?}", self)
        }
    }
    pub fn emit_item_type<'s>(&'s self, ctx: &mut CodeEmissionContext<'s>) -> ItemType<'s> {
        match self {
            Self::RecordType(es) => ItemType::Struct(
                es.iter().map(|(k, v)| (k as _, v.emit_inline_type(ctx))).collect()
            ),
            Self::UnionType(es) => ItemType::Enum(
                es.iter().map(|(k, v)| (k as _, v.as_ref().map(|x| x.emit_inline_type(ctx)))).collect()
            ),
            Self::RecordLiteral(es) => if let Some(ty) = es.get("Type") {
                ty.emit_item_type(ctx)
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
    #[allow(dead_code)]
    pub fn emit_code<'s>(&self, ctx: &CodeEmissionContext<'s>) -> std::borrow::Cow<'s, str> {
        match self {
            Self::Bool => From::from("bool"),
            Self::Nat => From::from("u64"),
            Self::Int => From::from("i64"),
            Self::Dbl => From::from("f64"),
            Self::Txt => From::from("String"),
            Self::Opt(x) => From::from(format!("Option<{}>", x.emit_code(ctx))),
            Self::List(x) => From::from(format!("Vec<{}>", x.emit_code(ctx))),
            &Self::TypeRef(tyid) => ctx.type_name(tyid)
        }
    }
}
impl<'s> ItemType<'s> {
    #[allow(dead_code)]
    pub fn emit_code(&self, name: &str, ctx: &CodeEmissionContext) -> String {
        match self {
            Self::Struct(es) => format!(
                "#[derive(Deserialize)]\npub struct {} {{ {} }}",
                name,
                es.iter().map(|(k, v)| format!("pub {}: {}", k, v.emit_code(ctx))).collect::<Vec<_>>().join(", ")
            ),
            Self::Enum(es) => format!(
                "#[derive(Deserialize)]\npub enum {} {{ {} }}",
                name,
                es.iter().map(|(k, v)| match v {
                    Some(ty) => format!("{}({})", k, ty.emit_code(ctx)),
                    None => String::from(*k)
                }).collect::<Vec<_>>().join(", ")
            )
        }
    }
}

pub struct CodeEmissionContext<'s> {
    type_ids: HashMap<ItemType<'s>, usize>,
    type_name_slots: Vec<Option<&'s str>>
}
impl<'s> CodeEmissionContext<'s> {
    pub fn new() -> Self {
        CodeEmissionContext {
            type_ids: HashMap::new(),
            type_name_slots: Vec::new()
        }
    }

    pub fn query_type_index(&mut self, ty: ItemType<'s>) -> usize {
        let (ids, names) = (&mut self.type_ids, &mut self.type_name_slots);
        *ids.entry(ty).or_insert_with(|| { names.push(None); names.len() - 1 })
    }
    pub fn update_type_name(&mut self, index: usize, name: &'s str) {
        self.type_name_slots[index] = Some(name);
    }
    pub fn type_name(&self, index: usize) -> std::borrow::Cow<'s, str> {
        self.type_name_slots.get(index).and_then(|x| x.map(From::from))
            .unwrap_or_else(|| From::from(format!("AnonType{}", index)))
    }
}