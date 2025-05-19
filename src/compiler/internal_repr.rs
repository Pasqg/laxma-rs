use core::panic;
use std::{collections::HashSet, rc::Rc};

use nohash_hasher::IntMap;

use crate::{
    parser::{ast::AST, token_stream::Token},
    utils::InsertionOrderHashMap,
};

use super::{
    grammar::Rules,
    identifier_map::{IdentifierId, IdentifierIdMap, UNDECIDED_ID},
};

pub(super) type RcType = Rc<Type>;

#[derive(Clone, Eq, PartialEq, Debug)]
pub(super) enum Type {
    PrimitiveType(IdentifierId),
    UnboundTypeVariable(IdentifierId),
    CompositeType(IdentifierId, Vec<RcType>),
    // IdentifierId is the id of the type name, not the name of the function
    // (id, arguments types, return type, captures types)
    FunctionType(
        IdentifierId,
        Vec<RcType>,
        RcType,
        Option<Rc<IntMap<IdentifierId, RcType>>>,
    ),
    Undecided,
}

impl Type {
    pub fn is_undecided(&self) -> bool {
        match self {
            Type::Undecided => true,
            _ => false,
        }
    }

    pub fn is_type_parameter(&self) -> bool {
        match self {
            Type::UnboundTypeVariable(_) => true,
            _ => false,
        }
    }

    pub fn as_return_type(&self) -> RcType {
        match self {
            Type::FunctionType(_, _, return_type, _) => Rc::clone(return_type),
            _ => panic!("Not a function type: '{:?}'", self),
        }
    }

    pub fn name(&self, map: &IdentifierIdMap) -> Rc<String> {
        match self {
            Type::PrimitiveType(id) => Rc::clone(map.get_identifier(id).unwrap()),
            Type::UnboundTypeVariable(id) => Rc::clone(map.get_identifier(id).unwrap()),
            Type::CompositeType(id, _) => Rc::clone(map.get_identifier(id).unwrap()),
            Type::FunctionType(id, _, _, _) => Rc::clone(map.get_identifier(id).unwrap()),
            Type::Undecided => Rc::new("Undecided".to_string()),
        }
    }

    pub fn full_repr(&self, map: &IdentifierIdMap) -> Rc<String> {
        match self {
            Type::CompositeType(id, items) => Rc::new(format!(
                "{}[{}]",
                map.get_identifier(&id).unwrap(),
                items
                    .iter()
                    .map(|t| t.full_repr(map).to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            )),
            Type::FunctionType(_, args_t, return_t, _) => Rc::new(format!(
                "({}) -> {}",
                args_t
                    .iter()
                    .map(|t| t.full_repr(map).to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                return_t.full_repr(map),
            )),
            _ => self.name(map),
        }
    }

    pub fn id(&self) -> IdentifierId {
        match self {
            Type::PrimitiveType(id) => *id,
            Type::UnboundTypeVariable(id) => *id,
            Type::CompositeType(id, _) => *id,
            Type::FunctionType(id, _, _, _) => *id,
            Type::Undecided => UNDECIDED_ID,
        }
    }

    pub fn type_parameters(&self) -> HashSet<IdentifierId> {
        match self {
            Type::PrimitiveType(_) => HashSet::new(),
            Type::UnboundTypeVariable(param) => HashSet::from([*param]),
            Type::CompositeType(_, vec) => {
                let mut parameters = HashSet::new();
                for t in vec {
                    for parameter in t.type_parameters() {
                        parameters.insert(parameter);
                    }
                }
                parameters
            }
            Type::FunctionType(_, args, ret, _) => {
                let mut parameters = HashSet::new();
                for t in args {
                    for parameter in t.type_parameters() {
                        parameters.insert(parameter);
                    }
                }
                for parameter in ret.type_parameters() {
                    parameters.insert(parameter);
                }
                parameters
            }
            Type::Undecided => HashSet::new(),
        }
    }

    pub fn create_function_type(
        identifier_id_map: &mut IdentifierIdMap,
        types: Vec<RcType>,
        return_type: RcType,
        captures: Option<Rc<IntMap<IdentifierId, RcType>>>,
    ) -> Type {
        let lambda_name_id = identifier_id_map.get_id(&Rc::new(format!(
            "({}) -> {}",
            types
                .iter()
                .map(|t| t.full_repr(identifier_id_map).as_ref().to_owned())
                .collect::<Vec<String>>()
                .join(", "),
            return_type.full_repr(identifier_id_map)
        )));
        Type::FunctionType(lambda_name_id, types, return_type, captures.clone())
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(super) enum TypeVariant {
    Constant(String),
    Cartesian(String, Vec<RcType>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(super) struct TypeDefinition {
    pub(super) def: RcType,
    pub(super) variants: IntMap<IdentifierId, Rc<TypeVariant>>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(super) struct SuperDefinition {
    pub(super) def: Rc<Type>,
    pub(super) variants: Vec<Rc<Type>>,
}

#[derive(Clone, PartialEq, Debug)]
pub(super) enum DestructuringComponent {
    Identifier(IdentifierId),
    Destructuring(Destructuring),
    Integer(i64),
    Float(f32),
    String(Rc<String>),
}

#[derive(Debug, PartialEq, Clone)]
pub(super) struct Destructuring(
    pub(super) IdentifierId,
    pub(super) Vec<DestructuringComponent>,
);

#[derive(Debug, PartialEq, Clone)]
pub(super) struct FunctionCall {
    pub(super) id: IdentifierId,
    pub(super) arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub(super) enum Expression {
    TypeConstructor(IdentifierId, IdentifierId, Vec<Expression>),
    FunctionCall(FunctionCall),
    Identifier(IdentifierId),
    Integer(i64),
    Float(f32),
    String(Rc<String>),
    WithBlock(Vec<(IdentifierId, Expression)>, Rc<Expression>),
    If(Rc<Expression>, Rc<Expression>, Rc<Expression>),
    LambdaExpression(Rc<FunctionDefinition>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(super) struct FunctionArgument {
    pub(super) identifier: IdentifierId,
    pub(super) typing: RcType,
}

#[derive(Debug, PartialEq, Clone)]
pub(super) struct Pattern {
    pub(super) components: Vec<DestructuringComponent>,
}

#[derive(Debug, PartialEq, Clone)]
pub(super) struct FunctionDefinition {
    pub(super) id: IdentifierId,
    pub(super) arguments: Vec<FunctionArgument>,
    pub(super) bodies: Vec<(Pattern, Expression)>,
}

impl FunctionDefinition {
    pub(super) fn is_not_pattern_matched(&self) -> bool {
        self.bodies.len() == 1 && self.bodies[0].0.components.is_empty()
    }
}

#[derive(Debug, Clone)]
pub(super) struct Program {
    pub(super) functions: InsertionOrderHashMap<IdentifierId, Rc<FunctionDefinition>>,
    pub(super) types: IntMap<IdentifierId, TypeDefinition>,
    pub(super) identifier_id_map: IdentifierIdMap,
}

impl Program {
    pub(super) fn new() -> Self {
        Self {
            functions: InsertionOrderHashMap::new(),
            types: IntMap::default(),
            identifier_id_map: IdentifierIdMap::new(),
        }
    }

    pub(super) fn var_name(&self, id: &IdentifierId) -> &Rc<String> {
        self.identifier_id_map.get_identifier(id).unwrap()
    }
}

fn type_repr(ast: &AST<Rules>, identifier_id_map: &mut IdentifierIdMap) -> Result<Type, String> {
    let name = ast.matched[0].unwrap_str();
    return match ast.id {
        Some(Rules::Identifier) => Ok(Type::PrimitiveType(
            identifier_id_map.get_id(&Rc::new(name)),
        )),
        Some(Rules::TypeParameter) => Ok(Type::UnboundTypeVariable(
            identifier_id_map.get_id(&Rc::new(name)),
        )),
        Some(Rules::CompositeType) => {
            let subtype = &ast.children[2];
            let type_params = if subtype.id == Some(Rules::Identifier)
                || subtype.id != Some(Rules::Elements)
            {
                vec![Rc::new(type_repr(subtype, identifier_id_map)?)]
            } else {
                let mut type_params = Vec::new();
                for i in (0..subtype.children.len()).step_by(2) {
                    type_params.push(Rc::new(type_repr(&subtype.children[i], identifier_id_map)?));
                }
                type_params
            };

            let id = identifier_id_map.get_id(&Rc::new(name));

            Ok(Type::CompositeType(id, type_params))
        }
        Some(Rules::FunctionType) => {
            let arguments = &ast.children[1];
            let mut types = Vec::new();
            for child in &arguments.children {
                if !child.matched.is_empty() && child.matched[0].unwrap_str() != "," {
                    types.push(Rc::new(type_repr(child, identifier_id_map)?));
                }
            }

            let return_type = Rc::new(type_repr(&ast.children[4], identifier_id_map)?);

            Ok(Type::create_function_type(
                identifier_id_map,
                types,
                return_type,
                None,
            ))
        }
        _ => Err(format!("Expected a type but got {}", ast)),
    };
}

fn destructuring_repr(
    ast: &AST<Rules>,
    identifier_id_map: &mut IdentifierIdMap,
) -> Result<Destructuring, String> {
    if ast.id.is_none() || ast.id.unwrap() != Rules::Destructuring {
        return Err(format!("Expected a Destructuring AST but got {}", ast));
    }

    let mut first_component = None;
    let mut components = Vec::new();
    for child in &ast.children {
        if !child.matched.is_empty() && child.matched[0].unwrap_str() != "," {
            match child.id {
                Some(Rules::Identifier) => {
                    let result = child.matched[0].unwrap_str();
                    if first_component.is_none() {
                        first_component = Some(result);
                    } else {
                        components.push(DestructuringComponent::Identifier(
                            identifier_id_map.get_id(&Rc::new(result)),
                        ));
                    }
                }
                Some(Rules::Destructuring) => {
                    let result = destructuring_repr(child, identifier_id_map)?;
                    components.push(DestructuringComponent::Destructuring(result));
                }
                _ => return Err(format!("Unexpected destructuring: {}", child)),
            }
        }
    }

    Ok(Destructuring(
        identifier_id_map.get_id(&Rc::new(
            first_component.expect("Bug! Destructuring cannot be empty"),
        )),
        components,
    ))
}

fn argument_repr(
    ast: &AST<Rules>,
    identifier_id_map: &mut IdentifierIdMap,
) -> Result<FunctionArgument, String> {
    let arg = &ast.children[0];
    let type_ = type_repr(&ast.children[2], identifier_id_map)?;
    return if arg.id == Some(Rules::Identifier) {
        Ok(FunctionArgument {
            identifier: identifier_id_map.get_id(&Rc::new(arg.matched[0].unwrap_str())),
            typing: Rc::new(type_),
        })
    } else {
        Err(format!(
            "Expected Identifier for function argument but got {:?}",
            arg.id.unwrap()
        ))
    };
}

fn arguments_repr(
    ast: &AST<Rules>,
    identifier_id_map: &mut IdentifierIdMap,
) -> Result<Vec<FunctionArgument>, String> {
    let mut arguments = Vec::new();
    match ast.id {
        None => {}
        Some(Rules::Argument) => {
            let result = argument_repr(ast, identifier_id_map)?;
            arguments.push(result);
        }
        Some(Rules::Arguments) => {
            for child in &ast.children {
                if !child.matched.is_empty() && child.matched[0].unwrap_str() != "," {
                    let result = argument_repr(child, identifier_id_map)?;
                    arguments.push(result);
                }
            }
        }
        _ => panic!("Expected None, Argument or Arguments but got {:?}", ast.id),
    }
    Ok(arguments)
}

fn signature_repr(
    ast: &AST<Rules>,
    identifier_id_map: &mut IdentifierIdMap,
) -> Result<(String, Vec<FunctionArgument>), String> {
    if ast.id.is_none() || ast.id.unwrap() != Rules::FunctionSignature {
        return Err(format!(
            "Expected a FunctionSignature AST but got {:?}",
            ast.id
        ));
    }

    let function_name = ast.matched[1].unwrap_str();
    if ast.children.len() <= 2 {
        return Ok((function_name, Vec::new()));
    }

    Ok((
        function_name,
        arguments_repr(&ast.children[2], identifier_id_map)?,
    ))
}

pub fn expression_repr(
    ast: &AST<Rules>,
    identifier_map: &mut IdentifierIdMap,
) -> Result<Expression, String> {
    if ast.id.is_none() || ast.id.unwrap() != Rules::Expression {
        return Err(format!("Expected Expression but got {:?}", ast.id));
    }

    let body = &ast.children[0];
    let rule = body.id;

    match rule {
        Some(Rules::Identifier) => Ok(Expression::Identifier(
            identifier_map.get_id(&Rc::new(ast.matched[0].unwrap_str())),
        )),
        Some(Rules::Integer) => Ok(Expression::Integer(
            ast.matched[0].unwrap_str().parse().unwrap(),
        )),
        Some(Rules::String) => Ok(Expression::String(Rc::new(ast.matched[0].unwrap_str()))),
        Some(Rules::Float) => Ok(Expression::Float(
            ast.matched[0].unwrap_str().parse().unwrap(),
        )),
        Some(Rules::FunctionCall) => {
            let mut parameters = Vec::new();
            for child in &body.children[2].children {
                if child.id == Some(Rules::Expression) {
                    parameters.push(expression_repr(child, identifier_map)?);
                }
            }
            Ok(Expression::FunctionCall(FunctionCall {
                id: identifier_map.get_id(&Rc::new(ast.matched[0].unwrap_str())),
                arguments: parameters,
            }))
        }
        Some(Rules::TypeConstructor) => {
            let mut parameters = Vec::new();
            for child in &body.children[4].children {
                if child.id == Some(Rules::Expression) {
                    parameters.push(expression_repr(child, identifier_map)?);
                }
            }
            Ok(Expression::TypeConstructor(
                identifier_map.get_id(&Rc::new(ast.matched[0].unwrap_str())),
                identifier_map.get_id(&Rc::new(ast.matched[2].unwrap_str())),
                parameters,
            ))
        }
        Some(Rules::WithBlock) => {
            let mut elements = Vec::new();
            for child in &body.children[1].children {
                let identifier = child.children[0].matched[0].unwrap_str();
                let expr = expression_repr(&child.children[2], identifier_map)?;
                elements.push((identifier_map.get_id(&Rc::new(identifier)), expr));
            }
            let result = expression_repr(&body.children[2], identifier_map)?;
            Ok(Expression::WithBlock(elements, Rc::new(result)))
        }
        Some(Rules::IfExpression) => {
            let condition = expression_repr(&body.children[1], identifier_map)?;
            let true_branch = expression_repr(&body.children[2], identifier_map)?;
            let false_branch = expression_repr(&body.children[3], identifier_map)?;

            Ok(Expression::If(
                Rc::new(condition),
                Rc::new(true_branch),
                Rc::new(false_branch),
            ))
        }
        Some(Rules::LambdaExpression) => {
            let result = arguments_repr(&body.children[1], identifier_map);
            if result.is_err() {
                return Err(format!(
                    "Error in lambda arguments: {}",
                    result.unwrap_err()
                ));
            }
            let args = result.unwrap();

            let result = expression_repr(&body.children[4], identifier_map);
            if result.is_err() {
                return Err(format!(
                    "Error in lambda return expression: {}",
                    result.unwrap_err()
                ));
            }
            let expr = result.unwrap();

            Ok(Expression::LambdaExpression(Rc::new(FunctionDefinition {
                id: identifier_map.get_id(&Rc::new(
                    body.matched
                        .iter()
                        .map(|t| t.unwrap_str())
                        .map(|t| {
                            if t.as_str() == "->" {
                                " -> ".to_string()
                            } else {
                                t
                            }
                        })
                        .collect::<Vec<String>>()
                        .join(""),
                )),
                arguments: args,
                bodies: vec![(
                    Pattern {
                        components: Vec::new(),
                    },
                    expr,
                )],
            })))
        }
        _ => Err(format!(
            "Expected WithExpression, IfExpression, FunctionCall, Identifier or Number, but got {}",
            body
        )),
    }
}

fn pattern_matching_repr(
    ast: &AST<Rules>,
    identifier_map: &mut IdentifierIdMap,
) -> Result<Vec<(Pattern, Expression)>, String> {
    let mut bodies = Vec::new();
    for pattern in &ast.children {
        if pattern.id != Some(Rules::Pattern) {
            return Err(format!("Expected Pattern but got {}", pattern));
        }

        let mut components = Vec::new();
        for component in &pattern.children[0].children {
            if component.matched[0] != Token::str(",") {
                let destructuring = match component.id {
                    Some(Rules::Identifier) => DestructuringComponent::Identifier(
                        identifier_map.get_id(&Rc::new(component.matched[0].unwrap_str())),
                    ),
                    Some(Rules::Destructuring) => {
                        let result = destructuring_repr(&component, identifier_map)?;
                        DestructuringComponent::Destructuring(result)
                    }
                    Some(Rules::Integer) => DestructuringComponent::Integer(
                        component.matched[0].unwrap_str().parse().unwrap(),
                    ),
                    Some(Rules::String) => {
                        DestructuringComponent::String(Rc::new(component.matched[0].unwrap_str()))
                    }
                    Some(Rules::Float) => DestructuringComponent::Float(
                        component.matched[0].unwrap_str().parse().unwrap(),
                    ),
                    _ => {
                        return Err(format!(
                            "Expected Identifier, Destructuring or Number but got {}",
                            component
                        ));
                    }
                };
                components.push(destructuring);
            }
        }

        let result = expression_repr(&pattern.children[1].children[1], identifier_map)?;
        bodies.push((Pattern { components }, result));
    }
    return Ok(bodies);
}

fn function_repr(
    ast: &AST<Rules>,
    identifier_map: &mut IdentifierIdMap,
) -> Result<FunctionDefinition, String> {
    if ast.id.is_none() || ast.id.unwrap() != Rules::FunctionDef {
        return Err(format!("Expected a FunctionDef AST but got {:?}", ast.id));
    }

    let (name, arguments) = signature_repr(&ast.children[0], identifier_map)?;

    let mut bodies = Vec::new();

    let body = &ast.children[1];
    if body.id.is_none() {
        return Err(format!("Expected 'Some' function body"));
    }
    let rule = body.id.unwrap();

    if rule == Rules::FunctionBody {
        bodies.push((
            Pattern {
                components: Vec::new(),
            },
            expression_repr(&body.children[1], identifier_map)?,
        ));
    } else if rule == Rules::PatternMatching {
        bodies = pattern_matching_repr(&body.children[1], identifier_map)?;
    } else {
        return Err(format!(
            "Expected FunctionBody or PatternMatching but got {}",
            body
        ));
    }

    let definition = FunctionDefinition {
        id: identifier_map.get_id(&Rc::new(name)),
        arguments,
        bodies,
    };

    Ok(definition)
}

fn type_variant_repr(
    ast: &AST<Rules>,
    identifier_id_map: &mut IdentifierIdMap,
) -> Result<(String, TypeVariant), String> {
    let name = ast.matched[0].unwrap_str();
    if ast.children[1].id == Some(Rules::Elements) {
        let mut components = Vec::new();
        for child in &ast.children[1].children {
            let result = type_repr(child, identifier_id_map)?;
            components.push(Rc::new(result));
        }
        return Ok((name.clone(), TypeVariant::Cartesian(name, components)));
    }

    Ok((name.clone(), TypeVariant::Constant(name)))
}

fn type_definition_repr(
    ast: &AST<Rules>,
    identifier_id_map: &mut IdentifierIdMap,
) -> Result<(IdentifierId, TypeDefinition), String> {
    if ast.id.is_none() || ast.id.unwrap() != Rules::TypeDef {
        return Err(format!("Expected a TypeDef AST but got {:?}", ast.id));
    }

    if ast.children[1].id == Some(Rules::TypeParameter) {
        return Err(format!(
            "New type name cannot be a type parameter, got {}",
            ast
        ));
    }

    let _type = type_repr(&ast.children[1], identifier_id_map)?;
    let mut variants = IntMap::default();
    for child in &ast.children[3].children {
        if child.id == Some(Rules::TypeDef) {
            let (name, variant) = type_variant_repr(&child, identifier_id_map)?;
            let variant_id = identifier_id_map.get_id(&Rc::new(name));
            if variants.contains_key(&variant_id) {
                return Err(format!(
                    "Duplicate variant '{}' of type '{}'",
                    identifier_id_map.get_identifier(&variant_id).unwrap(),
                    _type.full_repr(identifier_id_map),
                ));
            }
            variants.insert(variant_id, Rc::new(variant));
        }
    }

    let type_rc = Rc::new(_type);
    let definition = TypeDefinition {
        def: Rc::clone(&type_rc),
        variants,
    };

    Ok((type_rc.id(), definition))
}

pub fn to_repr(
    ast: &AST<Rules>,
    identifier_id_map: &mut IdentifierIdMap,
) -> Result<Program, String> {
    if ast.id.is_none() || ast.id.unwrap() != Rules::Program {
        return Err(format!("Expected a Program AST but got {:?}", ast.id));
    }

    let mut functions = InsertionOrderHashMap::new();
    let mut types = IntMap::default();
    for node in &ast.children {
        match node.id {
            Some(Rules::FunctionDef) => {
                let definition = function_repr(node, identifier_id_map)?;
                functions.insert(definition.id, Rc::new(definition));
            }
            Some(Rules::TypeDef) => {
                let (id, definition) = type_definition_repr(node, identifier_id_map)?;
                types.insert(id, definition);
            }
            _ => {
                return Err(format!(
                    "Expected FunctionDefinition or TypeDefinition at top level but got {:?}",
                    node.id
                ));
            }
        }
    }

    Ok(Program {
        functions,
        types,
        identifier_id_map: identifier_id_map.clone(),
    })
}
