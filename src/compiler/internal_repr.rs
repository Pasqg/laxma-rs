use core::panic;
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    parser::{ast::AST, token_stream::Token},
    utils::InsertionOrderHashMap,
};

use super::{
    grammar::Rules,
    identifier_map::{IdentifierId, IdentifierIdMap, UNKNOWN_ID},
};

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub(super) enum Type {
    SimpleType(IdentifierId),
    TypeParameter(IdentifierId),
    ParametrizedType(IdentifierId, Vec<Rc<Type>>),
    // IdentifierId is the id of the type name, not the name of the function
    FunctionType(IdentifierId, Vec<Rc<Type>>, Rc<Type>),
    Unknown,
}

impl Type {
    pub fn is_unknown(&self) -> bool {
        match self {
            Type::Unknown => true,
            _ => false,
        }
    }

    pub fn is_type_parameter(&self) -> bool {
        match self {
            Type::TypeParameter(_) => true,
            _ => false,
        }
    }

    pub fn as_return_type(&self) -> Rc<Type> {
        match self {
            Type::FunctionType(_, _, return_type) => Rc::clone(return_type),
            _ => panic!("Not a function type: '{:?}'", self),
        }
    }

    pub fn name(&self, map: &IdentifierIdMap) -> Rc<String> {
        match self {
            Type::SimpleType(id) => Rc::clone(map.get_identifier(id).unwrap()),
            Type::TypeParameter(id) => Rc::clone(map.get_identifier(id).unwrap()),
            Type::ParametrizedType(id, _) => Rc::clone(map.get_identifier(id).unwrap()),
            Type::FunctionType(id, _, _) => Rc::clone(map.get_identifier(id).unwrap()),
            Type::Unknown => Rc::new("Unknown".to_string()),
        }
    }

    pub fn full_repr(&self, map: &IdentifierIdMap) -> Rc<String> {
        match self {
            Type::ParametrizedType(id, items) => Rc::new(format!(
                "{}[{}]",
                map.get_identifier(&id).unwrap(),
                items
                    .iter()
                    .map(|t| map.get_identifier(&t.id()).unwrap().to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            )),
            _ => self.name(map),
        }
    }

    pub fn id(&self) -> IdentifierId {
        match self {
            Type::SimpleType(id) => *id,
            Type::TypeParameter(id) => *id,
            Type::ParametrizedType(id, _) => *id,
            Type::FunctionType(id, _, _) => *id,
            Type::Unknown => UNKNOWN_ID,
        }
    }

    pub fn type_parameters(&self) -> HashSet<IdentifierId> {
        match self {
            Type::SimpleType(_) => HashSet::new(),
            Type::TypeParameter(param) => HashSet::from([*param]),
            Type::ParametrizedType(_, vec) => {
                let mut parameters = HashSet::new();
                for t in vec {
                    for parameter in t.type_parameters() {
                        parameters.insert(parameter);
                    }
                }
                parameters
            }
            //todo: returns all the type params in inputs and output
            Type::FunctionType(_, _, _) => HashSet::new(),
            Type::Unknown => HashSet::new(),
        }
    }

    pub fn create_function_type(
        identifier_id_map: &mut IdentifierIdMap,
        types: Vec<Rc<Type>>,
        return_type: Rc<Type>,
    ) -> Type {
        let lambda_name_id = identifier_id_map.get_id(&Rc::new(format!(
            "({}) -> {}",
            types
                .iter()
                .map(|t| t.name(identifier_id_map).to_string())
                .collect::<Vec<String>>()
                .join(", "),
            return_type.name(identifier_id_map)
        )));
        Type::FunctionType(lambda_name_id, types, return_type)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(super) enum TypeVariant {
    Constant(String),
    Cartesian(String, Vec<Rc<Type>>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(super) struct TypeDefinition {
    pub(super) def: Rc<Type>,
    pub(super) variants: HashMap<IdentifierId, Rc<TypeVariant>>,
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
    pub(super) parameters: Vec<Expression>,
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
    pub(super) typing: Rc<Type>,
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
    pub(super) types: HashMap<IdentifierId, TypeDefinition>,
    pub(super) identifier_id_map: IdentifierIdMap,
}

impl Program {
    pub(super) fn new() -> Self {
        Self {
            functions: InsertionOrderHashMap::new(),
            types: HashMap::new(),
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
        Some(Rules::Identifier) => Ok(Type::SimpleType(identifier_id_map.get_id(&Rc::new(name)))),
        Some(Rules::TypeParameter) => Ok(Type::TypeParameter(
            identifier_id_map.get_id(&Rc::new(name)),
        )),
        Some(Rules::ParametrizedType) => {
            let subtype = &ast.children[2];
            let type_params =
                if subtype.id == Some(Rules::Identifier) || subtype.id != Some(Rules::Elements) {
                    let result = type_repr(subtype, identifier_id_map);
                    if result.is_err() {
                        return Err(result.unwrap_err());
                    }
                    vec![Rc::new(result.unwrap())]
                } else {
                    let mut type_params = Vec::new();
                    for i in (0..subtype.children.len()).step_by(2) {
                        let result = type_repr(&subtype.children[i], identifier_id_map);
                        if result.is_err() {
                            return Err(result.unwrap_err());
                        }
                        type_params.push(Rc::new(result.unwrap()));
                    }
                    type_params
                };

            let id = identifier_id_map.get_id(&Rc::new(name));

            Ok(Type::ParametrizedType(id, type_params))
        }
        Some(Rules::FunctionType) => {
            let arguments = &ast.children[1];
            let mut types = Vec::new();
            for child in &arguments.children {
                let result = type_repr(child, identifier_id_map);
                if result.is_err() {
                    return result;
                }
                types.push(Rc::new(result.unwrap()));
            }

            let result = type_repr(&ast.children[4], identifier_id_map);
            if result.is_err() {
                return result;
            }
            let return_type = Rc::new(result.unwrap());

            Ok(Type::create_function_type(
                identifier_id_map,
                types,
                return_type,
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
        if child.matched.len() > 0 && child.matched[0].unwrap_str() != "," {
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
                    let result = destructuring_repr(child, identifier_id_map);
                    if result.is_err() {
                        return Err(result.unwrap_err());
                    }
                    components.push(DestructuringComponent::Destructuring(result.unwrap()));
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
    let result = type_repr(&ast.children[2], identifier_id_map);
    if result.is_err() {
        return Err(result.unwrap_err());
    }

    let type_ = result.unwrap();
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
            let result = argument_repr(ast, identifier_id_map);
            if result.is_err() {
                return Err(result.unwrap_err());
            }
            arguments.push(result.unwrap());
        }
        Some(Rules::Arguments) => {
            for child in &ast.children {
                let result = argument_repr(child, identifier_id_map);
                if result.is_err() {
                    return Err(result.unwrap_err());
                }
                arguments.push(result.unwrap());
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

    if ast.children.len() > 2 {
        let result = arguments_repr(&ast.children[2], identifier_id_map);
        if result.is_err() {
            return Err(result.unwrap_err());
        }

        Ok((function_name, result.unwrap()))
    } else {
        Ok((function_name, Vec::new()))
    }
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
                    let result = expression_repr(child, identifier_map);
                    if result.is_err() {
                        return Err(result.unwrap_err());
                    }
                    parameters.push(result.unwrap());
                }
            }
            Ok(Expression::FunctionCall(FunctionCall {
                id: identifier_map.get_id(&Rc::new(ast.matched[0].unwrap_str())),
                parameters,
            }))
        }
        Some(Rules::TypeConstructor) => {
            let mut parameters = Vec::new();
            for child in &body.children[4].children {
                if child.id == Some(Rules::Expression) {
                    let result = expression_repr(child, identifier_map);
                    if result.is_err() {
                        return Err(result.unwrap_err());
                    }
                    parameters.push(result.unwrap());
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
                let expr = expression_repr(&child.children[2], identifier_map);
                if expr.is_err() {
                    return Err(expr.unwrap_err());
                }
                elements.push((identifier_map.get_id(&Rc::new(identifier)), expr.unwrap()));
            }
            let result = expression_repr(&body.children[2], identifier_map);
            if result.is_err() {
                return Err(result.unwrap_err());
            }
            Ok(Expression::WithBlock(elements, Rc::new(result.unwrap())))
        }
        Some(Rules::IfExpression) => {
            let condition = expression_repr(&body.children[1], identifier_map);
            if condition.is_err() {
                return Err(condition.unwrap_err());
            }
            let true_branch = expression_repr(&body.children[2], identifier_map);
            if true_branch.is_err() {
                return Err(true_branch.unwrap_err());
            }
            let false_branch = expression_repr(&body.children[3], identifier_map);
            if false_branch.is_err() {
                return Err(false_branch.unwrap_err());
            }

            Ok(Expression::If(
                Rc::new(condition.unwrap()),
                Rc::new(true_branch.unwrap()),
                Rc::new(false_branch.unwrap()),
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
                        .collect::<Vec<String>>()
                        .join(" "),
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
                        let result = destructuring_repr(&component, identifier_map);
                        if result.is_err() {
                            return Err(result.unwrap_err());
                        }
                        DestructuringComponent::Destructuring(result.unwrap())
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

        let result = expression_repr(&pattern.children[1].children[1], identifier_map);
        if result.is_err() {
            return Err(result.unwrap_err());
        }

        bodies.push((Pattern { components }, result.unwrap()));
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

    let result = signature_repr(&ast.children[0], identifier_map);
    if result.is_err() {
        return Err(result.unwrap_err());
    }

    let (name, arguments) = result.unwrap();

    let mut bodies = Vec::new();

    let body = &ast.children[1];
    if body.id.is_none() {
        return Err(format!("Expected 'Some' function body"));
    }
    let rule = body.id.unwrap();

    if rule == Rules::FunctionBody {
        let result = expression_repr(&body.children[1], identifier_map);
        if result.is_err() {
            return Err(result.unwrap_err());
        }
        bodies.push((
            Pattern {
                components: Vec::new(),
            },
            result.unwrap(),
        ));
    } else if rule == Rules::PatternMatching {
        let result = pattern_matching_repr(&body.children[1], identifier_map);
        if result.is_err() {
            return Err(result.unwrap_err());
        }
        bodies = result.unwrap();
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
            let result = type_repr(child, identifier_id_map);
            if result.is_err() {
                return Err(result.unwrap_err());
            }
            components.push(Rc::new(result.unwrap()));
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

    let _type = type_repr(&ast.children[1], identifier_id_map);
    if _type.is_err() {
        return Err(_type.unwrap_err());
    }
    let _type = _type.unwrap();

    let mut variants = HashMap::new();
    for child in &ast.children[3].children {
        if child.id == Some(Rules::TypeDef) {
            let result = type_variant_repr(&child, identifier_id_map);
            if result.is_err() {
                return Err(result.unwrap_err());
            }
            let (name, variant) = result.unwrap();
            variants.insert(identifier_id_map.get_id(&Rc::new(name)), Rc::new(variant));
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
    let mut types = HashMap::new();
    for node in &ast.children {
        match node.id {
            Some(Rules::FunctionDef) => {
                let result = function_repr(node, identifier_id_map);
                if result.is_err() {
                    return Err(result.unwrap_err());
                }

                let definition = result.unwrap();
                functions.insert(definition.id, Rc::new(definition));
            }
            Some(Rules::TypeDef) => {
                let result = type_definition_repr(node, identifier_id_map);
                if result.is_err() {
                    return Err(result.unwrap_err());
                }

                let (name, definition) = result.unwrap();
                types.insert(name, definition);
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

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::{
        compiler::{
            grammar::program_parser, identifier_map::IdentifierIdMap, internal_repr::to_repr,
        },
        parser::{combinators::ParserCombinator, token_stream::TokenStream},
    };

    #[test]
    fn test_compile_simple() {
        let code: Vec<&str> = "
        fun double x : Result [ int , String ] -> + ( x , x )
        fun first { x xs } : List [ 'T ] -> x
        fun rest { x xs } : List [ 'T ] = Empty -> x { x xs } -> xs
        "
        .split_whitespace()
        .collect();
        let tokens = TokenStream::from_str(code);
        let result = program_parser().parse(&tokens);

        assert!(result.result);
        print!("{}", result.ast);

        let mut identifier_id_map = IdentifierIdMap::new();
        let result = to_repr(&result.ast, &mut identifier_id_map);
        if result.is_err() {
            panic!("{:?}", result.unwrap_err());
        }
        print!("{:?}", result.unwrap());
    }

    #[test]
    fn test_example() {
        let code: Vec<&str> = "

        fun first { x xs } : List [ 'T ] -> x
        fun rest { x xs } : List [ 'T ] -> xs

        fun is_empty x : List [ 'T ] =
            Empty -> true
            { x xs } -> false

        fun length x : List [ 'T ] =
            Empty -> 0
            { _ xs } -> + ( 1 , length ( xs ) )

        fun sum x : int y : int -> + ( x , y )

        "
        .split_whitespace()
        .collect();
        let tokens = TokenStream::from_str(code);
        let result = program_parser().parse(&tokens);

        let no_exclusion = HashSet::new();
        println!("{}", result.ast.prune(&no_exclusion, &no_exclusion));
        println!(
            "{:?}",
            result
                .ast
                .matched
                .into_iter()
                .map(|x| x.unwrap_str())
                .collect::<Vec<String>>()
        );
    }
}
