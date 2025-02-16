use core::panic;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    rc::Rc,
};

use crate::parser::{ast::AST, token_stream::Token};

use super::grammar::Rules;

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub(super) enum Type {
    SimpleType(Rc<String>),
    TypeParameter(Rc<String>),
    ParametrizedType(Rc<String>, Vec<Type>),
    FunctionType(Vec<Type>, Rc<Type>),
    Unknown,
}

impl Type {
    pub fn name(&self) -> Rc<String> {
        match self {
            Type::SimpleType(name) => name.clone(),
            Type::TypeParameter(name) => name.clone(),
            Type::ParametrizedType(name, _) => name.clone(),
            Type::FunctionType(inputs, output) => Rc::new(format!(
                "({}) -> {}",
                inputs
                    .iter()
                    .map(|t| t.name().to_string())
                    .collect::<Vec<String>>()
                    .join(","),
                output.name()
            )),
            Type::Unknown => panic!("Uknown type doesn't have name"),
        }
    }

    pub fn type_parameters(&self) -> HashSet<Rc<String>> {
        match self {
            Type::SimpleType(_) => HashSet::new(),
            Type::TypeParameter(param) => HashSet::from([param.clone()]),
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
            Type::FunctionType(inputs, output) => HashSet::new(),
            Type::Unknown => HashSet::new(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(super) enum TypeVariant {
    Constant(String),
    Cartesian(String, Vec<Type>),
}

impl TypeVariant {
    pub(super) fn name(&self) -> &String {
        match self {
            TypeVariant::Constant(name) => name,
            TypeVariant::Cartesian(name, _) => name,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(super) struct TypeDefinition {
    pub(super) def: Type,
    pub(super) variants: HashMap<String, TypeVariant>,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub(super) enum DestructuringComponent {
    Identifier(Rc<String>),
    Destructuring(Destructuring),
    Number(i64),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub(super) struct Destructuring(pub(super) String, pub(super) Vec<DestructuringComponent>);

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub(super) struct FunctionCall {
    pub(super) name: String,
    pub(super) parameters: Vec<Expression>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub(super) enum Expression {
    TypeConstructor(String, String, Vec<Expression>),
    FunctionCall(FunctionCall),
    Identifier(String),
    Number(i64),
    WithBlock(Vec<(Rc<String>, Expression)>, Rc<Expression>),
    If(Rc<Expression>, Rc<Expression>, Rc<Expression>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(super) struct FunctionArgument {
    pub(super) identifier: Rc<String>,
    pub(super) typing: Type,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(super) struct Pattern {
    pub(super) components: Vec<DestructuringComponent>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(super) struct FunctionDefinition {
    pub(super) name: String,
    pub(super) arguments: Vec<FunctionArgument>,
    pub(super) bodies: Vec<(Pattern, Expression)>,
}

impl FunctionDefinition {
    pub(super) fn is_not_pattern_matched(&self) -> bool {
        self.bodies.len() == 1 && self.bodies[0].0.components.is_empty()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(super) struct Program {
    pub(super) functions: BTreeMap<Rc<String>, FunctionDefinition>,
    pub(super) types: HashMap<Rc<String>, TypeDefinition>,
}

fn type_repr(ast: &AST<Rules>) -> Result<Type, String> {
    let name = ast.matched[0].unwrap_str();
    return match ast.id {
        Some(Rules::Identifier) => Ok(Type::SimpleType(Rc::new(name))),
        Some(Rules::TypeParameter) => Ok(Type::TypeParameter(Rc::new(name))),
        Some(Rules::ParametrizedType) => {
            let subtype = &ast.children[2];
            if subtype.id == Some(Rules::Identifier) || subtype.id != Some(Rules::Elements) {
                let result = type_repr(subtype);
                if result.is_err() {
                    return Err(result.unwrap_err());
                }
                return Ok(Type::ParametrizedType(Rc::new(name), vec![result.unwrap()]));
            }

            let mut type_params = Vec::new();
            for i in (0..subtype.children.len()).step_by(2) {
                let result = type_repr(&subtype.children[i]);
                if result.is_err() {
                    return Err(result.unwrap_err());
                }
                type_params.push(result.unwrap());
            }

            Ok(Type::ParametrizedType(Rc::new(name), type_params))
        }
        _ => Err(format!("Expected a type but got {:?}", ast)),
    };
}

fn destructuring_repr(ast: &AST<Rules>) -> Result<Destructuring, String> {
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
                        components.push(DestructuringComponent::Identifier(Rc::new(result)));
                    }
                }
                Some(Rules::Destructuring) => {
                    let result = destructuring_repr(child);
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
        first_component.expect("Bug! Destructuring cannot be empty"),
        components,
    ))
}

fn argument_repr(ast: &AST<Rules>) -> Result<FunctionArgument, String> {
    let arg = &ast.children[0];
    let result = type_repr(&ast.children[2]);
    if result.is_err() {
        return Err(result.unwrap_err());
    }

    let type_ = result.unwrap();
    return if arg.id == Some(Rules::Identifier) {
        Ok(FunctionArgument {
            identifier: Rc::new(arg.matched[0].unwrap_str()),
            typing: type_,
        })
    } else {
        Err(format!(
            "Expected Identifier for function argument but got {:?}",
            arg.id.unwrap()
        ))
    };
}

fn signature_repr(ast: &AST<Rules>) -> Result<(String, Vec<FunctionArgument>), String> {
    if ast.id.is_none() || ast.id.unwrap() != Rules::FunctionSignature {
        return Err(format!(
            "Expected a FunctionSignature AST but got {:?}",
            ast.id
        ));
    }

    let function_name = ast.matched[1].unwrap_str();
    let mut arguments = Vec::new();

    if ast.children.len() > 2 {
        let node = &ast.children[2];
        match node.id {
            None => {}
            Some(Rules::Argument) => {
                let result = argument_repr(ast);
                if result.is_err() {
                    return Err(result.unwrap_err());
                }
                arguments.push(result.unwrap());
            }
            Some(Rules::Arguments) => {
                for child in &node.children {
                    let result = argument_repr(child);
                    if result.is_err() {
                        return Err(result.unwrap_err());
                    }
                    arguments.push(result.unwrap());
                }
            }
            _ => panic!("Expected None, Argument or Arguments but got {:?}", node.id),
        }
    }

    Ok((function_name, arguments))
}

pub fn expression_repr(ast: &AST<Rules>) -> Result<Expression, String> {
    if ast.id.is_none() || ast.id.unwrap() != Rules::Expression {
        return Err(format!("Expected Expression but got {}", ast));
    }

    let body = &ast.children[0];
    let rule = body.id;

    match rule {
        Some(Rules::Identifier) => Ok(Expression::Identifier(ast.matched[0].unwrap_str())),
        Some(Rules::Number) => Ok(Expression::Number(
            ast.matched[0].unwrap_str().parse::<i64>().unwrap(),
        )),
        Some(Rules::FunctionCall) => {
            let mut parameters = Vec::new();
            for child in &body.children[2].children {
                if child.id == Some(Rules::Expression) {
                    let result = expression_repr(child);
                    if result.is_err() {
                        return Err(result.unwrap_err());
                    }
                    parameters.push(result.unwrap());
                }
            }
            Ok(Expression::FunctionCall(FunctionCall {
                name: ast.matched[0].unwrap_str(),
                parameters,
            }))
        }
        Some(Rules::TypeConstructor) => {
            let mut parameters = Vec::new();
            for child in &body.children[4].children {
                if child.id == Some(Rules::Expression) {
                    let result = expression_repr(child);
                    if result.is_err() {
                        return Err(result.unwrap_err());
                    }
                    parameters.push(result.unwrap());
                }
            }
            Ok(Expression::TypeConstructor(
                ast.matched[0].unwrap_str(),
                ast.matched[2].unwrap_str(),
                parameters,
            ))
        }
        Some(Rules::WithBlock) => {
            let mut elements = Vec::new();
            for child in &body.children[1].children {
                let identifier = child.children[0].matched[0].unwrap_str();
                let expr = expression_repr(&child.children[2]);
                if expr.is_err() {
                    return Err(expr.unwrap_err());
                }
                elements.push((Rc::new(identifier), expr.unwrap()));
            }
            let result = expression_repr(&body.children[2]);
            if result.is_err() {
                return Err(result.unwrap_err());
            }
            Ok(Expression::WithBlock(elements, Rc::new(result.unwrap())))
        }
        Some(Rules::IfExpression) => {
            let condition = expression_repr(&body.children[1]);
            if condition.is_err() {
                return Err(condition.unwrap_err());
            }
            let true_branch = expression_repr(&body.children[2]);
            if true_branch.is_err() {
                return Err(true_branch.unwrap_err());
            }
            let false_branch = expression_repr(&body.children[3]);
            if false_branch.is_err() {
                return Err(false_branch.unwrap_err());
            }

            Ok(Expression::If(
                Rc::new(condition.unwrap()),
                Rc::new(true_branch.unwrap()),
                Rc::new(false_branch.unwrap()),
            ))
        }
        _ => Err(format!(
            "Expected WithExpression, IfExpression, FunctionCall, Identifier or Number, but got {}",
            body
        )),
    }
}

fn pattern_matching_repr(ast: &AST<Rules>) -> Result<Vec<(Pattern, Expression)>, String> {
    let mut bodies = Vec::new();
    for pattern in &ast.children {
        if pattern.id != Some(Rules::Pattern) {
            return Err(format!("Expected Pattern but got {}", pattern));
        }

        let mut components = Vec::new();
        for component in &pattern.children[0].children {
            if component.matched[0] != Token::str(",") {
                let destructuring = match component.id {
                    Some(Rules::Identifier) => DestructuringComponent::Identifier(Rc::new(
                        component.matched[0].unwrap_str(),
                    )),
                    Some(Rules::Destructuring) => {
                        let result = destructuring_repr(&component);
                        if result.is_err() {
                            return Err(result.unwrap_err());
                        }
                        DestructuringComponent::Destructuring(result.unwrap())
                    }
                    Some(Rules::Number) => DestructuringComponent::Number(
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

        let result = expression_repr(&pattern.children[1].children[1]);
        if result.is_err() {
            return Err(result.unwrap_err());
        }

        bodies.push((Pattern { components }, result.unwrap()));
    }
    return Ok(bodies);
}

fn function_repr(ast: &AST<Rules>) -> Result<(String, FunctionDefinition), String> {
    if ast.id.is_none() || ast.id.unwrap() != Rules::FunctionDef {
        return Err(format!("Expected a FunctionDef AST but got {:?}", ast.id));
    }

    let result = signature_repr(&ast.children[0]);
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
        let result = expression_repr(&body.children[1]);
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
        let result = pattern_matching_repr(&body.children[1]);
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
        name: name.clone(),
        arguments,
        bodies,
    };

    Ok((name, definition))
}

fn type_variant_repr(ast: &AST<Rules>) -> Result<(String, TypeVariant), String> {
    let name = ast.matched[0].unwrap_str();
    if ast.children[1].id == Some(Rules::Elements) {
        let mut components = Vec::new();
        for child in &ast.children[1].children {
            let result = type_repr(child);
            if result.is_err() {
                return Err(result.unwrap_err());
            }
            components.push(result.unwrap());
        }
        return Ok((name.clone(), TypeVariant::Cartesian(name, components)));
    }

    Ok((name.clone(), TypeVariant::Constant(name)))
}

fn type_definition_repr(ast: &AST<Rules>) -> Result<(Rc<String>, TypeDefinition), String> {
    if ast.id.is_none() || ast.id.unwrap() != Rules::TypeDef {
        return Err(format!("Expected a TypeDef AST but got {:?}", ast.id));
    }

    if ast.children[1].id == Some(Rules::TypeParameter) {
        return Err(format!(
            "New type name cannot be a type parameter, got {}",
            ast
        ));
    }

    let _type = type_repr(&ast.children[1]);
    if _type.is_err() {
        return Err(_type.unwrap_err());
    }
    let _type = _type.unwrap();

    let mut variants = HashMap::new();
    for child in &ast.children[3].children {
        if child.id == Some(Rules::TypeDef) {
            let result = type_variant_repr(&child);
            if result.is_err() {
                return Err(result.unwrap_err());
            }
            let (name, variant) = result.unwrap();
            variants.insert(name, variant);
        }
    }

    let definition = TypeDefinition {
        def: _type.clone(),
        variants: variants,
    };

    Ok((_type.name(), definition))
}

pub fn to_repr(ast: &AST<Rules>) -> Result<Program, String> {
    if ast.id.is_none() || ast.id.unwrap() != Rules::Program {
        return Err(format!("Expected a Program AST but got {:?}", ast.id));
    }

    let mut functions = BTreeMap::new();
    let mut types = HashMap::new();
    for node in &ast.children {
        match node.id {
            Some(Rules::FunctionDef) => {
                let result = function_repr(node);
                if result.is_err() {
                    return Err(result.unwrap_err());
                }

                let (name, definition) = result.unwrap();
                functions.insert(Rc::new(name), definition);
            }
            Some(Rules::TypeDef) => {
                let result = type_definition_repr(node);
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

    Ok(Program { functions, types })
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::{
        compiler::{grammar::program_parser, internal_repr::to_repr},
        parser::combinators::ParserCombinator,
        parser::token_stream::TokenStream,
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

        let result = to_repr(&result.ast);
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
