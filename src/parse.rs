use chumsky::prelude::*;
use crate::eval::Type;

#[derive(Debug)]
pub struct FuncArgument {
    typ: Type,
    name: String
}

#[derive(Debug)]
pub enum Expr {
    Int(i64),
    Var(String),
    Type(Type),

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Call(Box<Expr>, Vec<Expr>),

    Assign {
        name: Box<Expr>,
        rhs: Box<Expr>
    },

    Func {
        name: Box<Expr>,
        args: Vec<Box<Expr>>,
        body: Box<Vec<Expr>>
    }
}



pub fn parse() -> impl Parser<char, Vec<Expr>, Error=Simple<char>> {
    let ident = text::ident().padded();

    let stmt = recursive(|stmt| {
        let expr = recursive(|expr| {
            let int = text::int(10)
                .map(|s: String| Expr::Int(s.parse().unwrap()))
                .padded();

            let arg_list =
                expr.clone()
                .separated_by(just(',').padded())
                .at_least(0)
                .delimited_by(just('(').padded(), just(')').padded());

            let func_call = ident
                .then(arg_list)
                .map(|(lhs, rhs)| {
                    Expr::Call(Box::new(Expr::Var(lhs)), rhs)
                });
            
            let atom =
                int
                .or(expr.clone().delimited_by(just('('), just(')')))
                .or(func_call);
    
            let op = |c| just(c).padded();
    
            let unary = op('-')
                .repeated()
                .then(atom)
                .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

            let product = unary.clone()
                .then(op('*').to(Expr::Mul as fn(_, _) -> _)
                    .or(op('/').to(Expr::Div as fn(_, _) -> _))
                    .then(unary)
                    .repeated())
                .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
    
            let sum = product.clone()
                .then(op('+').to(Expr::Add as fn(_, _) -> _)
                    .or(op('-').to(Expr::Sub as fn(_, _) -> _))
                    .then(product)
                    .repeated())
                .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

            sum.padded()
        });
    
        let decl = text::keyword("var")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .map(|(lhs, rhs)| {
                Expr::Assign {
                    name: Box::new(Expr::Var(lhs)),
                    rhs: Box::new(rhs)
                }
            })
            .padded();
    
        let elem = decl.or(expr);
        
        elem.padded()
    });

    let block = stmt.clone()
        .separated_by(just(';').padded())
        .delimited_by(just('{').padded(), just('}').padded())
        .padded();

    let ident_list =
        ident.clone().separated_by(just(',')).at_least(0)
        .delimited_by(just('('), just(')'));

    let fcn_def =
        text::keyword("fcn")
        .ignore_then(ident)
        .then(ident_list)
        .then(block)
        .map(|((name, args), block)| {
            Expr::Func {
                name: Box::new(Expr::Var(name)),
                args: args.into_iter().map(Expr::Var).map(Box::new).collect(),
                body: Box::new(block)
            }
        });

    fcn_def.padded().repeated().then_ignore(end())
}