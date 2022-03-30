use chumsky::prelude::*;

#[derive(Debug)]
pub enum Lit {
    Int(i64),
    Str(String),
    Flt(f32)
}

#[derive(Debug)]
pub enum Expr {
    Lit(Lit),
    Var(String),
    Type(String),

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Call(Box<Expr>, Vec<Expr>),

    Assign {
        name: Box<Expr>,
        typ: Box<Expr>,
        rhs: Box<Expr>
    },

    Func {
        name: Box<Expr>,
        return_type: Box<Expr>,
        args: Vec<Box<Expr>>,
        body: Box<Vec<Expr>>
    },

    Return {
        value: Box<Expr>
    }
}



pub fn parse() -> impl Parser<char, Vec<Expr>, Error=Simple<char>> {
    let ident = text::ident().padded();

    let stmt = recursive(|stmt| {
        let expr = recursive(|expr| {
            let int = text::int(10)
                .map(|s: String| Expr::Lit(Lit::Int(s.parse().unwrap())))
                .padded();

            let str =
                just('"')
                .ignore_then(filter(|c| *c != '\\' && *c != '"').repeated())
                .then_ignore(just('"'))
                .map(|b| Expr::Lit(Lit::Str(String::from_iter(b))));

            let arg_list =
                expr.clone()
                .separated_by(just(',').padded())
                .at_least(0)
                .delimited_by(just('(').padded(), just(')').padded());

            let func_call =
                ident
                .then(arg_list)
                .map(|(lhs, rhs)| {
                    Expr::Call(Box::new(Expr::Var(lhs)), rhs)
                });
            
            let atom =
                int
                .or(str)
                .or(expr.clone().delimited_by(just('('), just(')')))
                .or(func_call);
    
            let op = |c| just(c).padded();
    
            let unary =
                op('-')
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
    
        let decl =
            ident
            .then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .map(|((typ, lhs), rhs)| {
                Expr::Assign {
                    typ: Box::new(Expr::Type(typ)),
                    name: Box::new(Expr::Var(lhs)),
                    rhs: Box::new(rhs)
                }
            })
            .then_ignore(just(';'))
            .padded();

        let r#return =
            text::keyword("ret")
            .or(
                text::keyword("return")
            )
            .ignore_then(expr.clone())
            .map(|e| {
                Expr::Return {
                    value: Box::new(e)
                }
            })
            .then_ignore(just(';'))
            .padded();
    
        let elem = decl.or(r#return).or(expr);
        
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
        .ignore_then(ident) // Type
        .then(ident)
        .then(ident_list)
        .then(block)
        .map(|(((typ, name), args), block)| {
            Expr::Func {
                return_type: Box::new(Expr::Type(typ)),
                name: Box::new(Expr::Var(name)),
                args: args.into_iter().map(Expr::Var).map(Box::new).collect(),
                body: Box::new(block)
            }
        });

    fcn_def.padded().repeated().then_ignore(end())
}