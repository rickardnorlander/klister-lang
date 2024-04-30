#![allow(unused_assignments)]

use std::fmt;

use num_bigint::BigInt;
use once_cell::sync::Lazy;
use regex::Regex;

use crate::ast::*;
use crate::value::KlisterInteger;

#[derive(Debug)]
pub struct SyntaxError {
    pub remain_len: usize,
    pub info: String,
}

impl SyntaxError {
    pub fn prettyprint(&self, fullstr: &str) -> String {
        // todo: row/col is bugged but seems to be close to the right place at least..
        let consumed_len = fullstr.len() - self.remain_len;
        let prefix = &fullstr[0..consumed_len];
        let sp = prefix.split('\n').collect::<Vec<_>>();
        let line = sp.len();
        let col = match sp.last() {
            Some(last) => {last.len()}
            None => 0
        };
        return format!("Syntax error \"{}\" at line {} col {}", self.info, line, col);
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Oh no, something bad went down")
    }
}

macro_rules! synerr {
    ($remaining:ident, $info:expr) => {
        return Err(SyntaxError{remain_len: $remaining.len(), info: $info.to_string()})
    };
}

type ParseResult<T> = Result<T, SyntaxError>;

trait SyntaxErrorTrait<T> {
    fn context(self, remaining: &str, info: &str) -> ParseResult<T>;
}

impl<T> SyntaxErrorTrait<T> for Option<T> {
    fn context(self, remaining: &str, info: &str) -> ParseResult<T>{
        self.ok_or_else(||SyntaxError{remain_len: remaining.len(), info: info.to_string()})
    }
}

fn parse_import(remaining: &mut &str) -> ParseResult<KlisterStatement> {
    skip_space(remaining);
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^import ([a-z0-9.]+) ([a-z0-9.]+) ([a-z][a-z0-9]*)\(((?:[a-z, ]+)?)\)").unwrap());
    let caps = RE.captures(remaining).context(remaining, "Failed to parse import")?;
    let args = if caps[4].trim() == "" {
        Vec::new()
    } else {
        caps[4].split(",").map(|x| x.trim().to_string()).collect::<Vec<_>>()
    };

    *remaining = &remaining[caps[0].len()..];

    return Ok(KlisterStatement::Import(caps[1].to_string(), caps[3].to_string(), caps[2].to_string(), args));
}

fn parse_catch_expr(s: &mut&str) -> ParseResult<KlisterExpression> {
    skip_space(s);
    consume("?(", s)?;
    let res = KlisterExpression::CatchExpr(Box::new(parse_precedence_1(s)?));
    consume(")", s)?;
    return Ok(res);
}
use crate::value::KlisterStr;

fn parse_precedence_6(s: &mut& str) -> ParseResult<KlisterExpression> {
    skip_space(s);

    let mut in_chars = s.char_indices();

    let first_char = in_chars.next().context(s, "Empty string")?.1;

    if first_char == '"' {
        let mut out_str = String::new();
        loop {
            let (pos, c) = in_chars.next().context(s, "Unterminated string literal")?;
            if c == '"' {
                *s = &s[pos+c.len_utf8()..];
                return Ok(KlisterExpression::Literal(Box::new(KlisterStr{val: out_str})));
            }
            if c == '\\' {
                out_str.push(in_chars.next().context(s,"Incomplete escape sequence")?.1);
            }
            out_str.push(c);
        }
    }
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[+-]?[0-9]+").unwrap());
    if let Some(caps) = RE.captures(s) {
        let result = caps[0].parse::<BigInt>().ok().context(s, "Invalid numeric literal")?;
        *s = &s[caps[0].len()..];
        return Ok(KlisterExpression::Literal(KlisterInteger::wrapn(result)));
    }
    static SHELL_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^`").unwrap());
    if SHELL_RE.is_match(s) {
        return parse_shell_main(s);
    }

    static CATCH_EXPR_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^\?\(").unwrap());
    if CATCH_EXPR_RE.is_match(s) {
        return parse_catch_expr(s);
    }

    static PAREN: Lazy<Regex> = Lazy::new(|| Regex::new(r"^\(").unwrap());
    if PAREN.is_match(s) {
        consume("(", s)?;
        let ret = parse_precedence_1(s);
        skip_space(s);
        consume(")", s)?;
        return ret;
    }

    if let Ok(id) = parse_id(s) {
        return Ok(KlisterExpression::Variable(id));
    }
    synerr!(s, format!("Parse precedence 6 failed at {}", s));
}

fn parse_id(remaining: &mut& str) -> ParseResult<String> {
    skip_space(remaining);
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^([A-Za-z_][A-Za-z0-9_]*)").unwrap());
    let caps = RE.captures(remaining).context(remaining, "Failed to parse id")?;
    let result = &caps[1];
    *remaining = &remaining[result.len()..];
    return Ok(result.to_string());
}

fn consume(prefix: &str, remaining: &mut&str) -> ParseResult<()> {
    if remaining.is_empty() || remaining.len() < prefix.len() { synerr!(remaining, format!("Expected token {} not found", prefix));};
    for (a, b) in prefix.chars().zip(remaining.chars()) {
        if a != b {
            synerr!(remaining, format!("Expected token {} not found", prefix));
        }
    }
    *remaining = &remaining[prefix.len()..];
    return Ok(());
}

fn parse_call_args(remaining: &mut& str) -> ParseResult<Vec::<KlisterExpression>> {
    let mut args = Vec::<KlisterExpression>::new();

    let parse_arg = parse_precedence_1;

    let arg0_r = parse_arg(remaining);
    if let Ok(arg0) = arg0_r {
        args.push(arg0);
        loop {
            skip_space(remaining);
            if consume(",", remaining).is_err() {
                break;
            }
            args.push(parse_arg(remaining)?);
        }
    }
    return Ok(args);
}

fn parse_precedence_5(s: &mut&str) -> ParseResult<KlisterExpression> {
    let mut ret = parse_precedence_6(s)?;
    while !s.is_empty() {
        skip_space(s);
        if consume(".", s).is_ok() {
            ret = KlisterExpression::Dot(Box::new(ret), parse_id(s)?);
        } else if consume("(", s).is_ok() {
            ret = KlisterExpression::Call(Box::new(ret), parse_call_args(s)?);
            skip_space(s);
            consume(")", s)?;
        } else if consume("[", s).is_ok() {
            let index = parse_precedence_1(s);
            ret = KlisterExpression::Index(Box::new(ret), Box::new(index?));
            skip_space(s);
            consume("]", s)?;
        } else {
            break;
        }
    }
    return Ok(ret);
}

fn parse_precedence_4point5(s: &mut&str) -> ParseResult<KlisterExpression> {
    let parse_next = parse_precedence_5;

    skip_space(s);
    if consume("!", s).is_ok() {
        return Ok(KlisterExpression::Not(Box::new(parse_precedence_4point5(s)?)));
    } else {
        return parse_next(s);
    }
}

fn parse_precedence_4(s: &mut&str) -> ParseResult<KlisterExpression> {
    let parse_next = parse_precedence_4point5;

    let mut ret = parse_next(s)?;
    while !s.is_empty() {
        skip_space(s);
        if consume("*", s).is_ok() {
            ret = KlisterExpression::BinOp(Operation::Mul, Box::new(ret), Box::new(parse_next(s)?));
        } else if consume("/", s).is_ok() {
            ret = KlisterExpression::BinOp(Operation::Div, Box::new(ret), Box::new(parse_next(s)?));
        } else {
            break;
        }
    }
    return Ok(ret);
}

fn parse_precedence_3(s: &mut&str) -> ParseResult<KlisterExpression> {
    let parse_next = parse_precedence_4;
    let mut ret = parse_next(s)?;
    while !s.is_empty() {
        skip_space(s);
        if consume("+", s).is_ok() {
            ret = KlisterExpression::BinOp(Operation::Add, Box::new(ret), Box::new(parse_next(s)?));
        } else if consume("-", s).is_ok() {
            ret = KlisterExpression::BinOp(Operation::Sub, Box::new(ret), Box::new(parse_next(s)?));
        } else {
            break;
        }
    }
    return Ok(ret);
}

fn parse_precedence_2(s: &mut&str) -> ParseResult<KlisterExpression> {
    let parse_next = parse_precedence_3;
    let mut ret = parse_next(s)?;
    if !s.is_empty() {
        skip_space(s);
        if consume("<=", s).is_ok() {
            ret = KlisterExpression::BinOp(Operation::Lte, Box::new(ret), Box::new(parse_next(s)?));
        } else if consume(">=", s).is_ok() {
            ret = KlisterExpression::BinOp(Operation::Gte, Box::new(ret), Box::new(parse_next(s)?));
        } else if consume("<", s).is_ok() {
            ret = KlisterExpression::BinOp(Operation::Lt, Box::new(ret), Box::new(parse_next(s)?));
        } else if consume(">", s).is_ok() {
            ret = KlisterExpression::BinOp(Operation::Gt, Box::new(ret), Box::new(parse_next(s)?));
        } else if consume("==", s).is_ok() {
            ret = KlisterExpression::BinOp(Operation::Eq, Box::new(ret), Box::new(parse_next(s)?));
        } else if consume("!=", s).is_ok() {
            ret = KlisterExpression::BinOp(Operation::Ne, Box::new(ret), Box::new(parse_next(s)?));
        }
    }
    return Ok(ret);
}

fn parse_precedence_1point2(s: &mut&str) -> ParseResult<KlisterExpression> {
    let parse_next = parse_precedence_2;
    let mut ret = parse_next(s)?;
    if !s.is_empty() {
        skip_space(s);
        if consume("&&", s).is_ok() {
            ret = KlisterExpression::BinOp(Operation::And, Box::new(ret), Box::new(parse_next(s)?));
        }
    }
    return Ok(ret);
}

fn parse_precedence_1(s: &mut&str) -> ParseResult<KlisterExpression> {
    let parse_next = parse_precedence_1point2;
    let mut ret = parse_next(s)?;
    if !s.is_empty() {
        skip_space(s);
        if consume("||", s).is_ok() {
            ret = KlisterExpression::BinOp(Operation::Or, Box::new(ret), Box::new(parse_next(s)?));
        }
    }
    return Ok(ret);
}

fn parse_precedence_0(s: &mut&str) -> ParseResult<KlisterStatement> {
    let mut s2_x = *s;
    let s2:&mut&str = &mut s2_x;
    let idid = parse_id(s2);
    skip_space(s2);
    // todo: Need to handle == 
    if idid.is_ok() && consume("=", s2).is_ok() {
        let ret = KlisterStatement::Assign(idid?, parse_precedence_1(s2)?);
        *s = *s2;
        return Ok(ret);
    }
    return Ok(KlisterStatement::Expression(parse_precedence_1(s)?));
}

fn consume_remaining_line(s: &mut&str) -> ParseResult<()> {
    loop {
        let Some(c) = s.chars().next() else {
            synerr!(s, "Incomplete line");
        };
        *s = &s[c.len_utf8()..];
        if c == '\n' {
            return Ok(());
        }
    }
}

fn skip_space_and_newlines(s: &mut&str) -> ParseResult<()>  {
    while let Some(c) = s.chars().next() {
        if c == ' ' || c == '\n' {
            *s = &s[c.len_utf8()..];
        } else if c == '#' {
            *s = &s[c.len_utf8()..];
            consume_remaining_line(s)?;
        } else {
            break;
        }
    }
    Ok(())
}

fn skip_space(s: &mut&str) {
    while let Some(c) = s.chars().next() {
        if c != ' ' {break;}
        *s = &s[c.len_utf8()..];
    }
}

fn consume_statement_enders(s: &mut&str) -> ParseResult<()> {
    let mut newline = false;
    while let Some(c) = s.chars().next() {
        if c == '\n' {
             newline = true;
        } else if c == ' ' {
        } else if c == '#' {
            *s = &s[c.len_utf8()..];
            consume_remaining_line(s)?;
            newline = true;
            continue;
        }
        else {
            break;
        }
        *s = &s[c.len_utf8()..];
    }
    if !newline {
        synerr!(s, "No statement ender found");
    }
    return Ok(());
}

fn parse_block(remaining: &mut&str) -> ParseResult<KlisterStatement> {
    let mut ret = Vec::new();
    skip_space_and_newlines(remaining)?;
    consume("{", remaining)?;
    skip_space_and_newlines(remaining)?;
    loop {
        if consume("}", remaining).is_ok() {
            return Ok(KlisterStatement::Block(ret));
        }
        ret.push(parse_statement(remaining)?);
        consume_statement_enders(remaining)?; // Should the last statement require a newline? Might change.
    }
}

fn parse_while(s: &mut&str) -> ParseResult<KlisterStatement> {
    skip_space_and_newlines(s)?;
    consume("while", s)?;
    skip_space_and_newlines(s)?;
    consume("(", s)?;
    let expr = parse_precedence_1(s)?;
    skip_space_and_newlines(s)?;
    consume(")", s)?;
    let body = parse_block(s)?;
    return Ok(KlisterStatement::While(expr, Box::new(body)));
}

fn parse_if(s: &mut&str) -> ParseResult<KlisterStatement> {
    skip_space_and_newlines(s)?;
    consume("if", s)?;
    skip_space_and_newlines(s)?;
    consume("(", s)?;
    let expr = parse_precedence_1(s)?;
    skip_space_and_newlines(s)?;
    consume(")", s)?;
    let body = parse_block(s)?;

    let mut s2_x = *s;
    let s2 = &mut s2_x;
    skip_space_and_newlines(s2)?;
    let elseres = consume("else", s2);
    if elseres.is_err() {
        return Ok(KlisterStatement::If(expr, Box::new(body), None));
    }

    skip_space_and_newlines(s)?;
    consume("else", s)?;
    let elsebody = parse_block(s)?;

    return Ok(KlisterStatement::If(expr, Box::new(body), Some(Box::new(elsebody))));
}

fn parse_function(s: &mut&str) -> ParseResult<KlisterStatement> {
    skip_space_and_newlines(s)?;
    consume("function", s)?;
    skip_space_and_newlines(s)?;
    let name = parse_id(s)?;
    skip_space_and_newlines(s)?;
    consume("(", s)?;
    // Parameters not supported for now
    skip_space_and_newlines(s)?;
    consume(")", s)?;
    let body = parse_block(s)?;

    return Ok(KlisterStatement::Function(name, Box::new(body)));
}

fn get_shell_escape(remaining: &str, c: char) -> ParseResult<char> {
    Ok(match c {
        // Should this be enabled?
        // 'n' => '\n',
        '\\' => '\\',
        '"' => '"',
        '`' => '`',
        '|' => '|',
        '$' => '$',
        '*' => '*',
        _ => {
            synerr!(remaining, "Unknown escape sequence");
        }
    })
}

fn parse_interpolation(s: &mut&str) -> ParseResult<GlobPart> {
    if consume("{", s).is_ok() {
        let ret = parse_precedence_1(s)?;
        consume("}", s)?;
        return Ok(GlobPart::GlobPartInterpolation(Box::new(ret)));
    }
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[A-Za-z_]+").unwrap());
    let caps = RE.captures(s).context(s, "Invalid interpolation")?;
    *s = &s[caps[0].len()..];
    return Ok(GlobPart::GlobPartInterpolation(Box::new(KlisterExpression::Variable(caps[0].to_string()))));
}

fn parse_shell_quote(s: &mut&str) -> ParseResult<String> {
    consume("\"", s)?;
    let mut ret = String::new();
    loop {
        let c = s.chars().next().context(s, "Unterminated quote")?;
        *s = &s[c.len_utf8()..];
        // Maybe enable interpolation in quotes?
        if c == '"' {
            return Ok(ret);
        } else if c == '\n' {
            synerr!(s, "Incomplete quote")
        } else if c == '\\' {
            let c = s.chars().next().context(s, "Unterminated escape")?;
            ret.push(get_shell_escape(s, c)?);
            *s = &s[c.len_utf8()..];
        } else {
            ret.push(c);
        }
    }
}

fn parse_glob_part(s: &mut&str) -> ParseResult<Option<GlobPart>> {
    let mut ret = String::new();
    while let Some(c) = s.chars().next() {
        if c == ' ' || c == '`' || c == '\n' || c == '|' || c == '`' {
            break;
        } else if c == '*' {
            if ret.is_empty() {
                *s = &s[c.len_utf8()..];
                return Ok(Some(GlobPart::GlobPartAsterisk));
            }
            return Ok(Some(GlobPart::GlobPartS(ret)));
        } else if c == '$' {
            if ret.is_empty() {
                *s = &s[c.len_utf8()..];
                return parse_interpolation(s).map(|x| Some(x));
            }
            return Ok(Some(GlobPart::GlobPartS(ret)));
        } else if c == '"' {
            ret.extend(parse_shell_quote(s));
        } else if c == '\\' {
            *s = &s[c.len_utf8()..];

            let c = s.chars().next().context(s, "Incomplete escape")?;
            ret.push(get_shell_escape(s, c)?);
            *s = &s[c.len_utf8()..];
        } else {
            ret.push(c);
            *s = &s[c.len_utf8()..];
        }
    }
    if !ret.is_empty() {return Ok(Some(GlobPart::GlobPartS(ret)));} else {return Ok(None)}
}

fn parse_argon(s: &mut&str) -> ParseResult<Option<Argon>> {
    let mut ret = Vec::<GlobPart>::new();
    loop {
        let xx = parse_glob_part(s)?;
        if let Some(yy) = xx {
            ret.push(yy);
        } else {
            break;
        }
    }
    if ret.is_empty() {
        return Ok(None);
    }
    return Ok(Some(Argon::ArgonGlob(ret)));
}

fn skip_shell_separators(s: &mut&str) {
    while let Some(c) = s.chars().next() {
        if c != ' ' {
            break;
        }
        *s = &s[c.len_utf8()..];
    }
}

fn parse_shell_command(s: &mut&str) -> ParseResult<ShellCommand> {
    let mut shell_tokens = Vec::new();
    loop {
        skip_shell_separators(s);
        let token_opt_res = parse_argon(s);
        let token_opt = token_opt_res?;
        let Some(token) = token_opt else {
            break;
        };
        shell_tokens.push(token);
    }
    let mut it = shell_tokens.into_iter();
    let cmd = it.next().context(s, "Shell expression must have command")?;
    let args = it.collect();
    return Ok(ShellCommand{command: cmd, args});
}

fn parse_shell_main(s: &mut&str) -> ParseResult<KlisterExpression> {
    // todo: Possibly rethink newline handling in shell environment.
    skip_space(s);
    consume("`", s)?;
    let mut cmds = Vec::<ShellCommand>::new();
    skip_shell_separators(s);
    let parse_shell_command_res = parse_shell_command(s);
    cmds.push(parse_shell_command_res?);
    loop {
        skip_shell_separators(s);
        if consume("`", s).is_ok() {break;}
        consume("|", s)?;
        skip_shell_separators(s);
        let parse_shell_command_res = parse_shell_command(s);
        cmds.push(parse_shell_command_res?);
    }
    if cmds.len() == 0 {
        synerr!(s, "No cmds");
    }
    return Ok(KlisterExpression::ShellPipeline(ShellPipelineS::new(cmds)))
}

fn parse_statement(remaining: &mut&str) -> ParseResult<KlisterStatement> {
    skip_space_and_newlines(remaining)?;
    static IMPORT_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^import ").unwrap());
    static WHILE_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^while[ (]").unwrap());
    static IF_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^if[ (]").unwrap());
    static FUNCTION_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^function ").unwrap());
    if IMPORT_RE.is_match(remaining) {
        return parse_import(remaining);
    } else if WHILE_RE.is_match(remaining) {
        return parse_while(remaining);
    } else if IF_RE.is_match(remaining) {
        return parse_if(remaining);
    } else if FUNCTION_RE.is_match(remaining) {
        return parse_function(remaining);
    } else {
        return parse_precedence_0(remaining);
    }
}

fn parse_statements(remaining: &mut&str) -> ParseResult<Vec<KlisterStatement>> {
    let mut ret = Vec::new();
    skip_space_and_newlines(remaining)?;
    while !remaining.is_empty() {
        ret.push(parse_statement(remaining)?);
        consume_statement_enders(remaining)?;
    }
    return Ok(ret)
}

pub fn parse_ast(s: &str) -> ParseResult<KlisterStatement> {
    // Add a trailing newline if the file doesn't end in one.
    let mut appended = String::new();
    let s = if s.chars().last() == Some('\n') {
        s
    } else {
        appended = format!("{s}\n");
        &appended
    };

    let mut remaining: &str = &s;
    let statements = parse_statements(&mut remaining)?;
    if !remaining.is_empty() {
        synerr!(remaining, "Trailing garbage");
    }
    return Ok(KlisterStatement::Block(statements));
}