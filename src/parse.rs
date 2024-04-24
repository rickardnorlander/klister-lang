use crate::ast::*;
use regex::Regex;
use once_cell::sync::Lazy;
use anyhow::anyhow;
use anyhow::Context;

pub fn parse_import(remaining: &mut &str) -> anyhow::Result<KlisterStatement> {
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^import ([a-z0-9.]+) ([a-z0-9.]+) ([a-z][a-z0-9]*)\(((?:[a-z, ]+)?)\)").unwrap());
    let caps = RE.captures(remaining).unwrap();
    let args = if caps[4].trim() == "" {
        Vec::new()
    } else {
        caps[4].split(",").map(|x| x.trim()).collect::<Vec<_>>()
    };

    *remaining = &remaining[caps[0].len()..];
    return Ok(im(&caps[1], &caps[3], &caps[2], &args));
}

pub fn parse_literal2(s: &mut& str) -> anyhow::Result<KlisterExpression> {
    let mut in_chars = s.char_indices();

    let first_char = in_chars.next().context("Empty string in parse_literal2")?.1;

    if first_char == '"' {
        let mut out_str = String::new();
        loop {
            let (pos, c) = in_chars.next().context("Unterminated string literal")?;
            if c == '"' {
                *s = &s[pos+c.len_utf8()..];
                return Ok(KlisterExpression::Literal(KlisterValue::CS(out_str)));
            }
            if c == '\\' {
                out_str.push(in_chars.next().context("Incomplete escape sequence")?.1);
            }
            out_str.push(c);
        }
    }
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[+-]?[0-9]+").unwrap());
    if let Some(caps) = RE.captures(s) {
        let result = caps[0].parse::<i32>().ok().context("Invalid numeric literal")?;
        *s = &s[caps[0].len()..];
        return Ok(KlisterExpression::Literal(KlisterValue::Int(result)));
    }
    static SHELL_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^`").unwrap());
    if SHELL_RE.is_match(s) {
        return parse_shell_main(s);
    }

    if let Ok(id) = parse_id(s) {
        return Ok(KlisterExpression::Variable(id));
    }
    return Err(anyhow!(""));
}

pub fn parse_id(remaining: &mut& str) -> anyhow::Result<String> {
    //let in_chars = s.chars().collect::<Vec<_>>();
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^([A-Za-z_][A-Za-z0-9_]*)").unwrap());
    let caps = RE.captures(remaining).context("Failed to parse id")?;
    let result = &caps[1];
    *remaining = &remaining[result.len()..];
    return Ok(result.to_string());
}

pub fn consume(tocon: &str, remaining: &mut&str) -> anyhow::Result<()> {
    if remaining.is_empty() || remaining.len() < tocon.len() { return Err(anyhow!(""))};
    for (a, b) in tocon.chars().zip(remaining.chars()) {
        if a != b {
            return Err(anyhow!(""));
        }
    }
    *remaining = &remaining[tocon.len()..];
    return Ok(());
}

pub fn parse_call(remaining: &mut& str) -> anyhow::Result<KlisterExpression> {
    let fnname = parse_id(remaining)?;
    consume("(", remaining)?;
    let mut args = Vec::<KlisterExpression>::new();
    let arg0_r = parse_add(remaining);
    if let Ok(arg0) = arg0_r {
        args.push(arg0);
        loop {
            if consume(",", remaining).is_err() {
                break;
            }
            args.push(parse_add(remaining)?);
        }
    }
    consume(")", remaining)?;
    return Ok(KlisterExpression::Call(fnname, args));
}

pub fn parse_index(remaining: &mut&str) -> anyhow::Result<KlisterExpression> {
    let arr = parse_id(remaining)?;
    consume("[", remaining)?;
    let index = parse_literal2(remaining);
    consume("]", remaining)?;
    return Ok(KlisterExpression::Index(Box::new(KlisterExpression::Variable(arr)), Box::new(index?)));
}

pub fn parse_call_or_lower(s: &mut&str) -> anyhow::Result<KlisterExpression> {
    let mut s2_x = *s;
    let s2:&mut&str = &mut s2_x;
    let idid = parse_id(s2);
    if s2.chars().next() == Some('[') {
        //panic!("{:?}", idid);
    }
    if idid.is_ok() && consume("(", s2).is_ok() {
        return parse_call(s);
    }
    if idid.is_ok() && consume("[", s2).is_ok() {
        return parse_index(s);
    }
    return parse_literal2(s);
}

pub fn parse_mul(s: &mut&str) -> anyhow::Result<KlisterExpression> {
    let parse_lower = parse_call_or_lower;

    let mut ret = parse_lower(s)?;
    while !s.is_empty() {
        if consume("*", s).is_ok() {
            ret = KlisterExpression::Mul(Box::new(ret), Box::new(parse_lower(s)?));
        } else if consume("/", s).is_ok() {
            ret = KlisterExpression::Div(Box::new(ret), Box::new(parse_lower(s)?));
        } else {
            break;
        }
    }
    return Ok(ret);
}

pub fn parse_add(s: &mut&str) -> anyhow::Result<KlisterExpression> {
    let mut ret = parse_mul(s)?;
    while !s.is_empty() {
        if consume("+", s).is_ok() {
            ret = KlisterExpression::Add(Box::new(ret), Box::new(parse_mul(s)?));
        } else if consume("-", s).is_ok() {
            ret = KlisterExpression::Sub(Box::new(ret), Box::new(parse_mul(s)?));
        } else {
            break;
        }
    }
    return Ok(ret);
}

pub fn parse_lt(s: &mut&str) -> anyhow::Result<KlisterExpression> {
    let mut ret = parse_add(s)?;
    if !s.is_empty() {
        if consume("<", s).is_ok() {
            ret = KlisterExpression::Lt(Box::new(ret), Box::new(parse_add(s)?));
        } else if consume(">", s).is_ok() {
            ret = KlisterExpression::Gt(Box::new(ret), Box::new(parse_add(s)?));
        } else if consume("<=", s).is_ok() {
            ret = KlisterExpression::Lte(Box::new(ret), Box::new(parse_add(s)?));
        } else if consume(">=", s).is_ok() {
            ret = KlisterExpression::Gte(Box::new(ret), Box::new(parse_add(s)?));
        } else if consume("==", s).is_ok() {
            ret = KlisterExpression::Eq(Box::new(ret), Box::new(parse_add(s)?));
        } else if consume("!=", s).is_ok() {
            ret = KlisterExpression::Ne(Box::new(ret), Box::new(parse_add(s)?));
        }
    }
    return Ok(ret);
}

pub fn parse_catch(s: &mut&str) -> anyhow::Result<KlisterExpression> {
    if consume("?", s).is_err() {
        return parse_lt(s);
    }
    consume("(", s)?;
    let res = KlisterExpression::Catch(Box::new(parse_lt(s)?));
    consume(")", s)?;
    return Ok(res);
}

pub fn parse_assignment_or_lower(s: &mut&str) -> anyhow::Result<KlisterStatement> {
    let mut s2_x = *s;
    let s2:&mut&str = &mut s2_x;
    let idid = parse_id(s2);
    if idid.is_ok() && consume("=", s2).is_ok() {
        let ret = KlisterStatement::Assign(idid?, parse_catch(s2)?);
        *s = *s2;
        return Ok(ret);
    }
    return Ok(KlisterStatement::Expression(parse_catch(s)?));
}

pub fn skip_space_and_newlines(s: &mut&str) {
    loop {
        let Some(c) = s.chars().next() else {break;};
        if c != ' ' && c != '\n' {break;}
        *s = &s[c.len_utf8()..];
    }
}

pub fn consume_statement_enders(s: &mut&str) -> anyhow::Result<()> {
    let s_back = *s;
    let mut newline = false;
    loop {
        let Some(c) = s.chars().next() else {break;};
        if c == '\n' {
             newline = true;
        } else if c != ' ' {
            break
        }
        *s = &s[c.len_utf8()..];
    }
    return if newline {Ok(())} else {Err(anyhow!("No statement ender found {}", s_back))};
}

pub fn parse_block(remaining: &mut&str) -> anyhow::Result<KlisterStatement> {
    let mut ret = Vec::new();
    consume("{", remaining)?;
    skip_space_and_newlines(remaining);
    loop {
        if consume("}", remaining).is_ok() {
            return Ok(KlisterStatement::Block(ret));
        }
        ret.push(parse_statement(remaining)?);
        consume_statement_enders(remaining)?;
    }
}

pub fn parse_while(s: &mut&str) -> anyhow::Result<KlisterStatement> {
    consume("while", s)?;
    skip_space_and_newlines(s);
    consume("(", s)?;
    let expr = parse_lt(s).expect("Todo");
    consume(")", s)?;
    let body = parse_block(s)?;
    return Ok(KlisterStatement::While(expr, Box::new(body)));
}

pub fn parse_if(s: &mut&str) -> anyhow::Result<KlisterStatement> {
    consume("if", s)?;
    skip_space_and_newlines(s);
    consume("(", s)?;
    let expr = parse_lt(s).expect("Todo");
    consume(")", s)?;
    let body = parse_block(s)?;

    let mut s2_x = *s;
    let s2 = &mut s2_x;
    let elseres = consume("else", s2);
    if elseres.is_err() {
        return Ok(KlisterStatement::If(expr, Box::new(body), None));
    }

    consume("else", s)?;
    let elsebody = parse_block(s)?;

    return Ok(KlisterStatement::If(expr, Box::new(body), Some(Box::new(elsebody))));
}

pub fn parse_shell_token(s: &mut&str) -> anyhow::Result<Option<String>> {
    let mut ret = String::new();
    let mut charss = s.chars();
    loop {
        let Some(c) = charss.next() else {break;};
        if c == ' ' || c == '`' || c == '\n' || c == '|' || c == '`' {
            break;
        } else if c == '\\' {
            let c2 = charss.next().context("Incomplete escape")?;
            ret.push(c2);
            
            *s = &s[c.len_utf8() + c2.len_utf8()..];
        } else {
            ret.push(c);
            *s = &s[c.len_utf8()..];
        }
    }
    if !ret.is_empty() {return Ok(Some(ret));} else {return Ok(None)}
}

pub fn skip_shell_separators(s: &mut&str) {
    let mut charss = s.chars();
    loop {
        let Some(c) = charss.next() else {break;};
        if c != ' ' && c != '\n' {
            break;
        }
        *s = &s[c.len_utf8()..];
    }
}

pub fn parse_shell_command(s: &mut&str) -> anyhow::Result<ShellCommand> {
    let mut shell_tokens = Vec::new();
    loop {
        skip_shell_separators(s);
        let token_opt_res = parse_shell_token(s);
        let token_opt = token_opt_res?;
        let Some(token) = token_opt else {
            break;
        };
        shell_tokens.push(token);
    }
    let mut it = shell_tokens.into_iter();
    let cmd = it.next().context("Shell expression must have command")?;
    let args = it.collect();
    return Ok(ShellCommand{command:cmd, args});
}

pub fn parse_shell_main(s: &mut&str) -> anyhow::Result<KlisterExpression> {
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
    if cmds.len() == 0 {return Err(anyhow!("No cmds"))};
    return Ok(KlisterExpression::ShellPipeline(cmds))
}

pub fn parse_statement(remaining: &mut&str) -> anyhow::Result<KlisterStatement> {
    static IMPORT_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^import ").unwrap());
    static WHILE_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^while[ (]").unwrap());
    static IF_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^if[ (]").unwrap());
    if IMPORT_RE.is_match(remaining) {
        return parse_import(remaining);
    } else if WHILE_RE.is_match(remaining) {
        return parse_while(remaining);
    } else if IF_RE.is_match(remaining) {
        return parse_if(remaining);
    } else {
        return parse_assignment_or_lower(remaining);
    }
}

pub fn parse_statements(remaining: &mut&str) -> anyhow::Result<Vec<KlisterStatement>> {
    let mut ret = Vec::new();
    skip_space_and_newlines(remaining);
    while !remaining.is_empty() {
        ret.push(parse_statement(remaining)?);
        consume_statement_enders(remaining)?;
    }
    return Ok(ret)
}

pub fn just_parse_statements(s: &str) -> anyhow::Result<Vec<KlisterStatement>> {
    let mut remaining: &str = &s;
    let result = parse_statements(&mut remaining);
    if remaining.is_empty() {return result} else {return Err(anyhow!("Trailing garbage {}", remaining))}
}

pub fn parse_ast(s: &str) -> anyhow::Result<KlisterStatement> {
    let statements = just_parse_statements(s)?;

    return Ok(KlisterStatement::Block(statements));
}