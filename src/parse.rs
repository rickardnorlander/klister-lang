#![allow(unused_assignments)]

use anyhow::anyhow;
use anyhow::Context;
use num_bigint::BigInt;
use once_cell::sync::Lazy;
use regex::Regex;


use crate::ast::*;

fn parse_import(remaining: &mut &str) -> anyhow::Result<KlisterStatement> {
    skip_space(remaining);
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^import ([a-z0-9.]+) ([a-z0-9.]+) ([a-z][a-z0-9]*)\(((?:[a-z, ]+)?)\)").unwrap());
    let caps = RE.captures(remaining).unwrap();
    let args = if caps[4].trim() == "" {
        Vec::new()
    } else {
        caps[4].split(",").map(|x| x.trim().to_string()).collect::<Vec<_>>()
    };

    *remaining = &remaining[caps[0].len()..];

    return Ok(KlisterStatement::Import(caps[1].to_string(), caps[3].to_string(), caps[2].to_string(), args));
}

fn parse_precedence_6(s: &mut& str) -> anyhow::Result<KlisterExpression> {
    skip_space(s);

    let mut in_chars = s.char_indices();

    let first_char = in_chars.next().context("Empty string")?.1;

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
        let result = caps[0].parse::<BigInt>().ok().context("Invalid numeric literal")?;
        *s = &s[caps[0].len()..];
        return Ok(KlisterExpression::Literal(KlisterValue::BInt(result)));
    }
    static SHELL_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^`").unwrap());
    if SHELL_RE.is_match(s) {
        return parse_shell_main(s);
    }

    if let Ok(id) = parse_id(s) {
        return Ok(KlisterExpression::Variable(id));
    }
    return Err(anyhow!("Parse precedence 6 failed at {}", s));
}

fn parse_id(remaining: &mut& str) -> anyhow::Result<String> {
    skip_space(remaining);
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^([A-Za-z_][A-Za-z0-9_]*)").unwrap());
    let caps = RE.captures(remaining).context("Failed to parse id")?;
    let result = &caps[1];
    *remaining = &remaining[result.len()..];
    return Ok(result.to_string());
}

fn consume(prefix: &str, remaining: &mut&str) -> anyhow::Result<()> {
    if remaining.is_empty() || remaining.len() < prefix.len() { return Err(anyhow!(""))};
    for (a, b) in prefix.chars().zip(remaining.chars()) {
        if a != b {
            return Err(anyhow!(""));
        }
    }
    *remaining = &remaining[prefix.len()..];
    return Ok(());
}

fn parse_call_args(remaining: &mut& str) -> anyhow::Result<Vec::<KlisterExpression>> {
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

fn parse_precedence_5(s: &mut&str) -> anyhow::Result<KlisterExpression> {
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

fn parse_precedence_4(s: &mut&str) -> anyhow::Result<KlisterExpression> {
    let parse_next = parse_precedence_5;

    let mut ret = parse_next(s)?;
    while !s.is_empty() {
        skip_space(s);
        if consume("*", s).is_ok() {
            ret = KlisterExpression::Mul(Box::new(ret), Box::new(parse_next(s)?));
        } else if consume("/", s).is_ok() {
            ret = KlisterExpression::Div(Box::new(ret), Box::new(parse_next(s)?));
        } else {
            break;
        }
    }
    return Ok(ret);
}

fn parse_precedence_3(s: &mut&str) -> anyhow::Result<KlisterExpression> {
    let parse_next = parse_precedence_4;
    let mut ret = parse_next(s)?;
    while !s.is_empty() {
        skip_space(s);
        if consume("+", s).is_ok() {
            ret = KlisterExpression::Add(Box::new(ret), Box::new(parse_next(s)?));
        } else if consume("-", s).is_ok() {
            ret = KlisterExpression::Sub(Box::new(ret), Box::new(parse_next(s)?));
        } else {
            break;
        }
    }
    return Ok(ret);
}

fn parse_precedence_2(s: &mut&str) -> anyhow::Result<KlisterExpression> {
    let parse_next = parse_precedence_3;
    let mut ret = parse_next(s)?;
    if !s.is_empty() {
        skip_space(s);
        if consume("<", s).is_ok() {
            ret = KlisterExpression::Lt(Box::new(ret), Box::new(parse_next(s)?));
        } else if consume(">", s).is_ok() {
            ret = KlisterExpression::Gt(Box::new(ret), Box::new(parse_next(s)?));
        } else if consume("<=", s).is_ok() {
            ret = KlisterExpression::Lte(Box::new(ret), Box::new(parse_next(s)?));
        } else if consume(">=", s).is_ok() {
            ret = KlisterExpression::Gte(Box::new(ret), Box::new(parse_next(s)?));
        } else if consume("==", s).is_ok() {
            ret = KlisterExpression::Eq(Box::new(ret), Box::new(parse_next(s)?));
        } else if consume("!=", s).is_ok() {
            ret = KlisterExpression::Ne(Box::new(ret), Box::new(parse_next(s)?));
        }
    }
    return Ok(ret);
}

fn parse_precedence_1(s: &mut&str) -> anyhow::Result<KlisterExpression> {
    skip_space(s);
    if consume("?", s).is_err() {
        return parse_precedence_2(s);
    }
    consume("(", s)?;
    let res = KlisterExpression::CatchExpr(Box::new(parse_precedence_2(s)?));
    consume(")", s)?;
    return Ok(res);
}

fn parse_precedence_0(s: &mut&str) -> anyhow::Result<KlisterStatement> {
    let mut s2_x = *s;
    let s2:&mut&str = &mut s2_x;
    let idid = parse_id(s2);
    skip_space(s2);
    if idid.is_ok() && consume("=", s2).is_ok() {
        let ret = KlisterStatement::Assign(idid?, parse_precedence_1(s2)?);
        *s = *s2;
        return Ok(ret);
    }
    return Ok(KlisterStatement::Expression(parse_precedence_1(s)?));
}

fn consume_remaining_line(s: &mut&str) -> anyhow::Result<()> {
    loop {
        let Some(c) = s.chars().next() else {return Err(anyhow!("Incomplete line"))};
        *s = &s[c.len_utf8()..];
        if c == '\n' {
            return Ok(());
        }
    }
}

fn skip_space_and_newlines(s: &mut&str) -> anyhow::Result<()>  {
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

fn consume_statement_enders(s: &mut&str) -> anyhow::Result<()> {
    let s_back = *s;
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
    return if newline {Ok(())} else {Err(anyhow!("No statement ender found {}", s_back))};
}

fn parse_block(remaining: &mut&str) -> anyhow::Result<KlisterStatement> {
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

fn parse_while(s: &mut&str) -> anyhow::Result<KlisterStatement> {
    skip_space_and_newlines(s)?;
    consume("while", s)?;
    skip_space_and_newlines(s)?;
    consume("(", s)?;
    let expr = parse_precedence_2(s)?;
    skip_space_and_newlines(s)?;
    consume(")", s)?;
    let body = parse_block(s)?;
    return Ok(KlisterStatement::While(expr, Box::new(body)));
}

fn parse_if(s: &mut&str) -> anyhow::Result<KlisterStatement> {
    skip_space_and_newlines(s)?;
    consume("if", s)?;
    skip_space_and_newlines(s)?;
    consume("(", s)?;
    let expr = parse_precedence_2(s)?;
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

fn parse_shell_token(s: &mut&str) -> anyhow::Result<Option<String>> {
    let mut ret = String::new();
    let mut charss = s.chars();
    loop {
        let Some(c) = charss.next() else {break;};
        if c == ' ' || c == '`' || c == '\n' || c == '|' || c == '`' {
            break;
        } else if c == '\\' {
            let c2 = charss.next().context("Incomplete escape")?;
            if c2 == '\\' {
                ret.push('\\');
            } else if c2 == 'n' {
                ret.push('\n');
            } else {
                return Err(anyhow!("Unknown escape sequence"));
            }
            
            *s = &s[c.len_utf8() + c2.len_utf8()..];
        } else {
            ret.push(c);
            *s = &s[c.len_utf8()..];
        }
    }
    if !ret.is_empty() {return Ok(Some(ret));} else {return Ok(None)}
}

fn skip_shell_separators(s: &mut&str) {
    while let Some(c) = s.chars().next() {
        if c != ' ' {
            break;
        }
        *s = &s[c.len_utf8()..];
    }
}

fn parse_shell_command(s: &mut&str) -> anyhow::Result<ShellCommand> {
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

fn parse_shell_main(s: &mut&str) -> anyhow::Result<KlisterExpression> {
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
    if cmds.len() == 0 {return Err(anyhow!("No cmds"))};
    return Ok(KlisterExpression::ShellPipeline(ShellPipelineS::new(cmds)))
}

fn parse_statement(remaining: &mut&str) -> anyhow::Result<KlisterStatement> {
    skip_space_and_newlines(remaining)?;
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
        return parse_precedence_0(remaining);
    }
}

fn parse_statements(remaining: &mut&str) -> anyhow::Result<Vec<KlisterStatement>> {
    let mut ret = Vec::new();
    skip_space_and_newlines(remaining)?;
    while !remaining.is_empty() {
        ret.push(parse_statement(remaining)?);
        consume_statement_enders(remaining)?;
    }
    return Ok(ret)
}

pub fn parse_ast(s: &str) -> anyhow::Result<KlisterStatement> {
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
        return Err(anyhow!("Trailing garbage {}", remaining))
    }
    return Ok(KlisterStatement::Block(statements));
}