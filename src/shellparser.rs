use once_cell::sync::Lazy;
use regex::Regex;

use crate::ast::Argon;
use crate::ast::GlobPart;
use crate::ast::KlisterExpression;
use crate::ast::ShellCommand;
use crate::ast::ShellPipelineS;
use crate::parse::consume;
use crate::parse::parse_expr;
use crate::parse::ParseResult;
use crate::parse::skip_space;
use crate::parse::SyntaxErrorTrait;
use crate::parse::synerr;
use crate::parse::SyntaxError;

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
        let ret = parse_expr(s)?;
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

pub fn parse_shell_main(s: &mut&str) -> ParseResult<KlisterExpression> {
    // todo: Possibly rethink newline handling in shell environment.
    skip_space(s);
    let is_catching = consume("?", s).is_ok();
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
    return Ok(KlisterExpression::ShellPipeline(ShellPipelineS::new(cmds, is_catching)))
}