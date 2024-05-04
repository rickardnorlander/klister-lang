use once_cell::sync::Lazy;
use regex::Regex;

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
        '<' => '<',
        '>' => '>',
        _ => {
            synerr!(remaining, "Unknown escape sequence");
        }
    })
}

fn parse_array_interpolation(s: &mut&str) -> ParseResult<Option<Vec<GlobPart>>>  {
    consume("{", s)?;
    let expr = parse_expr(s)?;
    consume("}", s)?;
    return Ok(Some(vec![GlobPart::ArrayInterpolation(Box::new(expr))]));
}

fn parse_interpolation(s: &mut&str) -> ParseResult<GlobPart> {
    let expr = if consume("{", s).is_ok() {
        let expr = parse_expr(s)?;
        consume("}", s)?;
        expr
    } else {
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[A-Za-z_]+").unwrap());
        let caps = RE.captures(s).context(s, "Invalid interpolation")?;
        *s = &s[caps[0].len()..];
        KlisterExpression::Variable(caps[0].to_string())
    };
    return Ok(GlobPart::Interpolation(Box::new(expr)));
}

fn parse_shell_quote(s: &mut&str) -> ParseResult<String> {
    consume("\"", s)?;
    let mut ret = String::new();
    loop {
        let c = s.chars().next().context(s, "Unterminated quote")?;
        *s = &s[c.len_utf8()..];
        // Maybe enable interpolation in quotes? Can always do it later.
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
                return Ok(Some(GlobPart::Asterisk));
            }
            return Ok(Some(GlobPart::Str(ret)));
        } else if c == '$' {
            if ret.is_empty() {
                *s = &s[c.len_utf8()..];
                return parse_interpolation(s).map(|x| Some(x));
            }
            return Ok(Some(GlobPart::Str(ret)));
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
    if !ret.is_empty() {return Ok(Some(GlobPart::Str(ret)));} else {return Ok(None)}
}

#[derive(Clone)]
#[derive(Debug)]
pub enum Neon {
    Argon(Vec<GlobPart>),
    RedirectStdoutToStderr,
    RedirectStderrToStdout,
    RedirectStdout(Vec<GlobPart>),
    RedirectStdin(Vec<GlobPart>),
    RedirectStderr(Vec<GlobPart>),
}

fn parse_redirect(s: &mut &str, ind: i32) -> ParseResult<Neon> {
    skip_shell_separators(s);

    if ind == 1 && consume("&2", s).is_ok() {
        return Ok(Neon::RedirectStdoutToStderr);
    }
    if ind == 2 && consume("&1", s).is_ok() {
        return Ok(Neon::RedirectStderrToStdout);
    }

    let mut ret = Vec::<GlobPart>::new();
    while let Some(glob_part) = parse_glob_part(s)? {
        ret.push(glob_part);
    }
    if !ret.is_empty() {
        match ind {
            0 => Ok(Neon::RedirectStdin(ret)),
            1 => Ok(Neon::RedirectStdout(ret)),
            2 => Ok(Neon::RedirectStderr(ret)),
            _ => panic!("Internal interpreter error"),
        }
    } else {
        synerr!(s, "Incomplete redirect");
    }
}

fn parse_neon(s: &mut&str) -> ParseResult<Option<Neon>> {
    if consume("$[]", s).is_ok() {
        return parse_array_interpolation(s).map(|x| x.map(|y| Neon::Argon(y)));
    }
    if consume(">", s).is_ok() {
        return parse_redirect(s, 1).map(|x| Some(x));
    }
    if consume("1>", s).is_ok() {
        return parse_redirect(s, 1).map(|x| Some(x));
    }
    if consume("2>", s).is_ok() {
        return parse_redirect(s, 2).map(|x| Some(x));
    }
    if consume("<", s).is_ok() {
        return parse_redirect(s, 0).map(|x| Some(x));
    }
    if consume("0<", s).is_ok() {
        return parse_redirect(s, 0).map(|x| Some(x));
    }

    let mut ret = Vec::<GlobPart>::new();
    while let Some(glob_part) = parse_glob_part(s)? {
        ret.push(glob_part);
    }
    if !ret.is_empty() {
        Ok(Some(Neon::Argon(ret)))
    } else {
        Ok(None)
    }
}

fn skip_shell_separators(s: &mut&str) {
    while let Some(c) = s.chars().next() {
        if c != ' ' {
            break;
        }
        *s = &s[c.len_utf8()..];
    }
}

fn consume_shell_separator(s: &mut&str) -> ParseResult<()> {
    if let Some(c) = s.chars().next() {
        if c == ' ' {
            *s = &s[c.len_utf8()..];
            return Ok(());
        }
    }
    synerr!(s, "Missing separator");
}

use crate::ast::OutErr;

fn build_invocation(s: &&str, shell_tokens: Vec<Neon>) -> ParseResult<ShellCommand> {
    let mut cmd: Option<Vec<GlobPart>> = None;
    let mut args: Vec<Vec<GlobPart>> = Vec::new();

    let mut infile = None;
    let mut outfile = None;
    let mut errfile = None;

    let mut outtoerr = false;
    let mut errtoout = false;

    for token in shell_tokens {
        match token {
            Neon::Argon(argon) => {
                if cmd.is_none() {
                    cmd = Some(argon)
                } else {
                    args.push(argon)
                }
            }
            Neon::RedirectStdoutToStderr => {
                if outtoerr || errtoout || outfile.is_some() {
                    synerr!(s, "Invalid redirects");
                }
                outtoerr = true
            }
            Neon::RedirectStderrToStdout => {
                if outtoerr || errtoout || errfile.is_some() {
                    synerr!(s, "Invalid redirects");
                }
                errtoout = true
            }
            Neon::RedirectStdin(v) => {
                infile = Some(v);
            }
            Neon::RedirectStdout(v) => {
                if outtoerr || errtoout {
                    synerr!(s, "Invalid redirects");
                }
                outfile = Some(v);
            }
            Neon::RedirectStderr(v) => {
                if outtoerr || errtoout {
                    synerr!(s, "Invalid redirects");
                }
                errfile = Some(v);
            }
        }
    }

    let outerr = if outtoerr {
        if let Some(file) = errfile {
            OutErr::MergedToFile(file)
        } else {
            OutErr::MergedToStderr
        }
    } else if errtoout {
        if let Some(file) = outfile {
            OutErr::MergedToFile(file)
        } else {
            OutErr::MergedToStdout
        }
    } else {
        OutErr::NoMerge(outfile, errfile)
    };

    let Some(cmd) = cmd else {
        synerr!(s, "No cmd in invocation")
    };
    return Ok(ShellCommand{command: cmd, args, stdin: infile, outerr});
}

fn parse_shell_invocation(s: &mut&str) -> ParseResult<ShellCommand> {
    let mut first = true;
    let mut shell_tokens = Vec::new();
    loop {
        if !first {
            if consume_shell_separator(s).is_err() {
                break;
            }
        }
        skip_shell_separators(s);
        let token_opt_res = parse_neon(s);
        let token_opt = token_opt_res?;
        let Some(token) = token_opt else {
            break;
        };
        shell_tokens.push(token);
        first = false;
    }
    return build_invocation(s, shell_tokens);
}

pub fn parse_shell_main(s: &mut&str) -> ParseResult<KlisterExpression> {
    // todo: Possibly rethink newline handling in shell environment.
    skip_space(s);
    let is_catching = consume("?", s).is_ok();
    let is_write = consume("w", s).is_ok();
    consume("`", s)?;
    let mut invocations = Vec::<ShellCommand>::new();
    skip_shell_separators(s);
    let parse_shell_command_res = parse_shell_invocation(s);
    invocations.push(parse_shell_command_res?);
    loop {
        skip_shell_separators(s);
        if consume("`", s).is_ok() {break;}
        consume("|", s)?;
        skip_shell_separators(s);
        invocations.push(parse_shell_invocation(s)?);
    }
    if invocations.len() == 0 {
        synerr!(s, "No cmds");
    }
    return Ok(KlisterExpression::ShellPipeline(ShellPipelineS::new(invocations, is_catching, is_write)))
}