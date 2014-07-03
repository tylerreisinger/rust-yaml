use std::str;
use std::mem;
use std::num;
use std::char;
use std::uint;
use std::collections::ringbuf;
use std::collections::{Deque};

#[deriving(Show, Clone, PartialEq)] pub enum LexerError {
    EndOfInput,
    TokenizeError(String, SourceLocation), 
    GenericError,
}

pub type LexerResult<T> = Result<T, LexerError>;

#[deriving(Show, Clone, PartialEq)]
pub struct SourceLocation {
    line: uint,
    col: uint
}

#[deriving(Show, Clone, PartialEq)]
pub struct SourceRegion {
    start: SourceLocation,
    end: SourceLocation
}

#[deriving(Clone, PartialEq)]
enum ChompingKind {
    ChompingStrip,
    ChompingClip,
    ChompingKeep,
}

#[deriving(Clone, PartialEq)]
enum BlockScalarKind {
    LiteralBlockScalar,
    FoldedBlockScalar,
}

#[deriving(Show, Clone, PartialEq)]
pub enum TokenKind { 
    TkError,

    TkScalarPlain,
    TkScalarSingleQtd,
    TkScalarDoubleQtd,
    TkScalarBlock,

    TkIndicatorMappingValue,
    TkIndicatorMappingKey,
    TkIndicatorSequenceEntry,

    TkIndicatorFlowSequenceEntry,
    TkIndicatorFlowSequenceStart,
    TkIndicatorFlowSequenceEnd,
    TkIndicatorFlowMappingStart,
    TkIndicatorFlowMappingEnd,

    TkIndicatorAnchor,
    TkIndicatorAlias,
    TkIndicatorDirective,
    TkIndicatorReserved,

    TkIndicatorTagPrimary,
    TkIndicatorTagSecondary,
    TkNamedTagIdentifier,
    TkTagSuffix,
    TkTagUri,

    TkDocumentStart,
    TkDocumentEnd,

    TkComment,

    TkIndentIn,
    TkIndentOut,
}

#[deriving(Show, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    data: Option<String>,
    extent: SourceRegion,
}

pub struct Lexer {
    buffer: String,

    start_loc: SourceLocation,
    cur_loc: SourceLocation,

    //Necessary to handle indentation properly
    token_backlog: ringbuf::RingBuf<Token>,

    flow_level: int,
    indent: Vec<uint>,

    cur_char: str::CharRange,
    buf_pos: uint,
    data: String,
    eof: bool
}

//Character classes
#[inline]
fn is_nb_whitespace(ch: char) -> bool {
    match ch {
        ' ' | '\t' => true,
        _ => false
    }
}

#[inline]
fn is_newline(ch: char) -> bool {
    match ch {
        '\n' | '\r' => true,
        _ => false
    }
}

#[inline]
fn is_whitespace(ch: char) -> bool {
    match ch {
        _ if is_nb_whitespace(ch) || is_newline(ch) => true,
        _ => false
    }
}

#[inline]
fn is_whitespace_byte(ch: u8) -> bool {
    is_whitespace(ch as char)
}

#[inline]
fn is_indicator(ch: char) -> bool {
    match ch {
        '-' | '?' | ':' | ',' | '[' | ']' | '{' | '}'
        | '#' | '&' | '*' | '!' | '|' | '>' | '\'' | '"'
        | '%' | '@' | '`' => true,
        _ => false,
    }
}

#[inline]
fn is_flow_indicator(ch: char) -> bool {
    match ch {
        '[' | ']' | '{' | '}' | ',' => true,
        _ => false,
    }
}

#[inline]
fn is_flow_start(ch: char) -> bool {
    match ch {
        '[' | '{' => true,
        _ => false,
    }
}

#[inline]
fn is_flow_end(ch: char) -> bool {
    match ch {
        ']' | '}' => true,
        _ => false,
    }
}

#[inline]
fn is_flow_indicator_byte(ch: u8) -> bool {
    is_flow_indicator(ch as char)
}


impl Lexer
{
    pub fn new(input: String) -> Lexer {
        let charRange = input.as_slice().char_range_at(0);
        let mut lex = Lexer {
            buffer: input,
            start_loc: SourceLocation{line: 0, col: 0},
            cur_loc: SourceLocation{line: 0, col: 0},
            flow_level: 0,
            token_backlog: ringbuf::RingBuf::new(),
            indent: vec!(0),
            buf_pos: 0,
            cur_char: charRange,
            data: String::new(),
            eof: false,
            };
        lex.buffer.push_char('\n');
        lex
    }

    #[inline]
    fn current_char(&self) -> char {
        self.cur_char.ch
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.eof
    }

    #[inline]
    fn is_in_flow_context(&self) -> bool {
        self.flow_level > 0
    }

    #[inline]
    fn advance_input(&mut self) {
        self.cur_loc.col += self.cur_char.next - self.buf_pos;
        self.buf_pos = self.cur_char.next;
        self.cur_char = self.buffer.as_slice().char_range_at(self.buf_pos);
    }

    #[inline]
    fn advance_bytes(&mut self, count: uint) {
        self.cur_loc.col += count;
        self.buf_pos += count;
        self.cur_char = self.buffer.as_slice().char_range_at(self.buf_pos);
    }

    #[inline]
    fn peek_next_byte(&self) -> u8 {
        debug_assert!(self.buf_pos+1 < self.buffer.len());
        self.buffer.as_slice()[self.buf_pos+1]
    }

    #[inline]
    fn peek_next_char(&self) -> char {
        debug_assert!(self.cur_char.next < self.buffer.len());
        self.buffer.as_slice().char_at(self.cur_char.next)
    }

    #[inline]
    fn advance_count(&mut self, count: uint) {
        for _ in range(0, count) {
            self.advance_input()
        }
    }

    #[inline]
    fn next_line(&mut self) {
        if self.buf_pos + 1 >= self.buffer.len() {
            self.eof = true;
            return;
        }
        match self.current_char() {
            '\n' => {
                self.advance_input();
                self.cur_loc.line += 1;
                self.cur_loc.col = 0;
            },
            '\r' => {
                self.advance_input();
                if !self.is_eof() && self.current_char() == '\n' {
                    self.advance_input();
                }
                self.cur_loc.line += 1;
                self.cur_loc.col = 0;
            },
            _ => debug_assert!(false),
        }
        if self.buf_pos <= self.buffer.len() {
            self.cur_char = self.buffer.as_slice().char_range_at(self.buf_pos);
        }
        else {
            self.eof = true;
        }
    }

    #[inline]
    fn consume_input(&mut self) {
        let ch = self.current_char();
        self.data.push_char(ch);
        self.advance_input();
    }

    #[inline]
    fn mark_start_location(&mut self) {
        self.start_loc = self.cur_loc;
    }

    #[inline]
    fn skip_inline_whitespace(&mut self) {
        while is_nb_whitespace(self.current_char()) {
            self.advance_input();
        }
    }

    fn skip_whitespace(&mut self) {
        let mut cur_pos = self.buf_pos;
        let mut line_start = cur_pos;
        let mut backtrack = false;
        loop {
            let cur_byte = self.buffer.as_slice()[cur_pos] as char;
            if is_nb_whitespace(cur_byte) {
                cur_pos += 1;
            }
            else if is_newline(cur_byte) {
                self.advance_bytes(cur_pos - line_start);
                self.next_line();
                cur_pos = self.buf_pos;
                line_start = cur_pos;
                backtrack = true;
                if self.is_eof() {
                    break;
                }
            }
            else {
                if !backtrack {
                    self.advance_bytes(cur_pos - line_start);
                }
                break;
            }
        }
    }

    #[inline]
    fn is_line_start(&self) -> bool{
        return self.cur_loc.col == 0
    }

    #[inline]
    fn pop_token(&mut self, kind: TokenKind) -> Token {
        let mut tk = Token{kind: kind, data: None, 
            extent: SourceRegion{start: self.start_loc, end: self.cur_loc}};
        tk.data = Some(mem::replace(&mut self.data, String::new()));
        tk
    }

    #[inline]
    fn make_empty_token(&mut self, kind: TokenKind) -> Token {
        Token{kind: kind, data: None, 
            extent: SourceRegion{start: self.start_loc, end: self.cur_loc}}
    }

    #[inline]
    fn make_tokenize_error(&self, desc: String) -> LexerError {
        TokenizeError(desc, self.cur_loc)
    }

    #[inline]
    fn current_indent(&self) -> uint {
        *self.indent.last().unwrap()
    }

    #[inline]
    fn increase_indent(&mut self, level: uint) {
        self.indent.push(level);
    }

    fn trim_data_end(&mut self) {
        while self.data.len() > 0  && is_whitespace_byte(self.data.as_slice()[self.data.len()-1]) {
            //All whitespace characters are ascii, so we can
            //safely operate at the byte level.
            unsafe {self.data.pop_byte();}
        }
    }

    fn count_indent_chars(&mut self) -> uint {
        let mut current_pos = self.buf_pos;
        let mut indent = 0;
        while self.buffer.as_slice()[current_pos] == (' ' as u8) {
            indent += 1;
            current_pos += 1; 
        }
        indent
    }

    fn handle_line_indent(&mut self) -> Option<Token> {
        let indent = self.count_indent_chars();
        let ret = 
        if indent > self.current_indent() {
            self.increase_indent(indent);
            Some(self.make_empty_token(TkIndentIn))
        }
        else if indent < self.current_indent() {
            while self.current_indent() > indent {
                self.indent.pop();
                let new_token = self.make_empty_token(TkIndentOut);
                self.token_backlog.push_back(new_token);
            }
            let first_token = self.token_backlog.pop_front().unwrap();
            Some(first_token)
        }
        else {
            None
        };
        let advance_len = self.current_indent();
        self.advance_bytes(advance_len);
        ret
    }

    #[inline]
    fn flow_fold(&mut self) {
        self.trim_data_end();
        self.next_line();
    }

    fn process_escape_char(&mut self, size: uint) -> LexerResult<char> {
        debug_assert!(size == 2 || size == 4 || size == 8);
        self.advance_input();
        let prefixCh = match size {
            2 => 'x',
            4 => 'u',
            8 => 'U',
            _ => 'X'
        };
        if self.buf_pos + size >= self.buffer.len() {
            Err(self.make_tokenize_error(
                format!("Expected {} digit hex \
                    sequence after '\\{}' escape pattern", size, prefixCh)))
        }
        else {
            let ret = {
                let hexStr = self.buffer.as_slice().slice(self.buf_pos, self.buf_pos+size);
                let val = num::from_str_radix::<int>(hexStr, 16);
                let ret = match val {
                    Some(x) => {
                        let ch = char::from_u32(x as u32);
                        match ch {
                            Some(c) => {
                                Ok(c)
                            }
                            None => Err(self.make_tokenize_error(
                                format!("Invalid character escape \
                                value '{}'", hexStr)))
                        }
                    }
                    None => Err(self.make_tokenize_error(
                        format!("Unable to interpret value '{}' \
                        after '\\{}' as hex literal", hexStr, prefixCh)))
                };
                ret
            };
            if ret.is_ok() {
                self.advance_count(size);
            }
            ret
        }
    }

    #[inline]
    fn advance_and_return<T>(&mut self, ret: T) -> T
    {
        self.advance_input();
        ret
    }

    fn process_escape(&mut self) -> LexerResult<char> {   
        debug_assert!(self.current_char() == '\\');
        self.advance_input();
        let ch = self.current_char();
        match ch
        {   
            '\\' => Ok(self.advance_and_return('\\')),
            '0' => Ok(self.advance_and_return('\0')),
            'a' => Ok(self.advance_and_return('\x07')),
            'b' => Ok(self.advance_and_return('\x08')),
            't' => Ok(self.advance_and_return('\t')),
            'n' => Ok(self.advance_and_return('\n')),
            'v' => Ok(self.advance_and_return('\x0B')),
            'f' => Ok(self.advance_and_return('\x0C')),
            'r' => Ok(self.advance_and_return('\r')),
            'e' => Ok(self.advance_and_return('\x1B')),
            '"' => Ok(self.advance_and_return('"')),
            '/' => Ok(self.advance_and_return('/')),
            'N' => Ok(self.advance_and_return('\x85')),
            '_' => Ok(self.advance_and_return('\xA0')),
            'L' => Ok(self.advance_and_return('\u2028')),
            'P' => Ok(self.advance_and_return('\u2029')),
            'x' => self.process_escape_char(2),
            'u' => self.process_escape_char(4),
            'U' => self.process_escape_char(8),
            _ => Err(self.make_tokenize_error(
                    format!("Invalid escape sequence '\\{}'", ch)))
        }   
    }

    fn read_single_quoted_scalar(&mut self, multiline: bool) -> LexerResult<Token> {
        debug_assert!(self.current_char() == '\'');
        self.advance_input();

        let mut in_flow = false;
        loop {
            let ch = self.current_char();
            match ch {
                _ if is_newline(ch) => {
                    if !multiline {
                        return Err(self.make_tokenize_error(
                            "Invalid newline in quoted scalar".to_string()));
                    }
                    else {
                        self.flow_fold();
                        if in_flow {
                            self.data.push_char('\n')
                        }
                        else {
                            self.data.push_char(' ')
                        }
                        in_flow = true;
                    }
                },
                '\'' => {
                    self.advance_input();
                    if self.current_char() == '\'' {
                        self.consume_input();
                    }
                    else {
                        break;
                    }
                },
                _ if is_nb_whitespace(ch) && in_flow => {
                    self.advance_input();
                }
                _ => {
                    if is_nb_whitespace(ch) {
                        in_flow = false;
                    }
                    self.consume_input();
                },
            }
        }
        Ok(self.pop_token(TkScalarSingleQtd))
    }

    fn read_double_quoted_scalar(&mut self, multiline: bool) -> LexerResult<Token> {
        debug_assert!(self.current_char() == '"'); 
        self.advance_input();
        
        let mut in_flow = false;
        loop { 
            let ch = self.current_char();
            match ch {
                _ if is_newline(ch) => {
                    if !multiline {
                        return Err(self.make_tokenize_error(
                            "Invalid newline in quoted scalar".to_string()));
                    }
                    else {
                        self.flow_fold();
                        if in_flow {
                            self.data.push_char('\n')
                        }
                        else {
                            self.data.push_char(' ')
                        }
                        in_flow = true;
                    }
                },
                '"' => {
                    self.advance_input();
                    break;
                },
                '\\' => {
                    let escaped_char = try!(self.process_escape());
                    self.data.push_char(escaped_char); 
                }
                _ if is_nb_whitespace(ch) && in_flow => {
                    self.advance_input();
                }
                _ => {
                    if is_nb_whitespace(ch) {
                        in_flow = false;
                    }
                    self.consume_input();
                },
            }
        }
        Ok(self.pop_token(TkScalarDoubleQtd))
    }

    fn is_valid_plain_scalar(&self) -> bool {
        let ch = self.current_char();
        match ch {
            _ if !is_whitespace(ch) && !is_indicator(ch) => true,
            ':' | '-' | '?' => {
                let next_ch = self.peek_next_byte();
                if self.is_in_flow_context() {
                    if !is_whitespace_byte(next_ch) {
                        true
                    }
                    else {
                        false
                    }
                }
                else {
                    if !is_whitespace_byte(next_ch) 
                        && !is_flow_indicator_byte(next_ch) {
                        true
                    }
                    else {
                        false
                    }
                }
            },
            _ => false,
        }
    }

    fn read_plain_scalar(&mut self) -> LexerResult<Token> {
        debug_assert!(self.is_valid_plain_scalar());        
        loop {
            let ch = self.current_char();
            match ch {
                _ if is_newline(ch) => break,
                ':' => {
                    if is_whitespace_byte(self.peek_next_byte()) {
                        break;
                    }
                    else {
                        self.consume_input();
                    }
                },
                '#' => {
                    if self.buf_pos > 0 && 
                            self.buffer.as_slice()[self.buf_pos-1] == ' ' as u8 {
                        break;
                    }
                    else {
                        self.consume_input();
                    }
                },
                _ if self.is_in_flow_context() && 
                     is_flow_indicator(ch) => break,
                _ => self.consume_input(),

            }
        }
        self.trim_data_end();
        Ok(self.pop_token(TkScalarPlain))
    }

    fn read_indicator(&mut self) -> LexerResult<Option<Token>> {
        let ch = self.current_char();
        let kind = match ch {
            ':' => TkIndicatorMappingValue,
            '?' => TkIndicatorMappingKey,
            '-' => TkIndicatorSequenceEntry,
            '[' => TkIndicatorFlowSequenceStart,
            ']' => TkIndicatorFlowSequenceEnd,
            '{' => TkIndicatorFlowMappingStart,
            '}' => TkIndicatorFlowMappingEnd,
            ',' => TkIndicatorFlowSequenceEntry,
            '%' => TkIndicatorDirective,
            '*' => TkIndicatorAlias,
            '&' => TkIndicatorAnchor,
            '@' | '`' => TkIndicatorReserved,
            _ => TkError,
        };
        if kind != TkError {
            if is_flow_start(ch) {
                self.flow_level += 1;
            }
            else if is_flow_end(ch) {
                self.flow_level -= 1;
                if self.flow_level < 0 {
                    return Err(self.make_tokenize_error(
                        format!("Unmatched closing '{}' delimiter", ch)));
                }
                
            }
            self.advance_input();
            Ok(Some(self.make_empty_token(kind)))
        }
        else {
            Ok(None)
        }
    }

    fn read_document_marker(&mut self) -> Option<Token> {
        let ch = self.current_char() as u8;
        debug_assert!(ch == '-' as u8 || ch == '.' as u8);
        let mut current_pos = self.buf_pos;
        let mut success = true;
        for _ in range(0i, 3) {
            if self.buffer.as_slice()[current_pos] != ch {
                success = false;
            }
            current_pos += 1;
        }
        if success {
            let tk = match ch as char {
                '-' => TkDocumentStart,
                '.' => TkDocumentEnd,
                _ => TkError,
            };
            if tk != TkError {
                self.advance_bytes(3);
                Some(self.make_empty_token(tk))
            }
            else {
                None
            }
        }
        else {
            None
        }
    }

    fn read_comment(&mut self) -> LexerResult<Token> {
        debug_assert!(self.current_char() == '#');
        self.advance_input();
        loop {
            let ch = self.current_char();
            match ch {
                _ if is_newline(ch) => {
                    self.next_line();
                    break;
                },
                _ => self.consume_input(),
            }
        }
        Ok(self.pop_token(TkComment))
    }

    #[inline]
    fn is_valid_uri_hex_escape(&mut self, start_pos: uint) -> bool {
        for i in range(0u, 2) {
            match self.buffer.as_slice()[start_pos+i] as char {
                '0'..'9' | 'A'..'F' | 'a'..'f' => (),
                _ => return false,
            }
        }
        true
    }

    fn try_read_uri(&mut self) -> bool {
        loop {
            let ch = self.current_char();
            match ch {
                '%' => {
                    let hex_start_pos = self.buf_pos+1;
                    if self.is_valid_uri_hex_escape(hex_start_pos) {
                        self.advance_bytes(3);
                    }
                    else {
                        return false;
                    }
                },
                '0'..'9' | 'a'..'z' | 'A'..'Z' 
                    |'#' | ';' | '/' | '?' | ':' | '@' | '&' | '='
                    | '+' | '$' | ',' | '_' | '.' | '!' | '~' 
                    | '*' | '\'' | '(' | ')' | '[' | ']' => self.consume_input(),
                '>' => {
                    self.advance_input();
                    break;
                },
                '<' => self.advance_input(),
                _ => return false,
            }
        }
        true
    }

    fn try_read_tag_suffix(&mut self) -> bool {
        loop {
            let ch = self.current_char();
            match ch {
                '%' => {
                    let hex_start_pos = self.buf_pos+1;
                    if self.is_valid_uri_hex_escape(hex_start_pos) {
                        self.advance_bytes(3);
                    }
                    else {
                        return false;
                    }
                },
                '0'..'9' | 'a'..'z' | 'A'..'Z' 
                    |'#' | ';' | '/' | '?' | ':' | '@' | '&' | '='
                    | '+' | '$' | '_' | '.' | '~' 
                    | '*' | '\'' | '(' | ')' => self.consume_input(),
                _ if is_whitespace(ch) => break,
                _ => return false,
            }
        }
        true
    }

    fn read_tag_suffix_token(&mut self) -> LexerResult<Token> {
        let ch = self.current_char();
        if ch == '<' {
            if !self.try_read_uri() {
                Err(self.make_tokenize_error(
                    "Expected a valid URI identifying \
                    a tag between <>s".to_string()))
            }
            else {
                Ok(self.pop_token(TkTagUri))
            }
        }
        else {
            if !self.try_read_tag_suffix() {
                Err(self.make_tokenize_error(
                    "Expected a valid tag suffix \
                    after '!'".to_string()))
            }
            else {
                Ok(self.pop_token(TkTagSuffix))
            }
        }
    }

    fn read_tag(&mut self) -> LexerResult<Token> {
        debug_assert!(self.current_char() == '!');
        self.advance_input();
        let ch = self.current_char();
        let mut kind = TkIndicatorTagPrimary;
        let mut tk;
        if ch == '!' {
            self.advance_input();
            kind = TkIndicatorTagSecondary;
            tk = Some(self.make_empty_token(kind));
        }
        else {
            let mut cur_pos = self.buf_pos;
            let mut is_named = false;
            loop {
                let byte = self.buffer.as_slice()[cur_pos] as char;
                match byte {
                    '0'..'9' | 'a'..'z' | 'A'..'Z' | '-' => cur_pos += 1,
                    '!' => {
                        is_named = true;
                        break;
                    }
                    _ => break,

                }
            }
            if is_named {
                while self.buf_pos < cur_pos {
                    self.consume_input();
                }
                self.advance_input();
                kind = TkNamedTagIdentifier;
                tk = Some(self.pop_token(kind));
            }
            else {
                tk = Some(self.make_empty_token(kind));
            }
        }
        
        let suffix_tk = try!(self.read_tag_suffix_token());
        self.token_backlog.push_back(suffix_tk);
        debug_assert!(tk.is_some());
        Ok(tk.unwrap())
    }

    fn read_block_header(&mut self) -> LexerResult<(ChompingKind, uint)> {
        let mut indent = 0;
        let mut chomping_kind = ChompingClip;
        for _ in range(0u, 2) {
            match self.current_char() {
                '+' => {
                    if chomping_kind != ChompingClip {
                        return Err(self.make_tokenize_error(
                            "Only one chomping indicator may be specified \
                            for a block scalar".to_string()));
                    }
                    chomping_kind = ChompingKeep;
                    self.advance_input();
                },
                '-' => {
                    if chomping_kind != ChompingClip {
                        return Err(self.make_tokenize_error(
                            "Only one chomping indicator may be specified \
                            for a block scalar".to_string()));
                    }
                    chomping_kind = ChompingStrip;
                    self.advance_input();
                },
                '0'..'9' => {
                    if indent != 0 {
                        return Err(self.make_tokenize_error(
                            "Only one indent specifier may be provided for \
                            a block scalar".to_string()));
                    }
                    let mut cur_pos = self.buf_pos;
                    match self.buffer.as_slice()[cur_pos] as char {
                        '0'..'9' => cur_pos += 1,
                        _ => (),
                    }
                    {
                        let indent_str = self.buffer
                            .as_slice().slice(self.buf_pos, cur_pos);
                        let parse_result = uint::parse_bytes(indent_str.as_bytes(), 10);
                        match parse_result {
                            Some(val) => {
                                indent = val;
                                if indent == 0 {
                                    return Err(self.make_tokenize_error(
                                        "Indent specification must be larger than 0".
                                        to_string()));
                                }
                            },
                            None => return Err(self.make_tokenize_error(
                                format!("Invalid string '{}' specified for indent level",
                                        indent_str))),
                        }
                    };
                    let advance_len = cur_pos - self.buf_pos;
                    self.advance_bytes(advance_len);
                }
                _ => break,
            }
        }
        Ok((chomping_kind, indent))
    }

    fn next_line_in_scalar_body(&mut self, kind: BlockScalarKind,
            blank_line: bool, more_indented: bool) {
        self.next_line();
        if kind == LiteralBlockScalar || blank_line 
                || more_indented {
            self.data.push_char('\n');
        }
        else {
            self.data.push_char(' ');
        }
    }

    fn chomp_block_scalar(&mut self, chomp: ChompingKind) {
        let mut cur_pos = (self.data.len()-1) as int;
        if chomp == ChompingKeep {
            return;
        }
        let mut was_chomped = false;
        while cur_pos >= 0 {
            let ch = self.data.as_slice()[cur_pos as uint] as char;
            match ch {
                _ if is_whitespace(ch) => {
                    self.data.pop_char();
                    cur_pos -= 1;
                    was_chomped = true;
                },
                _ => break,
            }
        }
        if was_chomped {
            if chomp == ChompingClip {
                self.data.push_char('\n');
            }
        }
    }

    fn read_block_scalar_body(&mut self, indent: uint,
        kind: BlockScalarKind, chomp: ChompingKind) -> LexerResult<()> {
        let mut start_of_line = true;
        let mut more_indented = false;
        
        let indent_block = 
            if indent != 0 {
                indent
            }
            else {
                self.count_indent_chars()
            };
        loop {
            let ch = self.current_char();
            if start_of_line {
                let indent_line = self.count_indent_chars();
                if is_newline(self.buffer.as_slice()[self.buf_pos+indent_line] as char) {
                    self.next_line_in_scalar_body(kind, true, more_indented);
                    self.advance_bytes(indent_line);
                    continue;
                }
                if indent_line < indent_block {
                    break;
                }
                else if indent_line > indent_block {
                    if !more_indented {
                        self.data.push_char('\n');
                    }
                    more_indented = true;
                }
                else {
                    more_indented = false;
                }
                self.advance_bytes(indent_block);
                start_of_line = false;
            }
            else {
                match ch {
                    _ if is_newline(ch) => {
                        start_of_line = true;
                        self.next_line_in_scalar_body(kind,
                            false, more_indented);
                    },
                    _ if is_nb_whitespace(ch) => {
                        self.consume_input();
                    },
                    _ => self.consume_input(),
                }
            }
        };
        self.chomp_block_scalar(chomp);
        Ok(())
    }
    
    fn read_outside(&mut self) -> LexerResult<Token> {
        Err(GenericError)
    }

    fn read_in_flow_node(&mut self) -> LexerResult<Token> {
        Err(GenericError)
    }

    fn read_in_block_node(&mut self) -> LexerResult<Token> {
        Err(GenericError)
    }

    fn read_block_scalar(&mut self) -> LexerResult<Token> {
        let block_kind = match self.current_char() {
            '|' => LiteralBlockScalar,
            '>' => FoldedBlockScalar,
            _ => fail!("Invalid block indicator"),
        };
        self.advance_input();
        let (chomping_kind, indent) = try!(self.read_block_header());
        self.skip_inline_whitespace();
        let ch = self.current_char();
        match self.current_char() {
            '#' => {
                let tk = try!(self.read_comment());
                self.token_backlog.push_back(tk);
            },
            _ if is_newline(ch) => self.next_line(),
            _ => return Err(self.make_tokenize_error(
                format!("Unexpected character '{}' found in block header", 
                    self.current_char()))),
        }
        if self.is_eof() {
            return Err(self.make_tokenize_error(
                "Unexpected end of input found while reading a block scalar"
                .to_string()));
        }
        try!(self.read_block_scalar_body(indent, block_kind, chomping_kind));
        let block_tk = self.pop_token(TkScalarBlock);
        //We need to put the token in the backlog so that
        //comments in the header show up before the block itself
        self.token_backlog.push_back(block_tk);
        let ret = self.token_backlog.pop_front().unwrap();
        Ok(ret)
    }

    pub fn get_next_token(&mut self) -> LexerResult<Token> {
        if self.token_backlog.len() > 0 {
            return Ok(self.token_backlog.pop_front().unwrap());
        }
        if self.is_eof() {
            return Err(EndOfInput);
        }
        self.mark_start_location();
        if self.is_line_start() {
            let indent_tk = self.handle_line_indent();
            if indent_tk.is_some() {
                return Ok(indent_tk.unwrap());
            }
        }

        self.skip_whitespace();
        if self.is_eof() {
            return Err(EndOfInput);
        }
        if self.is_line_start() {
            let indent_tk = self.handle_line_indent();
            if indent_tk.is_some() {
                return Ok(indent_tk.unwrap());
            }
        }
        let ch = self.current_char();
        match ch {
            '"' => return self.read_double_quoted_scalar(false),
            '\'' => return self.read_single_quoted_scalar(false),
            '.' | '-' => {
                let tk = self.read_document_marker();
                if tk.is_some() {
                    return Ok(tk.unwrap());
                }
            },
            '#' => return self.read_comment(),
            '!' => return self.read_tag(),
            '|' | '>' => return self.read_block_scalar(),
            _ => (),
        }
        if self.is_valid_plain_scalar() {
            return self.read_plain_scalar()
        }
        let indicator_tk = try!(self.read_indicator());
        if indicator_tk.is_some() {
            return Ok(indicator_tk.unwrap());
        }
        Err(GenericError)
    }
}

#[test]
fn test_read_double_quoted_scalar() {
    let mut lex = Lexer::new("\"abc\"".to_string());
    let mut tk = lex.read_double_quoted_scalar(false);

    debug_assert!(tk.is_ok());
    debug_assert!(tk.ok().unwrap().data.unwrap() == "abc".to_string());

    lex = Lexer::new(
"\"abc
  def
  
   gh\"".to_string());
    tk = lex.read_double_quoted_scalar(true);

    debug_assert!(tk.is_ok());
    debug_assert!(tk.ok().unwrap().data.unwrap() == "abc def\ngh".to_string());

    lex = Lexer::new("\"a\\\"bc\\u20ACd\\ne\"".to_string());
    tk = lex.read_double_quoted_scalar(true);

    debug_assert!(tk.is_ok());
    debug_assert!(tk.ok().unwrap().data.unwrap() == "a\"bc\u20ACd\ne".to_string());
}
