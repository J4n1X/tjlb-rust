use crate::lexer::Position;
use std::fmt;

/// Language keywords
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    Extern,
    Const,
    Type,
    Struct,
    While,
    If,
    Else,
    Elif,
    For,
    Switch,
    Break,
    Continue,
    As,
    Return,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Keyword::Fn => "fn",
            Keyword::Extern => "extern",
            Keyword::Const => "const",
            Keyword::Type => "type",
            Keyword::Struct => "struct",
            Keyword::While => "while",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::Elif => "elif",
            Keyword::For => "for",
            Keyword::Switch => "switch",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::As => "as",
            Keyword::Return => "return",
        };
        write!(f, "{}", s)
    }
}

impl Keyword {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "fn" => Some(Keyword::Fn),
            "extern" => Some(Keyword::Extern),
            "const" => Some(Keyword::Const),
            "type" => Some(Keyword::Type),
            "struct" => Some(Keyword::Struct),
            "while" => Some(Keyword::While),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "elif" => Some(Keyword::Elif),
            "for" => Some(Keyword::For),
            "switch" => Some(Keyword::Switch),
            "break" => Some(Keyword::Break),
            "continue" => Some(Keyword::Continue),
            "as" => Some(Keyword::As),
            "return" => Some(Keyword::Return),
            _ => None,
        }
    }
}

/// Token kinds
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Punctuation
    OpenParen,          // (
    CloseParen,         // )
    OpenBrace,          // {
    CloseBrace,         // }
    OpenBracket,        // [
    CloseBracket,       // ]
    Semicolon,          // ;
    Colon,              // :
    Comma,              // ,
    Dot,                // .
    Arrow,              // ->
    Question,           // ?

    // Arithmetic operators
    Plus,               // +
    Minus,              // -
    Asterisk,           // *
    Slash,              // /
    Percent,            // %

    // Relational operators
    Equal,              // ==
    NotEqual,           // !=
    Less,               // <
    Greater,            // >
    LessEqual,          // <=
    GreaterEqual,       // >=

    // Logical operators
    LogicalAnd,         // &&
    LogicalOr,          // ||
    LogicalNot,         // !

    // Bitwise operators
    Ampersand,          // &
    Pipe,               // |
    Caret,              // ^
    Tilde,              // ~
    LeftShift,          // <<
    RightShift,         // >>

    // Assignment operators
    Assign,             // =
    PlusAssign,         // +=
    MinusAssign,        // -=
    MultAssign,         // *=
    DivAssign,          // /=
    ModAssign,          // %=
    AndAssign,          // &=
    OrAssign,           // |=
    XorAssign,          // ^=
    LeftShiftAssign,    // <<=
    RightShiftAssign,   // >>=

    // Literals and identifiers
    Integer(i64),
    Float(f64),
    StringLiteral(String),
    Identifier(String),
    Keyword(Keyword),
    LangType(LangType),

    // Special tokens
    Newline,            // \n (statement terminator)
    Eof,                // End of file
}

/// Language type representation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeBase {
    SInt,   // Signed integer
    UInt,   // Unsigned integer  
    SFloat, // Floating point
    Void,   // Void type (u0)
}

/// Complete language type with size and modifiers
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LangType {
    pub base: TypeBase,
    pub size_bits: u32,
    pub pointer_depth: u32,
    pub is_const: bool,
}

impl LangType {
    pub fn new(base: TypeBase, size_bits: u32, pointer_depth: u32, is_const: bool) -> Self {
        Self {
            base,
            size_bits,
            pointer_depth,
            is_const,
        }
    }
    
    pub fn from_str(s: &str) -> Option<Self> {
        if s.len() < 2 {
            return None;
        }
        
        let base = match s.chars().next()? {
            'i' => TypeBase::SInt,
            'u' => TypeBase::UInt,
            'f' => TypeBase::SFloat,
            _ => return None,
        };
        
        let size_str = &s[1..];
        let size: u32 = size_str.parse().ok()?;
        
        // Special case for void (u0)
        if matches!(base, TypeBase::UInt) && size == 0 {
            Some(LangType::new(TypeBase::Void, 0, 0, false))
        } else if size % 8 == 0 && size > 0 {
            Some(LangType::new(base, size, 0, false))
        } else {
            None
        }
    }
    
    pub fn with_const(mut self, is_const: bool) -> Self {
        self.is_const = is_const;
        self
    }
    
    pub fn with_pointer_depth(mut self, depth: u32) -> Self {
        self.pointer_depth = depth;
        self
    }
}

impl fmt::Display for LangType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let const_str = if self.is_const { "const " } else { "" };
        let base_str = match self.base {
            TypeBase::SInt => "i",
            TypeBase::UInt | TypeBase::Void => "u",
            TypeBase::SFloat => "f",
        };
        let asterisks = "*".repeat(self.pointer_depth as usize);
        
        if self.pointer_depth > 0 {
            write!(f, "{}{}{}{}", const_str, base_str, self.size_bits, asterisks)
        } else {
            write!(f, "{}{}{}", const_str, base_str, self.size_bits)
        }
    }
}

/// A token with position information
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: Position,
    pub lexeme: String,
}

impl Token {
    pub fn new(kind: TokenKind, pos: Position, lexeme: String) -> Self {
        Self { kind, pos, lexeme }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::OpenParen => write!(f, "("),
            TokenKind::CloseParen => write!(f, ")"),
            TokenKind::OpenBrace => write!(f, "{{"),
            TokenKind::CloseBrace => write!(f, "}}"),
            TokenKind::OpenBracket => write!(f, "["),
            TokenKind::CloseBracket => write!(f, "]"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Question => write!(f, "?"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Equal => write!(f, "=="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::Less => write!(f, "<"),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::LogicalAnd => write!(f, "&&"),
            TokenKind::LogicalOr => write!(f, "||"),
            TokenKind::LogicalNot => write!(f, "!"),
            TokenKind::Ampersand => write!(f, "&"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::Tilde => write!(f, "~"),
            TokenKind::LeftShift => write!(f, "<<"),
            TokenKind::RightShift => write!(f, ">>"),
            TokenKind::Assign => write!(f, "="),
            TokenKind::PlusAssign => write!(f, "+="),
            TokenKind::MinusAssign => write!(f, "-="),
            TokenKind::MultAssign => write!(f, "*="),
            TokenKind::DivAssign => write!(f, "/="),
            TokenKind::ModAssign => write!(f, "%="),
            TokenKind::AndAssign => write!(f, "&="),
            TokenKind::OrAssign => write!(f, "|="),
            TokenKind::XorAssign => write!(f, "^="),
            TokenKind::LeftShiftAssign => write!(f, "<<="),
            TokenKind::RightShiftAssign => write!(f, ">>="),
            TokenKind::Integer(n) => write!(f, "{}", n),
            TokenKind::Float(n) => write!(f, "{}", n),
            TokenKind::StringLiteral(s) => write!(f, "\"{}\"", s),
            TokenKind::Identifier(s) => write!(f, "{}", s),
            TokenKind::Keyword(kw) => write!(f, "{}", kw),
            TokenKind::LangType(ty) => write!(f, "{}", ty),
            TokenKind::Newline => write!(f, "\\n"),
            TokenKind::Eof => write!(f, "EOF"),
        }
    }
}