package org.willena.slox

enum TokenType:
  // Single-character tokens
  case LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star

  // Tokens that could be one-char, or part of a two-char token
  case Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual

  // Literals
  case Identifier, String, Number

  // Keywords
  case And, Class, Else, False, Fun, For, If, Nil, Or, Print, Return, Super, This, True, Var, While

  case Eof
