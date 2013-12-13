-- -*- haskell -*-
-- Lexer for Scheme
-- (C) Copyright 2013 Mateusz Kowalczyk (Fūzetsu)

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}

module Yi.Lexer.Scheme ( initState, alexScanToken ) where

import Yi.Lexer.Alex
import qualified Yi.Syntax
import Yi.Style

}

$whitechar = [\ \t\n\r\f\v]

@boolean   = "#t" | "#T" | "#f" | "#F"

@charname = "nul" | "alarm" | "backspace" | "tab" | "linefeed" |
            "newline" | "vtab" | "page" | "return" | "esc" |
            "space" | "delete"

@lineend = [\n\r] | \n\r | "\x85" | \r "\x85" | "\x2028"
@intrawhite = \t

@backescape = "\\\\"
@intraend = @intrawhite @lineend

$hexdigit = [0-9A-Fa-f]

@hexscalar = $hexdigit+

@inlinehexesc = \\ 'x' @hexscalar

@stringElem = \a | \b | \t | \n | \v | \f | \r | @intraend | @intrawhite |
              @inlinehexesc | \\ \" | \^ @backescape | ~\"
@string = \" stringElem* \"
@hello = "hello"

scheme :-

<0> {
  @string      { c stringStyle }
  \" @hello \" { c stringStyle }
}

{

type HlState = Int
type Token = StyleName

stateToInit :: HlState -> Int
stateToInit = const 0

initState :: HlState
initState = 0

#include "common.hsinc"
}
