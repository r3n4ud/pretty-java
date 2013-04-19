-- Copyright 2013 Renaud Aubin <root@renaud.io>
-- Time-stamp: <2013-04-20 00:57:28>
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
if not modules then modules = { } end modules ['t-pretty-java'] = {
    version   = 0.1,
    comment   = "Companion to t-pretty-java.mkiv",
    author    = "Renaud Aubin",
    copyright = "2013 Renaud Aubin",
    license   = "GNU General Public License version 3"
}

local P, S, V, R, patterns = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.patterns

local context               = context
local verbatim              = context.verbatim
local mp                    = visualizers.makepattern

local JavaSnippet           = context.JavaSnippet
local startJavaSnippet      = context.startJavaSnippet
local stopJavaSnippet       = context.stopJavaSnippet

-- verbatim processing
local JavaSnippetTradComment   = verbatim.JavaSnippetTradComment
local JavaSnippetEolComment    = verbatim.JavaSnippetEolComment
local JavaSnippetOperator      = verbatim.JavaSnippetOperator

local handler = visualizers.newhandler {
   startinline  = function()  JavaSnippet(false,"{") end,
   stopinline   = function()  context("}") end,
   startdisplay = function()  startJavaSnippet() end,
   stopdisplay  = function()  stopJavaSnippet() end,
   trad_comment = function(s) JavaSnippetTradComment(s) end,
   eol_comment  = function(s) JavaSnippetEolComment(s) end,
   operator     = function(s) JavaSnippetOperator(s) end,
}

local operator = {
   "=",">","<","!","~","?",":",
   "==","<=",">=","!=","&&","||","++","--",
   "+","-","*","/","&","|","^","%","<<",">>",">>>",
   "+=","-=","*=","/=","&=","|=","^=","%=","<<=",">>=",">>>=",
}

-- http://docs.oracle.com/javase/specs/jls/se7/html/jls-18.html

local line        = patterns.line
local whitespace  = patterns.whitespace
local space       = patterns.space

local eol_comment = P("//") * (P(1) - patterns.newline)^1 * patterns.newline

local grammar = visualizers.newgrammar(
   "default",
   {
      "visualizer",

      Operator = mp(handler, "operator", lpeg.oneof(operator)),

      TraditionalComment =
         mp(handler, "trad_comment", P("/*")) *
         (V("line") + V("whitespace") +  mp(handler, "trad_comment", (P(1) - P("*/"))))^0 *
         mp(handler, "trad_comment", P("*/")),

      EolComment = mp(handler, "eol_comment", eol_comment),

      Comment =
        V("TraditionalComment") + V("EolComment"),

      pattern =
         V("Comment") +
         V("Operator") +
         V("space") +
         V("line") +
         V("default"),

      visualizer = V("pattern")^1
   }
)

local parser = P(grammar)

visualizers.register("java", { parser = parser, handler = handler, grammar = grammar } )
