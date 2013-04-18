-- Copyright 2013 Renaud Aubin <root@renaud.io>
-- Time-stamp: <2013-04-19 00:19:53>
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

local P, S, V, R, patterns, tohash = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.patterns, table.tohash

local context               = context
local verbatim              = context.verbatim
local mp                    = visualizers.makepattern

local JavaSnippet           = context.JavaSnippet
local startJavaSnippet      = context.startJavaSnippet
local stopJavaSnippet       = context.stopJavaSnippet

local JavaSnippetNamespace  = verbatim.JavaSnippetNamespace
local JavaSnippetOperator   = verbatim.JavaSnippetOperator

local handler = visualizers.newhandler {
   startinline  = function()  JavaSnippet(false,"{") end,
   stopinline   = function()  context("}") end,
   startdisplay = function()  startJavaSnippet() end,
   stopdisplay  = function()  stopJavaSnippet() end ,
   namespace    = function(s) JavaSnippetNamespace(s) end,
   operator     = function(s) JavaSnippetOperator(s) end,
}

local keyword = {
   "assert", "break", "case", "catch", "continue", "default", "do", "else", "finally", "for", "if",
   "goto", "instanceof", "new", "return", "switch", "this", "throw", "try", "while",
}

local declaration = tohash {
   "abstract", "const", "enum", "extends", "final", "implements", "native", "private", "protected",
   "public", "static", "strictfp", "super", "synchronized", "throws", "transient", "volatile",
}

local type = tohash {
   "boolean", "byte", "char", "double", "float", "int", "long", "short", "void",
}

local constant = tohash {
   "true", "false", "null",
}

local namespace = tohash {
   "import",
}

local preproc = tohash {
   "define", "include", "pragma", "if", "ifdef", "ifndef", "elif", "endif",
   "defined",
}

local space       = patterns.space
local anything    = patterns.anything
local newline     = patterns.newline
local emptyline   = patterns.emptyline
local beginline   = patterns.beginline
local somecontent = patterns.somecontent

local comment     = P("//") * patterns.space^0 * (1 - patterns.newline)^0
local incomment_open = P("/*")
local incomment_close = P("*/")

local name        = (patterns.letter + patterns.underscore)
                  * (patterns.letter + patterns.underscore + patterns.digit)^0
local boundary    = S('{}')

-- WIP
-- keyword
-- keyword_decl
-- keyword_type
-- decorator
-- method return_arguments + method_name + signature_start
-- text
-- singleline_comment
-- multiline_comment
-- package
local pack = P("package") * patterns.whitespace^1
-- true false null
-- class / interface state
-- import state
local import = P("import") * patterns.whitespace^1
-- string
-- char

-- operator
local operator = lpeg.S("~^*!%&[](){}<>|+=:;,./?-")
-- float →
local float = patterns.float * ( S("eE")^1 * R("09")^1 )^0 * S("fd")^-1
-- hex → patterns.hexadecimal
local hex = patterns.hexadecimal
-- integer → patterns.integer
local integer = patterns.integer * S("L")^-1

-- patterns.propername usability?

local method_name = patterns.propername
local package_name  = R("az","__") * R("09","az", "__")^0

local grammar = visualizers.newgrammar(
   "default",
   {
      "visualizer",

      method =
         patterns.beginline *
--         pattern *
         mp(handler, "method_name", method_name) *
         patterns.whitespace^0 * P("("),

      package = mp(handler, "namespace", pack) *
         (mp(handler,"default", package_name) *
          mp(handler,"operator", patterns.period)
         )^0 * mp(handler,"operator", package_name) * mp(handler,"operator", patterns.semicolon),

      import = mp(handler, "namespace", import) *
         (mp(handler,"default", package_name) *
          mp(handler,"operator", patterns.period))^0 *
         mp(handler,"operator", R("AZ") * R("AZ","az","09")^0) * mp(handler,"operator", patterns.semicolon),


      operator = mp(handler, "operator", operator),

      keyword = mp(handler, "operator", lpeg.oneof(keyword)),

      pattern =
         V("package") +
         V("import") +
--         V("method") +
         V("space") +
         V("operator") +
         V("line") +
         V("keyword") +
         V("default"),

      visualizer = V("pattern")^1
   }
)

local parser = P(grammar)

visualizers.register("java", { parser = parser, handler = handler, grammar = grammar } )
