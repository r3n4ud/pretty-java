-- Copyright 2013 Renaud Aubin <root@renaud.io>
-- Time-stamp: <2013-04-20 17:05:43>
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
local JavaSnippetSeparator     = verbatim.JavaSnippetSeparator
local JavaSnippetPackage       = verbatim.JavaSnippetPackage
local JavaSnippetPackageTerm   = verbatim.JavaSnippetPackageTerm
local JavaSnippetImport        = verbatim.JavaSnippetImport
local JavaSnippetImportId      = verbatim.JavaSnippetImportId
local JavaSnippetImportTerm    = verbatim.JavaSnippetImportTerm
local JavaSnippetModifier      = verbatim.JavaSnippetModifier
local JavaSnippetChar          = verbatim.JavaSnippetChar
local JavaSnippetString        = verbatim.JavaSnippetString
local JavaSnippetBoolean       = verbatim.JavaSnippetBoolean
local JavaSnippetNull          = verbatim.JavaSnippetNull
local JavaSnippetBasicType     = verbatim.JavaSnippetBasicType

local handler = visualizers.newhandler {
   startinline  = function()  JavaSnippet(false,"{") end,
   stopinline   = function()  context("}") end,
   startdisplay = function()  startJavaSnippet() end,
   stopdisplay  = function()  stopJavaSnippet() end,
   trad_comment = function(s) JavaSnippetTradComment(s) end,
   eol_comment  = function(s) JavaSnippetEolComment(s) end,
   operator     = function(s) JavaSnippetOperator(s) end,
   separator    = function(s) JavaSnippetSeparator(s) end,
   package      = function(s) JavaSnippetPackage(s) end,
   package_term = function(s) JavaSnippetPackageTerm(s) end,
   import       = function(s) JavaSnippetImport(s) end,
   import_id    = function(s) JavaSnippetImportId(s) end,
   import_term  = function(s) JavaSnippetImportTerm(s) end,
   modifier     = function(s) JavaSnippetModifier(s) end,
   char         = function(s) JavaSnippetChar(s) end,
   string_      = function(s) JavaSnippetString(s) end,
   boolean      = function(s) JavaSnippetBoolean(s) end,
   null         = function(s) JavaSnippetNull(s) end,
   basic_type   = function(s) JavaSnippetBasicType(s) end,
}

local operator = {
   "=",">","<","!","~","?",":",
   "==","<=",">=","!=","&&","||","++","--",
   "+","-","*","/","&","|","^","%","<<",">>",">>>",
   "+=","-=","*=","/=","&=","|=","^=","%=","<<=",">>=",">>>=",
}

local separator = S("(){}[];,.")

local modifier = {
   "public", "protected", "private", "static", "abstract", "final", "native", "synchronized",
   "transient", "volatile", "strictfp",
}

local basic_type = {
   "byte", "short", "char", "int", "long", "float", "double", "boolean",
}

local keyword = {
   "abstract" ,  "continue" ,  "for"        ,  "new"       ,  "switch",
   "assert"   ,  "default"  ,  "if"         ,  "package"   ,  "synchronized",
   "boolean"  ,  "do"       ,  "goto"       ,  "private"   ,  "this",
   "break"    ,  "double"   ,  "implements" ,  "protected" ,  "throw",
   "byte"     ,  "else"     ,  "import"     ,  "public"    ,  "throws",
   "case"     ,  "enum"     ,  "instanceof" ,  "return"    ,  "transient",
   "catch"    ,  "extends"  ,  "int"        ,  "short"     ,  "try",
   "char"     ,  "final"    ,  "interface"  ,  "static"    ,  "void",
   "class"    ,  "finally"  ,  "long"       ,  "strictfp"  ,  "volatile",
   "const"    ,  "float"    ,  "native"     ,  "super"     ,  "while",
}

-- http://docs.oracle.com/javase/specs/jls/se7/html/jls-18.html

local line        = patterns.line
local whitespace  = patterns.whitespace
local space       = patterns.space
local spacer      = patterns.spacer
local eol_comment = P("//") * (P(1) - patterns.newline)^1 * patterns.newline

local java_letter = patterns.letter + P("_") + P("$")
local java_digit  = patterns.digit

local char_literal = patterns.singlequoted
local string_literal = patterns.doublequoted
local boolean_literal = P("true") + P("false")
local null_literal    = P("null")
local identifier = (( java_letter * (java_letter + java_digit)^0 )) -
   (lpeg.oneof(keyword) + boolean_literal + null_literal)

local grammar = visualizers.newgrammar(
   "default",
   {
      "visualizer",

--      Annotation = ,

--      Modifier =
         -- V("Annotation") +
--         ,

      Separator = mp(handler, "separator", separator),

      Operator = mp(handler, "operator", lpeg.oneof(operator)),

      TraditionalComment =
         mp(handler, "trad_comment", P("/*")) *
         (V("line") + V("whitespace") +  mp(handler, "trad_comment", (P(1) - P("*/"))))^0 *
         mp(handler, "trad_comment", P("*/")),

      EolComment = mp(handler, "eol_comment", eol_comment),

      Modifier = mp(handler, "modifier", lpeg.oneof(modifier)),
      BasicType = mp(handler, "basic_type", lpeg.oneof(basic_type)),

      Character = mp(handler, "char", char_literal),
      String = mp(handler, "string_", string_literal),
      Boolean = mp(handler, "boolean", boolean_literal),
      Null = mp(handler, "null", null_literal),
      Literal = V("Character") + V("String") + V("Boolean") + V("Null"),

      Comment =
        V("TraditionalComment") + V("EolComment"),

      -- ImportDeclaration:
      --    import [static] Identifier { . Identifier } [. *] ;
      ImportDeclaration = mp(handler, "import", P("import")) * V("space")^1 *
         (mp(handler, "modifier", P("static")) * V("space")^1)^-1 *
         (mp(handler, "import_id", identifier) * mp(handler, "separator", P(".")))^0 *
         ( mp(handler, "import_term", identifier) + mp(handler, "operator", P("*")) ) *
         mp(handler, "separator", P(";")),

      Package =  mp(handler, "package", P("package")) * V("space")^1 *
         (mp(handler, "default", identifier) * mp(handler, "separator", P(".")))^0 *
         mp(handler, "package_term", identifier) * mp(handler, "separator", P(";")),

      -- Identifier:
      --     IdentifierChars but not a Keyword or BooleanLiteral or NullLiteral
--      Identifiers = ,

      -- IdentifierChars:
      --     JavaLetter
      --     IdentifierChars JavaLetterOrDigit
--      IdentifierChars =  -- simplified. Ask Hans for utf8 letters-only pattern.
--         (java_letter * (java_letter + java_digit)^0) - lpeg.oneof(),

      -- JavaLetter:
      --     any Unicode character that is a Java letter (see below)

      -- JavaLetterOrDigit:
      --     any Unicode character that is a Java letter-or-digit (see below)

      -- CompilationUnit:
      --     [[Annotations] package QualifiedIdentifier ;]
      --                                 {ImportDeclaration} {TypeDeclaration}

      --    ( V("Annotations")^-1 * V("whitespace")^0 *
      --    V("whitespace")^0 * V("TypeDeclaration"),

      pattern =
         V("Package") +
         V("ImportDeclaration") +
         V("Comment") +
         V("Literal") +
         V("Operator") +
         V("Separator") +
         V("BasicType") +
         V("Modifier") +
         V("space") +
         V("line") +
         V("default"),

      visualizer = V("pattern")^1
   }
)

local parser = P(grammar)

visualizers.register("java", { parser = parser, handler = handler, grammar = grammar } )
