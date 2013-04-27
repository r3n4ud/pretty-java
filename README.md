<!--- coding: utf-8; fill-column: 80 --->
# pretty-java: a ConTeXt MkIV java syntax highlighting third party module

* [Homepage](https://github.com/nibua-r/pretty-java#readme)
* [Issues](https://github.com/nibua-r/pretty-java/issues)
* [Email](mailto:root at renaud.io)

## Description

pretty-java is a [ConTeXt](http://wiki.contextgarden.net/Main_Page) module and provides Java syntax
highlighting.

## Features

Java syntax highlighting.

## Examples

```tex
\usemodule[t-pretty-java]
\setupbodyfont[8pt]

\starttext
\starttyping[option=java]
public class HelloWorld {

    public static void main(String[] args) {
        System.out.println("Hello, World");
    }

}
\stoptyping
\stoptext
```

A sample document is available
[here](https://github.com/nibua-r/pretty-java/blob/rework-jls-se7/doc/context/third/pretty-java/test.pdf?raw=true).

## Requirements

You'll need the [ConTeXt Standalone distribution](http://wiki.contextgarden.net/ConTeXt_Standalone).
pretty-java may work with packaged ConTeXt but it has not been tested yet.

## Install

At that early stage of development, I recommand to clone the repo and symlink the `pretty-java`
directory to `tex/texmf-modules/tex/context/third/pretty-java`.

## Contributing to pretty-java

* Use pretty-java and report any issue with the Java snippet involved
* Follow the usual fork/branch/PR workflow to send changes, if I like them I'll merge them

## Copyright

Copyright (c) 2013 Renaud AUBIN
