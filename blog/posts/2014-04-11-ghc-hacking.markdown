---
title: GHC Hacking
author: Christopher Reichert
tags: ghc, haskell
---

<div style="text-align:center" markdown="1">
  <img src="/images/haskell_logo2.png" alt="Haskell Lambda" style="height: 250px;"/>
</div>

If you haven't figured it out already, I am a [Haskell](http://haskell.org/) fanatic. Haskell is a
fantastic language that has continuously driven me to explore new programming
concepts and improve my own skill.  Quite naturally, I am intrigued by [GHC --
The Glasgow Haskell Compiler](). GHC is an open-source native code compiler for
Haskell. GHC is written in Haskell with the majority of the runtime system
written in C and C-\- [[0]](#0).

This post describes how to:

* Check out and build the ghc source.
* Add libraries to your inplace ghc instance.
* Make a trivial change to GHCi.

<!--more-->

Some of the highlights of the GHC compiler are described on [haskell.org/ghc](http://haskell.org/ghc):

> * GHC supports the entire Haskell 2010 language plus a wide variety of
>   extensions.
> 
> * GHC has particularly good support for concurrency and parallelism,
>   including support for Software Transactional Memory (STM).
> 
> * GHC generates fast code, particularly for concurrent programs. Take a
>   look at GHC's performance on The Computer Language Benchmarks Game.
> 
> * GHC works on several platforms including Windows, Mac, Linux, most
>   varieties of Unix, and several different processor architectures. There are
>   detailed instructions for porting GHC to a new platform.
> 
> * GHC has extensive optimisation capabilities, including inter-module
>   optimisation.
> 
> * GHC compiles Haskell code either directly to native code or using LLVM as
>   a back-end. GHC can also generate C code as an intermediate target for porting
>   to new platforms. The interactive environment compiles Haskell to bytecode, and
>   supports execution of mixed bytecode/compiled programs.
> 
> * Profiling is supported, both by time/allocation and various kinds of heap
>   profiling.
> 
> * GHC comes with several libraries, and thousands more are available on
>   Hackage.

That's a lot of great reasons to give the compiler a closer look. Before we get
started, there are tons of resources to be found on the
[GHC wiki](http://www.haskell.org/ghc/) and even on the
[GHC github](https://github.com/ghc/ghc).

## Compiling GHC

Compiling GHC is actually fairly straightforward.

1. Get the code:

``` bash
    git clone git@github.com:ghc/ghc.git
```

2. Sync the repository:

``` bash
    cd ghc
    ./sync-all get
    perl boot
```

3. Configure and build:

``` bash
    ./configure
    make -j4
```

This will take about an hour. If all is succesful you should have your shiny new
GHC compiler in `./inplace/bin/ghc-stage2`.

## Adding Extra Package to inplace GHC.

Assuming you have a somewhat recent version of `cabal` installed, you can add
extra packages to your inplace GHC using:

``` bash
    cabal install --with-compiler=/path/to/ghc/inplace/bin/ghc-stage2 <package>
```

You should then be able to list packages using `ghc-pkg`:

```
    ./inplace/bin/ghc-pkg list
```

## Making changes to GHC

The goal is to hack on GHC so let's make a change. As a trivial example, I will
change the banner text for GHCi.

First, open `GHC/InteractiveUI.hs`.

On line 128 we see the function for showing the GHCi welcome msg.

``` haskell
    GHCiWelcomeMsg :: String
    GHCiWelcomeMsg = "GHCi, version " ++ cProjectVersion ++
                     ": http://www.haskell.org/GHC/  :? for help"
```

Next, add your own flair to the banner:

``` haskell
    GHCiWelcomeMsg :: String
    GHCiWelcomeMsg = "Hello GHC!\n" ++ "GHCi, version " ++ cProjectVersion ++
                     ": http://www.haskell.org/GHC/  :? for help"
```

The quickest way to recompile GHC after making changes like this is to run
`make stage=2` from the GHC directory.

``` bash
    cd $(TOP)/ghc
    make 2
```

`make 2` is an alias for `make stage=2 FAST=YES`.

You can find more information on that process
[here](https://GHC.haskell.org/trac/GHC/wiki/Building/Using#DevelopinginaGHCbuildtree).

Now run GHCi with your new flair:

``` bash
    $ $(TOP)/inplace/bin/GHC-stage2 --interactive
    Hello GHC!
    GHCi, version 7.9.20140412: http://www.haskell.org/GHC/  :? for help
    Loading package GHC-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Prelude> 
```

Happy Hacking!

----------

##### <a name="0"></a>[0] - [http://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler#Architecture](http://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler#Architecture)
