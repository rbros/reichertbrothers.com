---
title: The Reichert Brothers Blog
author: Christopher Reichert
tags: haskell, hakyll
---

#### Welcome to the Reichert Brothers Blog! ####

<div style="text-align:center" markdown="1">
  <img src="/images/rblogo.png" alt="Reichert Brothers" style="height:300px"/>
</div>


We finally built it! Our hope is to give back to the software development
community that we have learned so much from. We encounter a lot of peculiar
problems throughout our hacking journey and we want to make this a forum to
discussion related issues.

<!--more-->

##### What is the blog built on? #####

This blog (and our entire site) is built on
[Hakyll](http://jaspervdj.be/hakyll/), a static site generator written in
[Haskell](http://haskell.org).

> Static sites are fast, secure, easy to deploy, and manageable using version control.
> --- <cite>Hakyll documentation</cite>

If you don't know any Haskell don't be intimidated! The amount of Haskell code
it takes to get up and running is quite minimal. The rest of this post will be
a short tutorial on getting started with Hakyll. This is also covered in
[The Hakyll Documentation](http://jaspervdj.be/hakyll/tutorials/01-installation.html).

##### How can I build a Hakyll site? #####

Hakyll is simple to install if you are familiar with `cabal`:

``` bash
    cabal install hakyll
```

Use the `hakyll-init` executable to generate an example site:

``` bash
    hakyll-init foo
```

Then compile your site using `ghc`:

``` bash
    cd foo
    ghc --make -threaded site.hs
```

You can now run the hakyll preview server to test the site:

``` bash
    ./site preview
```

Your site should be visible at [http://localhost:8000](http://localhost:8000).

Not only does Hakyll provide the preview server, it also integrates extremely
easily with [NGinx](http://nginx.org), [Apache](http://apache.org), and other
web servers. Use the build command to compile the site:

``` bash
    ./site.hs build
```

This command creates a directory `_site` which contains the compiled site.
NGinx, for example, can point directly to this using the root directive:

    root /path/to/foo/_site;

Happy Hacking!
