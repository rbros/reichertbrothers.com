---
title: Teasers in Hakyll
author: Christopher Reichert
tags: haskell, hakyll
---

<div style="text-align:center" markdown="1">
  <img src="/images/haskell_logo1.png" alt="Hakyll Lambda" style="height: 250px;"/>
</div>

While building this blog, one of the primary issues I faced was figuring out
how to implement post teasers. Most of the resources I found were using teasers
in an rss or atom feed. The [Hakyll
tutorial](http://jaspervdj.be/hakyll/tutorials/using-teasers-in-hakyll.html) on
teasers was helpful but I found I needed a little more handholding. Partially,
because I misunderstood how template contexts work in Hakyll.

This post describes how to implement teasers in Hakyll on the default generated
test site.

<!--more-->

### Generate Test Site ###

Say you generate a site `foo`:

``` bash
    hakyll-init foo
    cd foo
```

Run the site to view the existing list of posts.

``` bash
    ghc --make -threaded site.hs
    ./site preview
```

Point your browser to [http://localhost:8000](http://localhost:8000). A list
of posts should be displaying on the homepage. All is good.

The goal is to show a preview of each post in the post-list.html template. This
would be visible on the homepage. Luckily, Hakyll has builtin functions to
support extracting this information.

### Modifying the HTML Templates ###

What would this look like from an html perspective?

First, we need to add a template variable for the teaser to our post-list.html
template. This way, a preview is shown every time we use the post-list
template.

This is the html template for a list of posts before we add the teaser:

``` html
    <ul>
        $for(posts)$
            <li>
                <a href="$url$">$title$</a> - $date$
            </li>
        $endfor$
    </ul>
```

Now we will add a variable `$teaser$`:

``` html
    <ul>
        $for(posts)$
            <li>
                <a href="$url$">$title$</a> - $date$
                <p>
                $if(teaser)$
                    $teaser$
                $else$
                    $body$
                $endif$
                </p>
            </li>
        $endfor$
    </ul>
```

If you do not plan on defining `<!--more-->` in each blog post then you should
conditionally check for $teaser$:

    $if(teaser)$
        $teaser$
    $else$
        $body$
    $endif$

Hakyll's `teaserField` function (used to build teasers) will fail if the
`<!--more-->` tag is not defined in the post. The `<!--more-->` tag tells
Hakyll where the teaser stops.

### The Haskell Code ###

Now that we have added the `$teaser$` value to our template we need to make
sure we add the Haskell code to add the teasers to our context. Open up
`site.hs` in your favorite editor:

``` bash
    vim site.hs
```

First, we must save a snapshot when generating posts. This must be done before
any templates are applied:

``` haskell
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
```

We save the snapshot `content` during the compilation phase. Now, we must get
the teaser from the same snapshot when loading a list of posts. Posts are added
to a context using the `listField` function. Here is an example of a route that
loads a list of posts:

``` haskell
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*" 
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext
        ...
```

We must use the `teaserField` function to add the teasers to our context for
each post. Thus, we want to add the teaser to the post context for each
post.

We define a convencience function to create a context for teasers based off
the post's context `postCtx`:

``` haskell
    teaserCtx = teaserField "teaser" "content" `mappend` postCtx
```

Now we can use this context in each post. We must also load the snapshots
during the compile phase using `loadAllSnapshots`:

``` haskell
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let indexCtx =
                    listField "posts" teaserCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext
        ...
```

You should do this for every page that uses the post-list template. In our case this also
include the archive page.

Now you should be able to see teasers on your posts: [http://localhost:8000](http://localhost:8000)

Happy Hacking.
