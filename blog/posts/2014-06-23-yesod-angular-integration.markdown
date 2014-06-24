---
title: Yesod and Angular JS
author: Christopher Reichert
tags: yesod, angular, javascript
---

<div style="text-align:center; margin-right: 20px; margin-bottom: 20px" markdown="1">
  <img src="/images/yesod.png" alt="Yesod Web Framework"
       style="height:200px; float: left; display: inline-block; margin-right: 20px"/>
</div>

Recently, while working on a [Yesod](http://www.yesodweb.com/) site I wanted to
integrate [Angular JS](https://angularjs.org/) to spice up the design and add
dynamic content.

Researching Angular js and Yesod integration via search engines doesn't yield
many helpful results. Michael Snoyman has an example from 2012
([http://www.yesodweb.com/blog/2012/10/yesod-fay-js](http://www.yesodweb.com/blog/2012/10/yesod-fay-js))
with a module named `Yesod.Angular` provided for integrating Angular with a
Haskell/Yesod backend. The problem is most of the code in the module was
written against an older version of Yesod, and thus, does not compile with
current versions.

I have proposed a [pull request](https://github.com/snoyberg/yesod-js/pull/2)
to update the example to work with Yesod 1.2, so those who stumble upon it are
not confused or blocked. The pull request was merged and should be in the
[yesod-js](https://github.com/snoyberg/yesod-js) repository now.

<!--more-->

If you want to integrate the
[Yesod.Angular](https://github.com/creichert/yesod-js/blob/master/yesod-angular/Yesod/Angular.hs)
module into your project you will need to include it directly in your code. I
am working to make this module usable as a library so feel free to comment with
any suggestions.

The complete code can be found on [Github](https://github.com/creichert/yesod-js/commit/6dd9989c4232dabcd7eae977a2d3722a34605064)

Happy Hacking!
