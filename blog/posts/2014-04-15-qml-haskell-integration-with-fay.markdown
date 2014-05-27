---
title: QML integration with Fay and Haskell
author: Christopher Reichert
tags: qml, qt, haskell
---

<div style="text-align:center" markdown="1">
  <img src="/images/qt-logo.png" alt="Qt logo"
       style="height:200px; float: left; display: inline-block"/>
</div>
<div style="text-align:center" markdown="1">
  <img src="/images/plus.png" alt="Plus symbol."
       style="height:100px; float: left; display: inline-block; padding-left: 40px; margin: 40px 40px"/>
</div>
<div style="text-align:center" markdown="1">
  <img src="/images/haskell_logo2.png" alt="Haskell logo"
       style="height:200px; display: inline-block"/>
</div>

If you are reading this, you are probably more or less aware of the [JavaScript
Problem](http://www.haskell.org/haskellwiki/The_JavaScript_Problem). Like many,
I frequently work with JavaScript. There is no avoiding JavaScript on the web
and many corners of the software development industry.

My most recent JavaScript endeavor has been in the world of
[Qt5](https://qt-project.org/wiki/Qt_5.0) and
[QML](http://qt-project.org/doc/qt-5/qtqml-index.html).  QML is a solid toolkit
for writing fluid and cross-platform user interfaces.

Recently, while experimenting with QML, I had a revelation. What if it was
possible to generate QML code from Haskell using Fay. Eureka!

<!--more-->

Luckily, [Fay](https://github.com/faylang/fay/wiki) is up for the job. Fay is a
Haskell to Javascript compiler which supports a proper subset of the Haskell
language.  Fay has several distinct advantageous:

> * Statically typed
> * Lazy
> * Pure by default
> * Compiles to JavaScript
> * Has fundamental data types (Double, String, etc.) based upon what JS can support, and compound data types (ADTs and GADTs)
> * Outputs minifier-aware code for small compressed size
> * Has a trivial foreign function interface to JavaScript
> * Supports cabal installation of Fay packages
> * Can automatically transcode values to/from JSON using the FFI
> * Provides an API to transcode on the server side as well
> * Lets you call Fay code from JavaScript
> * Has the Fay monad for side effects (think of it like IO)
>
> -- Fay Wiki

This post will cover how to integrate a JavaScript library generated using Fay
into a QML application. The library will be developed purely in Haskell while
the UI code will be written in QML.

QML, while a subset of JavaScript, is essentially it's own language. Fay does
not understand QML and therefore cannot compile it to JavaScript yet.

My goal is to, first, get the JS library integration functioning. After that,
the issue of getting Fay to compile QML Forms written in Haskell can be
evaluated and potentially implemented.

The most up-to-date version of the `qmlfay` example source is on my
[Github](https://github.com/creichert/qmlfay). I would like to get this unique
example into the Fay repository if anyone else finds it useful.

# Interfacing a Haskell Library With QML Using Fay

The first objective is to pass
[QtObjects](http://qt-project.org/doc/qt-4.8/qml-qtobject.html), created in
QML, to a Fay JavaScript library.  In the library, properties should be read
from the QtObject and the data can be processed accordingly.

Make sure `fay` and `fay-base` are installed.

``` bash
    cabal install fay fay-base
```

##### Haskell

Let's start with the fun part. The Haskell module is named `Library`
and exports a function named `qObjectName`. This function should return the
objectName property of a QML
[QtObject](http://qt-project.org/doc/qt-4.8/qml-qtobject.html).

``` haskell
    module Library (qObjectName) where
```
 
The data type QObject is defined to represent our QML component (which is based
on the QtObject element).  Unfortunately, I have not yet found a way to
generate this record based on the actual C++ QObject type properties.
Fortunately, we only need fields for each property used. As far as I can tell,
Fay matches the record entry with the JavaScript object's property when
applying this type.

   
``` haskell
    data QObject = QObject { objectName :: String }
```

The function `qObjectName` takes a QObject parameter and returns the objectName.
`undefined` will be returned if the `objectName` property doesn't exist.

``` haskell
    qObjectName :: QObject -> String
    qObjectName qo = objectName qo
```

Next, some boilerplate is necessary to initialize the QML application.

## Boilerplate

First, we need a main.cpp file to run the application. While it is technically
possible to run the app using `qmlscene`, I wanted to show an example with
qmake so it can be more easily adapted to existing qmake build systems.

##### Main.cpp

``` cpp
    #include <QGuiApplication>
    #include <QQuickView>
    #include <QUrl>

    int main(int argc, char** argv) {
    
        QGuiApplication a(argc, argv);
    
        QQuickView view;
        view.setSource(QUrl("./Main.qml"));
        view.show();
    
        return a.exec();
    }
```

The `QQuickView`'s source is set to `Main.qml` which is our QML entry point.

##### Main.qml

`Main.qml` starts off with imports. Our Fay generated JavaScript library,
`Library.js`, is a qualified import named `Library`.

``` javascript
import QtQuick 2.0 
import "Library.js" as Library
```

Next, there is a `Rectangle` element. The `objectName` property, which will
be accessed from `Library.js`, is set to `"MyObject"`.

We set the component `id` to `page` for reference.

``` javascript
Rectangle {
    id: page
    width: 100; height: 100 
    objectName: "MyObject"

    Component.onCompleted: {
```

After a reference to the `Library` object is retrieved,  it's possible to run
`L.objectName(page)`. Note that the id is being passed to the `qObjectName`
function. The id is reference to the JavaScript object `page`.

``` javascript
        var L = Library.Strict.Library; // Pass --strict to fay compiler.
        console.log(L.qObjectName(page))
    }   
}
```

##### qmlfay.pro

The qmake project file defines how the project should be built. `qmake` offers
macros to extend the build with extra compilers. This should work perfectly for
the Fay compiler build rules as their compilation is independent of the C++
sources.


``` bash
    TEMPLATE = app 
    TARGET = qmlfay
    INCLUDEPATH += .
    
    QT += gui quick
    CONFIG += target_predeps
    
    SOURCES += main.cpp
    FAY_SOURCES += Library.hs
    
    fay.name = fay compiling
    fay.input = FAY_SOURCES
    fay.output = ${QMAKE_FILE_IN_PATH}/${QMAKE_FILE_BASE}.js
```

Now the actual compile command is defined. Note the `--strict` flag.

``` bash
    fay.commands = fay ${QMAKE_FILE_NAME} -p --strict ${QMAKE_FILE_BASE}
    fay.variable_out = PRE_TARGETDEPS
    QMAKE_EXTRA_COMPILERS += fay 
```

## Compile and Run

Now compile and run the application.

``` bash
    qmake-qt5 .
    make
    ./qmlfay
```

## Issues

##### Expected token `identifier` error:

You might run into the following error importing the JavaScript sources from QML:

``` bash
    $ ./qmlfay 
    file:///.../dev/qmlfay/Main.qml:3:1: Script file:///.../dev/qmlfay/Library.js unavailable 
         import "Library.js" as Library 
         ^
    file:///.../dev/qmlfay/Library.js:2230:17: Expected token `identifier' 
                     var as = $tmp1.cdr;
```

The workaround, for the time being, is to comment out the following functions
from the generated javascript `Library.js`: `zipWith`,`zipWith3`,`zip`,`zip3`.
This also means these functions are unusable for now.

## Conclusions

My goal is to bring a usable QtQuick experience to Haskell through the use of
Fay.  Once I have developed tighter library integration I plan to attempt to
write a pass in the Fay compiler to handle pure qml.

I'm not unambiguously certain that all of these things are possible but this is
an itch I must scratch.

Happy Hacking!
