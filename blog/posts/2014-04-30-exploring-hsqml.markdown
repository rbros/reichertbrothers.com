---
title: Exploring QML In Haskell
author: Christopher Reichert
tags: qml, qt, haskell
---

<div style="text-align:center" markdown="1">
  <img src="/images/haskell_logo2.png" alt="Haskell logo"
       style="height:200px; float: left; display: inline-block"/>
</div>
<div style="text-align:center" markdown="1">
  <img src="/images/multiply.png" alt="Plus symbol."
       style="height:100px; float: left; display: inline-block; padding-left: 40px; margin: 40px 40px"/>
</div>
<div style="text-align:center" markdown="1">
  <img src="/images/qt-logo.png" alt="Qt logo"
       style="height:200px; display: inline-block"/>
</div>

I have recently been experimenting with the Haskell QML binding
[HsQML](http://hackage.haskell.org/package/hsqml).  I am a big fan of QML and
it's portability. It's a very flexible language for user interface development
and it makes for a powerful combination with Haskell.

Recently, I wrote about integrating [QML code and Haskell using
Fay](http://reichertbrothers.com/blog/posts/2014-04-15-qml-haskell-integration-with-fay.html).

HsQML, however, is a more direct way of integrating QML and Haskell. The HsQML
approach has the value of the Haskell runtime and garbage collector, among
other things (though, Fay may compile the garbage collector, not sure).

This post describes working with HsQML < 0.3.

<!--more-->

<div class="warning" >
As of HsQML 0.3 many of the issues I describe in this post are fixed. I will
have a follow-up post soon talking about HsQML 0.3!
</div>

HsQML is a binding to the QML engine from Haskell. The library is still fairly
young but it is extremely useful and can definitely get the job done. The
current HsQML packages in Hackage only support Qt4/QtDeclarative but there is
Qt5/QtQml support in the [HsQML Darcs
Repository](http://hub.darcs.net/komadori/HsQML/). I have tested HsQML with Qt5
and everything seems to be working very well.

I have begun implementing a [TorChat](https://github.com/prof7bit/TorChat)
client in Haskell using HsQML.  You can find the code on my
[Github](https://github.com/creichert/hstorchat). Contributions welcome!
HSTorChat uses many basic features of HsQML and can definitely be used as an
example for developing more complex user interfaces in QML.

TorChat is a simple messenger application that is built on top of Tor's hidden
services. When an HSTorChat client is talking to a buddy, outgoing messages are
gauranteed to be sent to the correct onion address.

> Everyone – including the introduction points, the distributed hash table
> directory, and of course the clients – can verify that they are talking to
> the right hidden service
>
> -- https://www.torproject.org/docs/hidden-services.html.en

<br/>

# HSTorChat

<br/>

#### State

Managing state in a Haskell GUI application is not always trivial. I was
largely unable to use Haskell data structures as QML models. I initially
attempted to define read-write properties which could be updated from QML.
However, I found that with lack of support for property signals I was unable to
get QML to always update the views to reflect the state. I was able to work
through these issues more easily in HsQML 0.3.

Because each buddy is managed on a differed thread, there must be some way to
coordinate changes with the ui.  The buddy list serves as the point of
communication between threads and uses an
[MVar](https://hackage.haskell.org/package/base-4.0.0.0/docs/Control-Concurrent-MVar.html).
When a new buddy authenticates, the corresponding onion address is added to an
MVar'd buddy list using `takeMVar` and `putMVar` (or `withMVar` to combine the
operations):

```haskell
    buds' <- takeMVar $ _buddies ui'
    putMVar (_buddies ui') (b:buds')
```

***(This should be done using withMVar)***

Since each buddy is managed in it's own thread, the MVar enables syncing
messages from the ui to the corresponding buddy.

#### Signals

When a new message is received a signal is sent from Haskell to QML. 

```haskell
    m <- newObject $ Msg (T.unpack msg) onion
    fireSignal (Tagged ui :: Tagged MsgReady (ObjRef UI)) m
```

`fireSignal` is called from the thread managing the Buddy. The HsQML
documentation has been updated to reflect that the function safe
to call from any thread:

http://hub.darcs.net/komadori/HsQML/patch/20140507214126-4d2ae

#### Calling Haskell functions from QML

Calling Haskell functions from QML is quite simple. In GUI.hs, `sendMsg`
defines a function on the main context object which takes three parameters:

```haskell
 instance Object UI where
      classDef = defClass [
            , defMethod "sendMsg" sendMsg
            ...
            ]
    
    sendMsg :: ObjRef UI -> T.Text -> T.Text -> IO ()
    sendMsg ui onion msg = do
        let ui' = fromObjRef ui
        buds <- readMVar $ _buddies ui'
        sendMsgTo buds
      where
        sendMsgTo []   = putStrLn "Unable to send msg: no buddies."
        sendMsgTo buds = do
            -- Filter proper buddy from list.
            let buddy = head $ filter (λb -> _addy b == T.unpack onion) buds
            hPutStrLn (_out_conn buddy) $ lowercase $ filter (/= '"') $ show $ Message msg
            return ()
```

`sendMsg` takes a reference to the UI object, a buddy name, and a message.

I admit, It would be better to experiment with constructing and passing a
complete Msg type from QML instead of the onion and msg individually.  The
function definition could be more readable.

If you happen to peruse the code, you might notice there is also some
discrepency between the ProtocolMsg Message and Msg type. I hope to combine the
two in the next few revisions of HSTorChat.

Here is an example call to `sendMsg` in the `onAccepted` slot of a QML
[TextInput](http://qt-project.org/doc/qt-4.8/qml-textinput.html) component.

```javascript
    onAccepted: { sendMsg("buddyname", "buddyonion") }
```

#### Model/View

There is no straight forward way to implement data models for QML in Haskell
that I am aware of. It would be nice to support actual `ListModel` declarations
some way from within Haskell.

Update (5/6/2014): As of HsQML 0.3 I have had success using Haskell Lists as
QML data models. See here for details
[https://github.com/creichert/hstorchat/commit/f3175e939fa5c10735e5fcdeec02a9383fa31d74](https://github.com/creichert/hstorchat/commit/f3175e939fa5c10735e5fcdeec02a9383fa31d74)

#### Error reporting

One thing that is very difficult is getting the output of errors in the event
of a QML crash or error creating a component. Most of the time the failure is
silent.  I have not yet found a way to work around this besides testing the qml
file separately with `qmlscene`.

Update: HsQML 0.3, however, has had fantastic error reporting.

# Side-Note

[Attoparsec](https://hackage.haskell.org/package/attoparsec) and
[Parsec](https://hackage.haskell.org/package/parsec) are extremely cool parser
combinator libraries for Haskell.

Anytime HSTorChat receives a new data packet on it's input connection it
attempts to apply a series of parser combinators which basically turn the
message into the type of `ProtocolMsg` the data represents.

```haskell
    data ProtocolMsg = Ping | Pong ...

    parseResponse :: Parser ProtocolMsg 
    parseResponse =  try parsePing
                 <|> try parsePong
                 <|> try parseVersion
                 <|> try parseClient
                 <|> try parseStatus
                 <|> try parseAddMe
                 <|> try parseDelayedMsg
                 <|> parseMsg
```

Check out the definition of the ProtocolMsg
[here](https://github.com/creichert/hstorchat/blob/master/src/Network/HSTorChat/Protocol.hs#L56).

Check out the definition of the combinators
[here](https://github.com/creichert/hstorchat/blob/master/src/Network/HSTorChat/Protocol.hs#L119).

# Coming Soon

HSTorchat in it's current state accomplishes quite a lot. My next goal is to
make HSTorChat into a full fledged chat client and port it to the Raspberry Pi.

I will also be experimenting with using Fay to generate JavaScript library code
in an HsQML application. More to come!

Happy Hacking!
