---
title: The WP-Evernote Plugin
author: Christopher Reichert
date: 2014-04-06
tags: evernote, php, wordpress
---
<div style="text-align:center" markdown="1">
  <img src="/images/wp-evernote.png" alt="WP-Evernote logo" style="height:300px"/>
</div>

<hr>

###### TL;DR We wrote an Evernote Plugin for Wordpress: [WP-Evernote](https://github.com/creichert/wpevernote) ######

Recently, my brother and I were asked to add support for publishing blog posts
on a [Wordpress](http://wordpress.org) site from
[Evernote](http://evernote.com). While taking a look at some existing plugins
that might support this workflow, I came across
[Everpress](http://wordpress.org/plugins/everpress/). Unfortunatly, Everpress
is based off using an RSS feed which is no longer supported by Evernote (and
hasn't been for some time).

<!--more-->

We took this awesome opportunity to develop a custom Wordpress plugin using the
Evernote api. WP-Evernote can publish notes in an Evernote public notebook to
wordpress posts and also keep them up-to-date. WP-Evernote is still under
development and can be found on my
[Github](https://github.com/creichert/wpevernote). WP-Evernote is written in
PHP and uses the [Evernote PHP
SDK](https://github.com/evernote/evernote-sdk-php).

This post describes how to install and test the current development version of
WP-Evernote.

<div class="warning" >
If you do not know how to use Git I will be more than happy to create a zip
archive for you. I will be providing zip archive release files on Github when
the plugin is more user friendly.
</div></br>

### Install ###

Navigate to the Wordpress plugin directory:

``` bash
    cd wp-content/plugins/
    git clone git@github.com:creichert/wpeverpress
```

There should now be a copy of the wpeverpress source in the Wordpress plugin
directory. There are a few files of interest in the plugin:

``` bash
    $ tree wpevernote
        wpevernote
        ├── evernote-sdk-php
        ├── readme.txt
        ├── wpevernote-panel.html
        └── wpevernote.php
```

`wpevernote.php` contains the php code that drives the plugin.
`wpevernote-panel.html` contains the WP-Evernote settings panel.
`evernote-sdk-php` contains the Evernote api client library. The `readme.txt`
file contains documentation about wpevernote.

### Configure ###

In Wordpress, Go to the `Settings -> WP Evernote` settings panel.

The first thing you must do is setup api keys so you can fetch data from
Evernote. To get API keys go to the [Evernote API](http://dev.evernote.com/doc/)
website. Click on the "Get An API Key" button and enter the form details:

* Evernote Username
* Developer Name
* Developer Email
* Organization
* App Name
* Describe app

You will be presented with your api credentials immediately. These keys will
only work on the Evernote sandbox website and are not enabled for the
production website. There is a small process involved in getting these api keys
authorized to use the production servers.

Go back to the WP-Evernote settings panel and enter in the consumer key and
consumer secret in the Settings form. Then hit "Save Settings".

You should be redirected to an Evernote OAuth authentication website. You must
Authorize the plugin here in order for WP-Evernote access to the Evernote data.

Upon successful authentication, you will be directed back to the WP-Evernote
settings panel. Now you can add your first notebook.

<div class="warning" >
The Evernote API uses OAuth for authentication. You must
be on a valid url capable of dns lookup. This is a major limitation of testing
sandbox data. You can get access to your sandbox auth token directly to avert
this issue.
</div><br/>

### Add A Notebook ###

Assuming you authenticated correctly, it's time to add your first notebook. In order
to add a notebook, the notebook must be public.

Place the full url of your public notebook in the `Publice notebook url` form area then
click the `Add Notebook` button. If your notebook was added correctly you should now see it
in the `Current Notebooks` area.

A post will be created for each note in your notebooks. The posts are uniquely
identified by the Evenote guid for notes. Therefore, if you were to change the
title of the note in evernote and update your notebooks, they will properly
import and update the correct posts without creating duplicates.

### Updating your notebooks ###

Support for scheduled notebook updates is still under development.

You can manually update your notebooks using the `Update Notebooks` button on
the WP-Evernote settings panel.

As mentioned before, notes are synced based on their Evernote guid. Any changes
you make in Evernote will sync to the correct post. This helps to avoid
duplicate posts.

### Conclusions ###

Despite some of the complications using api keys, we think this plugin will be
an extremely useful tool for people who use Evernote and Wordpress. We are
looking for testers so we can improve the user experience and add features. If
you would like to help test or even contribute to the code, feel free to
[Contact](/#contact) me.

Cheers!
