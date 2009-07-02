Parakeet Twitter Client for Emacs
=================================

Parakeet is a simple Twitter client for Emacs. There are other Twitter
clients for Emacs out there and they are pretty nice, but none of them
worked with the SOCKS proxy that I have to deal with at work. I wrote
this client both as an excerise in learning Elisp and to get something
together that I could use from the office.

Requirements
------------

You'll need to have the [json.el][0] library available for Parakeet to
work. That's it!

Installation
------------

Installation isn't too bad. Check this project out, preferably into
your Emacs configuration folder. Mine is called ".emacs.d" and keep
all of my Lisp files in "site-lisp".

    cd ~/.emacs.d/site-lisp
    git clone git://github.com/cmiles74/Parakeet.git parakeet

You can then add the following to your main Emacs configuration file:

    (add-to-list 'load-path "~/.emacs.d/site-lisp/parakeet")
    (load "~/.emacs.d/site-lisp/parakeet/autoload.el")

The autoload.el file will load in Parakeet and set up the default key
bindings.

Lastly, you'll want to tell parakeet your username and password. Add
the following (substituting in your own information) to the bottom of
your Emacs configuration file.

    (custom-set-variables '(parakeet-twitter-user "your user name"))
    (custom-set-variables '(parakeet-twitter-password "your
        password"))

That's all there is to it.

Usage
-----

Using Parakeet is easy! To post to your Twitter feed...

    C-c ' p u

That's press the "control" key and "c" together, then type apostrophe,
"p" and "u". A new window will appear and you can type your status
in. Once you are happy with your status, type...

    C-c C-c

That's the "control" key and "c", then the "control" key and "c"
again. Parakeet will post your status and then close the window.

You can also view your friend's tweets and the public feed.

    C-c ' p p   View the public Twitter feed
    C-c ' p f   View your friend Twitter feed

When you are viewing the feeds, you can press "C-n" and "C-p" to move
up and down a tweet at a time.

Future Plans
------------

Well, I have some plans and this is really the barest bones of what a
Twitter client should be. I'd like to add some functions for viewing
just one account's feed as well as functions for re-tweeting, replying
and direct messaging. Functions to automatically shorten URL's (using
aservice like Bit.ly) are also high on my to-do list.

Feedback Appreciated
--------------------

If you notice any problems or have any recommendations, please let me know!


[0] http://edward.oconnor.cx/2006/03/json.el "json.el Homepage"
