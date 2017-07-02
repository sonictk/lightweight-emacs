# Lightweight Emacs

## About

This is a lightweight emacs configuration focused on editing C/C++,
Python, and Markdown. 

## Why?

I decided to start working on a brand-new configuration after my
previous one started to become a little unwieldy/slow, along with the
fact that I was getting tired of having a configuration that I didn't
really fully understand. Additionally, I was tired of updating
packages from MELPA that broke in newer versions or had changed in
behaviour, and resulted in my configuration behaving slightly
different across my different workstations (which was supposedly part
of the reason why I switched to using Emacs in the first place).

Finally, I was tired of having my configuration dump a ton of stuff in
my user folder. I wanted my configuration to all stay in a single
directory, and not affect my existing Emacs installation or normal configuration.

## What?

This configuration does not rely on MELPA or any external package
manager. It is focused on using only the plugins I find useful
instead of starting with a gazillion ones that Prelude installs, and
writing my own functions for the rest of the functionality that I require.

## How?

You will need the following dependencies, regardless of your platform:

* Emacs 24.5.1 (Newer/older versions are untested)
* clang/llvm
* grep
* gdb
* CMake
* GNU Global Tags (gtags)

After that, just clone this repository with all its submodules and (hopefully)
things should work. What I do is launch Emacs using this command:

``emacs -q --load ~/Git/lightweight-emacs/lightweight_config.el``

You can run this side-by-side with your current Emacs installation if
you just want to check it out using this method.

## Who?

Siew Yi Liang a.k.a. sonictk
