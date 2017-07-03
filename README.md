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

* Emacs 24.5.1 (Newer/older versions are untested, though I'm hoping this works
  with 25 out of the box)
* [clang/llvm](https://clang.llvm.org/)
* grep
* gdb
* [CMake](https://cmake.org/)
* [GNU Global Tags](https://www.gnu.org/software/global/)
* [ag](https://github.com/ggreer/the_silver_searcher#installation)
* Git

After that, just clone this repository with all its submodules and (hopefully)
things should work. 

``git clone --recursive <git-repo-url>``

What I do is launch Emacs using this command:

``emacs -q --load ~/Git/lightweight-emacs/lightweight_config.el``

``irony-mode`` is used for completion. You will need to create a
``.clang_complete`` file that contains compilation options for your project
(determined via whether your project folder is a Git repository or not)

Sample ``.clang_complete`` file used for C/C++ completion via irony:

```
-std=c++14
-Wall
-Wextra
-pthread
-Wno-pragma-once-outside-header
-Iinclude
-I/home/sonictk/Qt5.8.0/5.8/gcc_64/include
```

On Windows:

```
-std=c++14
-Wall
-Wextra
-pthread
-Iinclude
-Ic:/Qt/4.8.5/include
```

You should also specify some directory-local variables in order for header
completion and finding of system headers.

Sample ``.dir-locals.el`` file used for header completion:

```
(
 (nil . ((tab-width . 4)
     (indent-tabs-mode . t)))

 (c++-mode . ((c-basic-offset . 4)
        (tab-width . 4)
        (indent-tabs-mode . t)
        (compile-command . "cmake --build ./build --config Release --target INSTALL")
        (cc-search-directories . ("."
                    "./include"
                    "/home/sonictk/Qt5.8.0/5.8/gcc_64/include"
                    "/usr/include"
                    "/usr/include/c++/5.3.1"
                    "/usr/local/include/*"))
        ))

 (c-mode . ((c-basic-offset . 4)
      (tab-width . 4)
      (indent-tabs-mode . t)
      (compile-command . "cmake --build ./build --config Release --target INSTALL")
      (cc-search-directories . ("."
                    "./include"
                    "/home/sonictk/Qt5.8.0/5.8/gcc_64/include"
                    "/usr/include"
                    "/usr/include/c++/5.3.1"
                    "/usr/local/include/*"))
      ))
)
```

Get rid of the dumb warnings from some of the plugins by byte-compiling them.
Run the following command in the ``/modules`` folder:

``emacs -batch -f batch-byte-compile ./**/*.el``

Anyway, once everything is cloned and set up, you can run this side-by-side
with your current Emacs installation if you just want to check it out using
this method.


## Who?

Siew Yi Liang a.k.a. sonictk

## Whaaaaaa~?

I make no promises that this won't break your installation or your heart. If it
does... ¯\_༼ ಥ ‿ ಥ ༽_/¯
