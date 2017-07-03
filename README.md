# Lightweight Emacs

## About

This is a "lightweight" emacs configuration focused on editing C/C++,
Python, and Markdown. It's not that lightweight after I've added all the
features that I want, but it's "lighter" than what I had, which was heavier
than the average blue whale.


## Why?

I decided to start working on a brand-new configuration after my
previous one started to become really unwieldy/slow, along with the
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

Goals of this configuration:

* Optimized for C/C++/Python work, in that order. Some extra stuff like shaders
  (HLSL/GLSL), Maya support (through **etom**) and Maxscript support.
* C/C++ completion and introspection through ``irony-mode`` and ``semantic``.
* Able to run completely side-by-side with your existing Emacs installation and
  not dump any of its configuration files anywhere outside of the repository.
* (As far as possible) Immutable configuration; not dependent on package
  managers or changing versions of modules.
* Not much else, really. It's still almost as slow as my original config; but
  now it's at least a lot easier for me to figure out what's going on within the
  config file and fix/add things.
* Not intended to be a Sublime Text/VS Code/Atom replacement. In my mind, this
  is intended to be my go-to IDE for real work. I still use my crazy Vim config
  for normal text editing.


## How?

You will need the following dependencies, regardless of your platform:

* Emacs 24.5.1 (Newer/older versions are untested, though I'm hoping this works
  with 25 out of the box)
* [clang/llvm](https://clang.llvm.org/)
* grep
* gdb
* [CMake](https://cmake.org/)
* [GNU Global Tags](https://www.gnu.org/software/global/)
* Git

After that, just clone this repository with all its submodules and (hopefully)
things should work.

``git clone --recursive <git-repo-url>``

What I do is launch Emacs using this command:

``emacs -q --load ~/Git/lightweight-emacs/lightweight_config.el``

``irony-mode`` is used for completion. You will need to create a
``.clang_complete`` file that contains compilation options for your project
(determined via whether your project folder is a Git repository or not)

You will also need to compile and install ``irony-server``. Just launch Emacs
(assuming it works fine) and run ``irony-install-server``. **You need to
compile that and restart Emacs after in order for C/C++ completion to work.**

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

Siew Yi Liang a.k.a. sonictk. Tons of plugins are used in this configration;
their licenses and original source code have been preserved (except for some
changes I made to ``back-button.el``, because it stomps default Emacs
keybindings) Please kowtow to them as appropriate if you end up using this
configuration for their kindness and benevolence.


## Whaaaaaa~?

Changes are being made constantly to this configuration, and I make no promises
that this won't break your installation or your heart. If it does...

¯\_༼ ಥ ‿ ಥ ༽_/¯
