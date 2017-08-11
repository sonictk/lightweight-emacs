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

### Dependencies

You will need the following dependencies, regardless of your platform:

* Emacs 24.5.1 (Newer/older versions are untested, though I'm hoping this works
  with 25 out of the box)
* [clang/llvm](https://clang.llvm.org/) (The ``libclang`` DLL/.so must be in
  your ``PATH``) Version should be at least > 3.7.1, I'm currently using 3.9.0
* grep
* gdb
* [CMake](https://cmake.org/)
* [GNU Global Tags](https://www.gnu.org/software/global/)
* Git
* [ag (A.K.A. The Silver Searcher)](https://github.com/ggreer/the_silver_searcher). 
  A Windows version is available [here](https://blog.kowalczyk.info/software/the-silver-searcher-for-windows.html).

### Installation

Clone this repository with all its submodules first.

``git clone --recursive <git-repo-url>``

or:

```
git clone <git-repo-url>
git submodule init
git submodule update --recursive
```

You will also need to compile and install ``irony-server``. Just launch Emacs
(assuming it works fine) and run ``irony-install-server``. You will need to add
the following flag to the compilation command in order for ``irony-server`` to
be linked with the correct RPATH (This is required for OSX/Linux):

``-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=1``

**You need to compile that and restart Emacs after in order for C/C++
completion to work.**

Anyway, once everything is cloned and set up, you can run this side-by-side
with your current Emacs installation if you just want to check it out using
this method.

What I do is launch Emacs using this command:

``emacs -q --load ~/Git/lightweight-emacs/lightweight_config.el``

Set that as an alias or desktop file or whatever you prefer.


### Setup

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
        (cd-compile-directory . "/home/sonictk/Git/project/build")
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
      (cd-compile-directory . "/home/sonictk/Git/project/build")
      (cc-search-directories . ("."
                    "./include"
                    "/home/sonictk/Qt5.8.0/5.8/gcc_64/include"
                    "/usr/include"
                    "/usr/include/c++/5.3.1"
                    "/usr/local/include/*"))
      ))
)
```

### Known issues

Get rid of the dumb warnings from some of the plugins by byte-compiling them.
Run the following command in the ``/modules`` folder:

``emacs -batch -f batch-byte-compile ./**/*.el``

### Most important non-standard keybinds

* ``C-,``/``C-.``: Finds the other header/implementation file in C/C++ modes in
  the same/other window.

* ``C-M->``/``C-M-<``: Finds the header file at point. Needs the
  ``.dir-locals.el`` file set with the header search paths.

* ``C-c h i``/``C-<f12>``: Shows all symbols defined in the current file for
  C/C++/Python modes.

* ``S-<arrow key>``: Switch cursor to buffer in direction of arrow key

* ``M-S-<arrow key>``: Move buffer in direction of arrow key (will swap buffer
  in other window if it exists)

* ``C-w/M-w`` will delete/copy lines if no region is selected

* ``C-enter``/``C-S-enter``/``S-enter`` all insert/prepend newlines like other
  text editors do.

* ``C-c <C-left/C-right>``: Navigates to last/next cursor
  position globally

* ``C-c <left/right>``/``C-c b/f``: Navigates to last/next cursor position
  locally

* ``S-<mouse3>``: Start marking rectangle regions

* ``S-<mouse2>``: Toggle hiding/showing of regions

* ``C-S-v``: yank (paste)

* ``C-z``/``C-S-z``: undo/redo

* ``C-x >/<``: Scroll left/right

* ``C-tab``: Autocomplete

* ``C-x m``/``C-x M-m``: Start Emacs shell/Normal shell

* ``C-M-S-w``: Toggle ``visual-line-mode`` (line wrapping)

* ``C-S-f``: Search using ``projectile-ag`` (The silver searcher). Way faster
  than ``grep``.

* ``C-S-b``: Run the ``cd-compile`` command to allow for building your project.

* ``C-S-s``: Toggle ``sr-speedbar``.

There's a ton more, obviously, which you can dig through the config file to
find. Those are the mose important ones, though.

### Most important new commands

* ``reload-dir-locals-for-current-buffer`` : Does what it says. No more
  restarting Emacs!

* ``kill-other-buffers``: Does what it says. No more hitting ``C-x k`` 50
  times!

### Most important non-standard notes:

* I disable flycheck by default because it's slow and you need a
  ``.dir-locals.el`` file to really make good use of it. Even then, it's still
  slow and stupid sometimes.

* However, I decided to leave it enabled for ``elpy-mode`` because for Python,
  you can configure the warnings with your ``~/.flake8`` configration file.
  Also Python inspection is far more reliable and less stupid in general using
  ``flake8``. I might disable this in the future when I decide that it's too
  slow as well and Pycharm is far better at this anyway.


## Who?

Siew Yi Liang a.k.a. sonictk. 

Tons of plugins are used in this configration; their licenses and original
source code have been preserved (except for some changes I made to
``back-button.el``, because it stomps default Emacs keybindings) Please kowtow
to them as appropriate if you end up using this configuration for their
kindness and benevolence.


## Whaaaaaa~?

Changes are being made constantly to this configuration, and I make no promises
that this won't break your installation or your heart. If it does...

¯\_༼ ಥ ‿ ಥ ༽_/¯

(Please report bugs, though I can't promise I'll fix them!)
