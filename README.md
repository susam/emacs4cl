Emacs For Common Lisp (Emacs4CL)
================================

This repository provides a tiny [`.emacs`] file to set up Emacs quickly
for Common Lisp programming. This document provides a detailed
description of how to set it up and get started with Common Lisp
programming.

[![View Source][Source SVG]][Source URL]
[![MIT License][License SVG]][L]
[![Twitter][Twitter SVG]][Twitter URL]

[Source SVG]: https://img.shields.io/badge/view-~%2f.emacs-brightgreen
[Source URL]: .emacs
[License SVG]: https://img.shields.io/badge/license-MIT-%233ea639
[Twitter SVG]: https://img.shields.io/badge/twitter-%40susam-%231da1f2
[Twitter URL]: https://twitter.com/susam

This repository provides a good middle ground between configuring Emacs
manually by installing SLIME, Paredit, etc. yourself with `M-x
package-install` commands and installing Portacle. It promotes a
do-it-yourself approach to automate customizing Emacs for Common Lisp
programming. Here is how the development environment is going to look
like:

[![Screenshot of Emacs][screenshot]][screenshot]

[screenshot]: https://i.imgur.com/OUFvQdh.png

If you are already comfortable with Emacs and only want to understand
what is in the [`.emacs`] file, you can skip ahead directly to the
[Line-by-Line Explanation](#line-by-line-explanation) section that
describes every line of this Emacs initialization file in detail.

[`.emacs`]: .emacs


Contents
--------

* [Who Is This For?](#who-is-this-for)
* [What About Portacle?](#what-about-portacle)
* [Get Started](#get-started)
* [Step-by-Step Usage](#step-by-step-usage)
  * [Use SBCL](#use-sbcl)
  * [Use Emacs](#use-emacs)
  * [Use SLIME](#use-slime)
  * [Use Paredit](#use-paredit)
  * [Use Rainbow Delimiters](#use-rainbow-delimiters)
* [Line-by-Line Explanation](#line-by-line-explanation)
  * [Tweak UI](#tweak-ui)
  * [Highlight Parentheses](#highlight-parentheses)
  * [Install Packages](#install-packages)
  * [Inferior Lisp Program](#inferior-lisp-program)
  * [Add Hooks](#add-hooks)
* [Opinion References](#opinion-references)
* [Forums](#forums)
* [License](#license)
* [Support](#support)


Who Is This For?
----------------

Are you an absolute beginner to Emacs? Are you so new to Emacs that you
don't even have `~/.emacs` or `~/.emacs.d` on your file system? Have you
considered learning Common Lisp but when you picked up a book like
[Practical Common Lisp][pcl], you learnt that it recommends Emacs and
SLIME for development environment and it seemed like a significant
additional learning curve for you? If you answered "yes" to most of
these questions, then this project is for you.

The [`.emacs`] file in this project provides you a quick way to get
started with setting up your development environment. This document
explains how to do so in a step-by-step manner. This document also
explains the content of [`.emacs`] file in a line-by-line manner.

[pcl]: http://www.gigamonkeys.com/book/
[clc]: https://lispcookbook.github.io/cl-cookbook/


What About Portacle?
--------------------

Portacle is fine. It is an often recommended Common Lisp development
environment for beginners. It provides an Emacs-based fully featured
integrated development environment for Common Lisp. Portacle consists of
Emacs customized heavily with custom configuration and a rich selection
of packages and tools that make it suitable for Common Lisp programming.
However, some of the customizations done by Portacle look odd to an
experienced Emacs user:

  - A default `*scratch*` buffer with more than 10 lines of comments
    that consume some valuable screen real-estate.

  - Enabling line numbers in all buffers including the SLIME
    Read-Eval-Print Loop (REPL) buffer. Enabling line numbers in Emacs
    is considered to be an antipattern by some experienced Emacs users.
    See the [Opinion References](#opinion-references) section for more
    details on this.

More importantly, installing Portacle hides away the underlying details
of what goes into making Emacs ready for Common Lisp development.
Programmers who like to set up their tools from scratch may find that
Portacle does not offer the opportunity to do so because it bundles all
useful tools already.

Also, Portacle is still Emacs. To use Portacle effectively, you have to
learn Emacs and SLIME anyway. If you are going to learn Emacs and SLIME
anyway, you might as well set it up yourself. Then you can add only
those customizations to Emacs that you need instead of Portacle deciding
what your Emacs experience should be like.

This project provides a good middle ground between setting up Emacs from
scratch manually and installing Portacle. It promotes a do-it-yourself
approach to setting up Emacs for Common Lisp programming. More
importantly, it helps you understand each step of the work that goes
into customizing Emacs as an environment for Common Lisp programming.


Get Started
-----------

This section helps you to set up Emacs for Common Lisp development
quickly and see what the end result looks like. Perform the following
steps to get started:

 1. Install SBCL and Emacs.

    On macOS, enter the following command if you have
    [Homebrew](https://brew.sh):

    ```sh
    brew install sbcl
    brew cask install emacs
    ```

    On Debian, Ubuntu, or another Debian-based Linux system, enter the
    following command:

    ```
    sudo apt-get install sbcl emacs
    ```

    For other environments, download SBCL and Emacs from
    http://www.sbcl.org/platform-table.html and
    https://www.gnu.org/software/emacs/ respectively.

 2. Copy the Emacs initialization file [`.emacs`] provided here to your
    home directory. Here is an example `curl` command to do so:

    ```sh
    curl https://raw.githubusercontent.com/susam/emacs4cl/master/.emacs >> ~/.emacs
    ```

 3. Start Emacs:

    ```sh
    emacs
    ```

    On macOS, you may receive the following error message in a dialog
    box: '“Emacs.app” can’t be opened because Apple cannot check it for
    malicious software.' To resolve this issue, go to Apple menu >
    System Preferences > Security & Privacy > General and click 'Open
    Anyway'.

    It may take a minute or so for Emacs to start the very first time.
    When it starts the first time with the new Emacs initialization
    file obtained in the previous step, it installs the packages
    specified in it. This is only a one-time activity. The next time you
    start Emacs, it will start instantly. We will see how [`.emacs`]
    takes care of it in the line-by-line guide later.

 4. Within Emacs, start SLIME by pressing the following key-sequence:

    ```
    M-x slime RET
    ```

    In the Emacs world (and elsewhere too), the prefix `M-` denotes the
    meta modifier key. It does not exist on most modern keyboards. Use
    the <kbd>alt</kbd> key or the <kbd>option</kbd> key as a modifier
    key or <kbd>esc</kbd> as a prefix key to enter `M-`.

    For example `M-x` is going to be <kbd>alt</kbd> + <kbd>x</kbd> or
    <kbd>option</kbd> + <kbd>x</kbd> or <kbd>esc</kbd> <kbd>x</kbd> on a
    modern keyboard.

    Similarly, `RET` denotes the <kbd>enter</kbd> key or the
    <kbd>return</kbd> key.

 5. After SLIME REPL starts, enter the following expression at the
    `CL-USER>` prompt and press <kbd>enter</kbd>.

    ```lisp
    (format t "hello, world~%")
    ```

    If the output `"hello, world"` appears in SLIME REPL, the
    development environment setup is complete.

 6. Optionally, install Quicklisp with the following commands:

    ```sh
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
    sbcl --load ~/quicklisp/setup.lisp --eval '(ql:add-to-init-file)' --quit
    ```

    Note that this is an optional step if you are just about to begin
    learning Common Lisp. However, as you start using Common Lisp for
    serious software development, sooner or later you are going to need
    Quicklisp in order to install Common Lisp libraries available on
    Quicklisp.

Now that your environment is setup, read the next section to learn how
to use this environment in more detail.


Step-by-Step Usage
------------------

### Use SBCL

Steel Bank Common Lisp (SBCL) is a high performance Common Lisp
compiler. It runs on several Unix and Unix-like systems such as Linux,
FreeBSD, macOS, etc. It also runs experimentally on Windows. It is the
most popular free and open source implementation of Common Lisp as of
December 2020. See the [Opinion References](#opinion-references) section
for survey results related to this.

The steps provided below show how to run SBCL independently. This is not
a typical way to run SBCL because most of the time we interact with SBCL
via SLIME right from within Emacs. However running it independently once
helps one appreciate that it is an independent program that compiles and
executes Common Lisp code. Here are the steps:

 1. Open your favourite editor, type this code, and save it as
    `hello.lisp`:

    ```lisp
    (format t "hello, world~%")
    ```

 2. Then enter this command in the shell to run the program:

    ```sh
    sbcl --script hello.lisp
    ```

 3. Now start the SBCL Read-Eval-Print Loop (REPL) with the following
    command in the shell:

    ```sh
    sbcl
    ```

 4. An asterisk prompt appears. Enter a Common Lisp expression at the
    asterisk prompt like this and press <kbd>enter</kbd>:

    ```lisp
    (+ 1 2)
    ```

    The result should appear as the output.

 5. Similarly, enter the following expression at the SBCL prompt and
    press <kbd>enter</kbd>:

    ```lisp
    (format t "hello, world~%")
    ```

 6. Finally, enter the following expression and press <kbd>enter</kbd>
    to exit the SBCL REPL:

    ```lisp
    (exit)
    ```


### Use Emacs

Emacs is a very powerful and extensible editor. It comes with over
10,000 built-in commands. A small section like this can barely scratch
the surface of Emacs. Yet, this section makes a modest attempt at
getting you started with Emacs and then provides more resources to learn
further. Perform the following steps to get started:

 1. Start Emacs:

    ```sh
    emacs
    ```

 2. Within Emacs, enter the following command to open a file, say,
    `~/hello.txt`:

    ```
    C-x C-f hello.txt RET
    ```

    A new buffer to edit `hello.txt` is created. If a file with that
    name already exists on your file system, then it loads the content
    of the file into the buffer.

    Note that in the Emacs world (and elsewhere too), the
    notation `C-` denotes the <kbd>ctrl</kbd> modifier key. Thus `C-x`
    denotes <kbd>ctrl</kbd> + <kbd>x</kbd>.

    The notation `RET` denotes the <kbd>enter</kbd> or <kbd>return</kbd>
    key.

    Consecutive `C-` key sequences can be optimized by pressing and
    holding the <kbd>ctrl</kbd> key once at the beginning, pressing the
    other keys after that, and then releasing the <kbd>ctrl</kbd> key in
    the end. For example, to type `C-x C-f`, first press and hold
    <kbd>ctrl</kbd>, then press <kbd>x</kbd>, then press <kbd>f</kbd>,
    and then release <kbd>ctrl</kbd>. In other words, think of `C-x C-f`
    as `C-(x f)`. This shortcut works for other modifier keys too.

 3. Now type some text into the buffer. Type out at least 3-4 words. We
    will need it for the next two steps.

 4. Move backward by one word with the following command:

    ```
    M-b
    ```

    Remember from the previous section that `M-` denotes the meta
    modifier key. The above command can be typed with
    <kbd>alt</kbd> + <kbd>b</kbd> or <kbd>option</kbd> + <kbd>b</kbd> or
    <kbd>esc</kbd> <kbd>b</kbd>.

    If you face any issue with the <kbd>alt</kbd> key or the
    <kbd>option</kbd> key, read [Emacs Wiki: Meta Key
    Problems](https://www.emacswiki.org/emacs/MetaKeyProblems).

 5. Now move forward by one word with the following:

    ```
    M-f
    ```

 5. The `C-g` key sequence cancels the current command. This can be used
    when you mistype a command and want to start over or if you type a
    command partially, then change your mind and then you want to cancel
    the partially typed command. Try out these examples:

    ```
    C-x C-f C-g
    ```

    ```
    C-x C-g
    ```

 7. Save the buffer to a file on the file system with this command:

    ```
    C-x C-s
    ```

 8. Quit Emacs:

    ```
    C-x C-c
    ```

Now you know how to start Emacs, open a file, save it, and quit. Improve
your Emacs knowledge further by taking the Emacs tutorial that comes
along with Emacs. In Emacs, enter the key-sequece `C-h t` to start the
tutorial.

The key-bindings to perform various operations like creating file,
saving file, quitting the editor, etc. may look arcane at first, but
repeated usage of the key-bindings develops muscle memory soon and after
having used them for a few days, one does not even have to think about
them. The fingers do what the mind wants effortlessly due to muscle
memory.

While you are getting used to the Emacs key-bindings, keep this [GNU
Emacs Reference Card][emacs-ref] handy. Also, if you are using it in GUI
mode, then the menu options can be quite helpful. The menu options
contain frequently used operations. The option for each operation also
displays the key-bindings that can be used to invoke the same operation.
Also, bookmark the [GNU Emacs Manual][emacs-doc] and refer to it when
you need it.

[emacs-doc]: https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html
[emacs-ref]: https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf


### Use SLIME

Superior Lisp Interaction Mode for Emacs (SLIME) is a very popular Emacs
mode that adds support for interacting with a running Common Lisp
process for compilation, debugging, document lookup, etc. while
developing Common Lisp applications. Perform the following steps to get
started with it:

 1. Start Emacs:

    ```sh
    emacs
    ```

 2. Within Emacs, start SLIME by pressing the following key-sequence:

    ```
    M-x slime RET
    ```

    Remember that `M-x` translates to <kbd>alt</kbd> + <kbd>x</kbd> or
    <kbd>esc</kbd> <kbd>x</kbd> on a modern keyboard.

 3. A new buffer named `*slime-repl sbcl*` should have now appeared with
    the following prompt:

    ```
    CL-USER>
    ```

    This is a Read-Eval-Print-Loop (REPL) where you can evaluate Common
    Lisp expressions.

 3. Enter the following expression in the REPL:

    ```lisp
    (+ 1 2)
    ```

    The following result should appear when you press <kbd>enter</kbd>:

    ```
    3
    ```

 4. We will now see how to work on a Lisp source file and send
    expressions to the REPL for evaluation using SLIME commands without
    having to leave Emacs. First, create a buffer for a new file, for
    example:

    ```
    C-x C-f foo.lisp
    ```

 5. Now enter this Lisp code into the buffer for `foo.lisp`:

    ```lisp
    (+ 1 2)
    ```

 6. While the cursor is placed after the closing parenthesis (not on it,
    but after it), enter the following command:

    ```
    C-x C-e
    ```

    The result `3` should appear in a minibuffer at the bottom.

There is a lot more to SLIME than what is described above. To learn more
about SLIME, see [Slime User Manual][slime-doc]. Also, keep this [Slime
Quick Reference Card][slime-ref] handy.

[slime-doc]: https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html
[slime-ref]: http://www.chiark.greenend.org.uk/doc/slime/slime-refcard.pdf


### Use Paredit

Paredit helps in keeping parentheses balanced and also in performing
structured editing of S-expressions in Lisp code. It provides a powerful
set of commands to manipulate S-expressions in various ways. Perform the
following steps to get started with Paredit:

 1. Run Emacs:

    ```sh
    emacs
    ```

 2. Open a Common Lisp source file:

    ```
    C-x C-f foo.lisp
    ```

 3. Type the following code only:

    ```lisp
    (defun square (x
    ```

    At this point, Paredit should have inserted the two closing
    parentheses automatically. The code should look like this:

    ```lisp
    (defun square (x))
                    -
    ```

    The cursor should be situated just after the parameter `x`. The
    underbar shows where the cursor should be.

 4. Type the closing parentheses now. Yes, type it even if the closing
    parenthesis is already present. The cursor should now skip over the
    first closing parenthesis like this:

    ```lisp
    (defun square (x))
                     -
    ```

    Of course, there was no need to type the closing parenthesis because
    it was already present but typing it out to skip over it is more
    efficient than then moving over it with movement commands. This is,
    in fact, a very nifty feature of Paredit. We can enter code with the
    same keystrokes as we would without Paredit.

 5. Now press <code>enter</code> to create a new line just before the
    last parenthesis. A newline is inserted like this:

    ```lisp
    (defun square (x)
      )
      -
    ```

 6. Now type only this:

    ```lisp
    (* x x
    ```

    Again, Paredit would have inserted the closing parenthesis
    automatically. The code should look like this now:

    ```lisp
    (defun square (x)
      (* x x))
            -
    ```

There is a lot more to Paredit than this. To learn more, see [The
Animated Guide to Paredit][paredit-ref].

Note: While many Lisp programmers find Paredit very convenient and
powerful while manipulating S-expressions in Lisp code, there are a few
people who do not like Paredit because they find the Paredit behaviour
intrusive. See the [Opinion References](#opinion-references) section for
more discussion on this topic.

[paredit-ref]: http://danmidwood.com/content/2014/11/21/animated-paredit.html


### Use Rainbow Delimiters

There is not much to learn about using Rainbow Delimiters. In the
previous sections, you must have seen that as you type nested
parentheses, each parenthesis is highlighted with a different color.
That is done by Rainbow Delimiters. It colors each parenthesis according
to its nesting depth level.

Note: Not everyone likes Rainbow Delimiters. Some people find
parentheses in multiple colors distracting. See the [Opinion
References](#opinion-references) section for more discussion on this
topic.


Line-by-Line Explanation
------------------------

This section explains the [`.emacs`] file provided here line-by-line.


### Tweak UI

The first few lines in our [`.emacs`] merely tweak the Emacs user
interface. These are of course not essential for Common Lisp
programming. However, many new Emacs users often ask how to customize
the user interface to add a good color scheme and make it look minimal,
so this section indulges a little in customizing the user interface. The
actual Common Lisp related customization begins in the next section:
[Highlight Parentheses](#highlight-parentheses).

Here is a line-by-line explanation of the UI tweaks in [`.emacs`]:

  - Hide the menu bar:

    ```elisp
    (menu-bar-mode 0)
    ```

    When Emacs runs in a GUI window, by default, starts with a menu bar,
    tool bar, and scroll bar. Experienced users use Emacs completely
    through the keyboard via the various key-bindings for various
    operations, so many of them hide these additional bars to make the
    Emacs window look clean and minimal. 

    If you are a beginner to Emacs, you might find the menu bar helpful
    initially, so you might not want this line in your Emacs
    initialization file. In that case, remove this line or just comment
    it out by inserting a semicolon (i.e., `;`) before the opening
    parentheses.

  - Hide the tool bar and scroll bar:

    ```elisp
    (when (display-graphic-p)
      (tool-bar-mode 0)
      (scroll-bar-mode 0))
    ```

    The `when` expression checks if Emacs is running with graphic
    display before disabling the tool bar and scroll bar. Without the
    `when` expression, we get the following error on Emacs without
    graphic display support: `Symbol's function definition is void:
    tool-bar-mode`. An example of Emacs without graphics support is
    `emacs-nox` on Debian 10.

    Note that this is only an author's preference. You may comment out
    one or more of these lines if you want to retain the tool bar or
    scroll bar.

  - Inhibit the startup screen with the `Welcome to GNU Emacs` message
    from appearing:

    ```elisp
    (setq inhibit-startup-screen t)
    ```

    If you are a beginner to Emacs, you might find the startup screen
    helpful. It contains links to tutorial, manuals, common tasks, etc.
    If you want to retain the startup screen, comment this line out.

  - Load a beautiful dark color theme known as `wombat`:

    ```elisp
    (load-theme 'wombat)
    ```

    If you want to check the other built-in themes, enter
    `M-x customize-themes RET`. A new window with a buffer named
    `*Custom Themes*` appear. In this buffer, select any theme you want
    to test. After you are done testing, you can close this new window
    with `C-x 0`.


### Highlight Parentheses

This following points describe how we enable highlighting of
parentheses:

  - The next point shows how to enable highlighting of matching pair of
    parentheses. By default, there is a small delay between the movement
    of a cursor and the highlighting of the matching pair of
    parentheses. The following line of code gets rid of this delay:

    ```elisp
    (setq show-paren-delay 0)
    ```

    This line of code must come before the one in the next point for it
    to be effective.

  - Highlight matching parentheses:

    ```elisp
    (show-paren-mode)
    ```

    A pair of parenthesis is highlighted when the cursor is on the
    opening parenthesis of the pair or just after the closing
    parenthesis of the pair.


### Install Packages

This section is essential to Common Lisp programming with Emacs. The
following points describe how we automate the installation of Emacs
packages we need:

  - The following code disables TLS 1.3 to work around a known bug in
    GNU Emacs versions 26.1 and 26.2:

    ```elisp
    (when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
      (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
    ```

    See https://debbugs.gnu.org/34341 for more details on the bug. This
    code is not required while using GNU Emacs version 26.3 or 27.1 or a
    later version although leaving this code intact should do no harm
    because this code checks whether the Emacs version is less than 26.3
    before applying the workaround.

  - This is necessary for defining the `package-archives` list we will
    use in the next point.

    ```elisp
    (package-initialize)
    ```

  - Add Milkypostman’s Emacs Lisp Package Archive (MELPA) to the list of
    archives to fetch packages from:

    ```elisp
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    ```

    By default only Emacs Lisp Package Archive (ELPA) is in the list of
    archives to fetch from. The above line adds MELPA too to the list.
    If you are curious to see what the original value of
    `package-archives` was and what it is now due to the above line,
    enter `C-h o package-archives RET`.

  - Download package descriptions from package archives only if they
    have not been downloaded before:

    ```elisp
    (unless package-archive-contents
      (package-refresh-contents))
    ```

    The first line checks whether package descriptions from package
    archives from archives have been fetched. See the
    `~/.emacs.d/elpa/archives` directory for archive contents in case
    you are curious. If the archive contents have not been fetched then
    the second line fetches them. Thus the second line executes only
    when the Emacs initialization is loaded for the first time. Thus,
    the first time Emacs starts with the [.emacs](.emacs) file of this
    repository, it takes a while to fetch the package archives. However, 
    once the packages archived have been fetched and Emacs is started
    again later, it starts instantly because the code above takes care
    not to fetch package archives again when it is already cached
    locally.

  - When we install packages using `package-install` (coming up in the
    next point), a few customizations are written automatically into the
    Emacs initialization file (`~/.emacs` in our case). This has the
    rather undesirable effect of our carefully handcrafted `~/.emacs`
    being meddled by `package-install`. To be precise, it is the
    `custom` package invoked by `package-install` that intrudes into our
    Emacs initialization file. To prevent that, we ask `custom` to write
    the customizations to a separate file at `~/.emacs.d/custom.el` with
    the following code:

    ```elisp
    (setq custom-file (concat user-emacs-directory "custom.el"))
    ```

  - Install SLIME, Paredit, and Rainbow Delimiters only if they are not
    installed already:

    ```elisp
    (dolist (package '(slime paredit rainbow-delimiters))
      (unless (package-installed-p package)
        (package-install package)))
    ```

    This loops iterates over each package name in a list of packages.
    For each package, it checks whether the package is installed with
    the `package-installed-p` function. If it is not installed, then it
    is installed with the `package-install` function. You can modify the
    list of packages in the first line to add other packages that you
    might need in future or remove packages that you do not want.

    The first time Emacs starts with this initialization file, it takes
    a while to install the packages we need. However, once the packages
    are installed and Emacs is started again later, it starts instantly
    because the code above takes care to not attempt installing packages
    that are already installed.


### Inferior Lisp Program

The following steps describe setting up the `inferior-lisp-program`
variable so that Emacs can use SBCL to load and execute SLIME:

  - Emacs uses a variable named `inferior-lisp-program` to start SLIME.
    The use of this variable can be seen in the next point. This
    variable specifies the program to be invoked for loading and
    executing SLIME. We will set this to just `sbcl`, i.e., just the
    program name without the complete path. Avoiding absolute path of
    SBCL in Emacs initialization file has the advantage that the same
    initialization file can work well on other systems too where the
    location of SBCL may be different.

    On macOS, when we install SBCL using `brew install sbcl`, the
    compiler binary executable is written to `/usr/local/bin/sbcl`. The
    path `/usr/local/bin` is generally available in the shell's `$PATH`
    environment variable, so when Emacs is launched from the shell, its
    `exec-path` variable contains `/usr/local/bin`. As a result, it can
    find `sbcl` and start SLIME successfully.

    However, when Emacs is launched from the desktop (say, from macOS
    Dock), it does not have `/usr/local/bin` in its `exec-path`, so it
    fails to start SLIME with this error: `Searching for program: No
    such file or directory, sbcl`. The following line of code works
    around this issue by adding `/usr/local/bin` to the `exec-path`
    variable:

    ```elisp
    (add-to-list 'exec-path "/usr/local/bin")
    ```

    Now there are several other ways to resolve this issue. A popular
    way is to specify the absolute path of SBCL as the
    `inferior-lisp-program`. Yet another way is to configure macOS
    desktop such that when a program is launched from GUI, it contains
    `/usr/local/bin` in its `PATH`.

    The workaround shown above is recommended in this document for
    two reasons. Firstly, we don't want to hard-code absolute path of
    SBCL in the Emacs initialization file, so that the same
    initilization file can work well on other systems where the location
    of SBCL may be different. Secondly, we want to keep the workaround
    minimally invasive, so that we don't have to go around meddling with
    the desktop settings only for the sake of Emacs.

  - Specify the program to be invoked for loading and executing SLIME:

    ```elisp
    (setq inferior-lisp-program "sbcl")
    ```


### Add Hooks

This section describes how to enable Paredit and Rainbow Delimiters.
These are not absolutely essential for having an interactive programming
environment for Common Lisp. However many programmers find them useful
while some do not find them useful.

In case you decide not to use either Paredit or Rainbow, then you may
skip this section. In that case, you might also want to remove these
packages from the `dolist` expression of [`.emacs`].

  - Enable Paredit while editing Emacs Lisp code:

    ```elisp
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    ```

    Paredit helps in keeping parentheses balanced and in performing
    structured editing of S-expressions. While we configure it to be
    used for Common Lisp programming, we might as well configure it for
    editing Emacs Lisp code too. Then the experience of editing Emacs
    Lisp code and that of editing Common Lisp code will be consistent
    with each other.

    To test that Paredit is enabled for editing Emacs Lisp code, open a
    new Emacs Lisp file, say, `foo.el`. Then type `(`. Paredit should
    automatically insert the corresponding `)`.

  - Enable Paredit in eval-expression minibuffer:

    ```elisp
    (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
    ```

    To test this, enter `M-:` to bring up the eval-expression minbuffer
    and type `(`. Paredit should automatically insert the corresponding
    `)`.

  - Enable Paredit while interactively evaluating Emacs Lisp expressions
    in inferior-emacs-lisp-mode (IELM):

    ```elisp
    (add-hook 'ielm-mode-hook 'enable-paredit-mode)
    ```

    To test this, enter `M-x ielm RET`. When the `*ielm*` buffer
    appears, type `(`. Paredit should automatically insert the
    corresponding `)`.

  - Enable Paredit while editing Common Lisp code:

    ```elisp
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
    ```

    To test this, open a new Common Lisp source file, say, `C-x C-f
    foo.lisp RET`. Then type `(`. Paredit should automatically insert
    the corresponding `)`.

  - Enable Paredit in Lisp interaction mode:

    ```elisp
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    ```

    To test this, first open a non-Lisp file, say, `C-x C-f foo.txt
    RET`. Now type `(`. Note that no corresponding `)` is inserted
    because we are not in Lisp interaction mode yet. Delete `(`. Then
    start Lisp interaction mode with the command `M-x
    lisp-interaction-mode RET`. Type `(` again. Paredit should now
    automatically insert the corresponding `)`.

  - Enable Paredit in SLIME REPL:

    ```elisp
    (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
    ```

    To test this, start SLIME with `M-x slime RET`. Then type `(` in
    SLIME REPL. Paredit should automatically insert the corresponding
    `)`.

  - When we press the <kbd>backspace</kbd> key or <kbd>delete</kbd> key
    to delete a parenthesis in the SLIME REPL, Paredit fails to keep the
    parentheses balanced because SLIME interferes with Paredit by
    grabbing the delete key. To fix this issue, use the following code:

    ```elisp
    (defun override-slime-del-key ()
      (define-key slime-repl-mode-map
        (read-kbd-macro paredit-backward-delete-key) nil))
    (add-hook 'slime-repl-mode-hook 'override-slime-del-key)
    ```

    To test this, start SLIME with `M-x slime RET`. Then type `(+ 1 (+
    2 (+ 3 4)))`. Even though, the closing parentheses `)))` will be
    automatically inserted, type them out to advance the cursor to the
    end of the line. When you type `)` even if it is already present,
    Paredit just skips over the already present `)`. Once you are at
    the end of the line, press <kbd>backspace</kbd> or <kbd>delete</kbd>
    multiple times. Paredit will keep the parentheses balanced at all
    times.

  - Enable Rainbow Delimiters while editing Emacs Lisp code:

    ```elisp
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    ```

    Rainbow Delimiters color nested parentheses with different colors
    according to the depth level of each parenthesis.

    To test this open a new Emacs Lisp file, say, `foo.el`. Then type
    `((((`. Rainbow Delimiters should color each parenthesis
    differently.

  - Enable Rainbow Delimiters while interactively evaluating Emacs Lisp expressions
    in inferior-emacs-lisp-mode (IELM):

    ```elisp
    (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
    ```

    To test this, enter `M-x ielm RET`. When the `*ielm*` buffer comes
    up, type `((((`. Rainbow Delimiters should color each parenthesis
    differently.

  - Enable Rainbow Delimiters while editing Common Lisp code:

    ```elisp
    (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
    ```

    To test this, open a new Common Lisp source file, say, `foo.lisp`.
    Then type `((((`. Rainbow Delimiters should color each parenthsis
    differently.

  - Enable Rainbow Delimiters in Lisp interaction mode:

    ```elisp
    (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
    ```

    To test this, first open a non-Lisp file, say, `foo.txt`. Now type
    `((((`. Then start Lisp interaction mode with the command `M-x
    lisp-interaction-mode RET`. Rainbow Delimiters should now color each
    parenthesis differently.

  - Enable Rainbow Delimiters in SLIME REPL:

    ```elisp
    (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
    ```

    To test this, start SLIME with `M-x slime RET`. Then type `(` in
    SLIME REPL. Rainbow Delimiters should automatically insert the corresponding
    `)`.

You may have noticed that we did not enable Rainbow Delimiters for
eval-expression. That is because it does not work as expected as of
Dec 2020. See https://github.com/Fanael/rainbow-delimiters/issues/57 for
more details.


Opinion References
------------------

- [State of Common Lisp Survey 2020][cl-survey-2020]
- [Displaying line numbers is an Emacs anti-pattern][ln-anti-pattern]
- [Give paredit mode a chance][paredit-chance]
- [Never warmed up to paredit][paredit-never-warmed]
- [Coloring each paren differently only adds noise][rainbow-noise]

[cl-survey-2020]: https://docs.google.com/forms/d/e/1FAIpQLSfg7UJRKrkI3OjOHWL4xI-murE4LpQjIxsiAhFdPEmtyLX3kg/viewanalytics
[ln-anti-pattern]: https://lobste.rs/s/vgjknq/emacs_begin_learning_common_lisp#c_su9qz9
[paredit-chance]: https://stackoverflow.com/a/5243421/303363
[paredit-never-warmed]: https://lobste.rs/s/vgjknq/emacs_begin_learning_common_lisp#c_0y6zpd
[rainbow-noise]: https://lobste.rs/s/vgjknq/emacs_begin_learning_common_lisp#c_1n78vl


Forums
------

The following community forums are available for asking questions and
seeking help regarding this project:

- Reddit: [r/spxy](https://www.reddit.com/r/spxy/)
- Matrix: [#spxy:matrix.org](https://app.element.io/#/room/#spxy:matrix.org)
- Freenode: [#spxy](https://webchat.freenode.net/#spxy)

Although these community forums are focussed on mathematics and computer
science, many members of these forums are very experienced in both Emacs
and Common Lisp.

Also, please feel free to [connect with me on Twitter][Twitter URL] and
share your feedback, questions, and ideas with me.


License
-------

This is free and open source software. You can use, copy, modify,
merge, publish, distribute, sublicense, and/or sell copies of it,
under the terms of the MIT License. See [LICENSE.md][L] for details.

This software is provided "AS IS", WITHOUT WARRANTY OF ANY KIND,
express or implied. See [LICENSE.md][L] for details.

[L]: LICENSE.md


Support
-------

To report bugs, suggest improvements, or ask questions, please create a
new issue at <http://github.com/susam/emacs4cl/issues>.
