Install

brew install rtags

brew install ipspell

https://vxlabs.com/2016/04/11/step-by-step-guide-to-c-navigation-and-completion-with-emacs-and-the-clang-based-rtags/

brew install the_silver_searcher

brew install rg

brew install ispell

;;; Files ;;; ===== ;;; Rtags uses the following files: ;;; ~/.rtags' (created automatically) ;;;     Where rdm stores its index files. They are reloaded when it restarts. ;;; ~/.rdmrc' (optional) ;;; Config file for rdm (see rdm.cpp) containing default command line args. ;;; .rtags-config' (optional, located in project root dir) ;;;     Project configuration file. Not needed if there is a .git or .svn at ;;;     the project root. ;;; compile_commands.json' (optional, located in project root dir) ;;; Compilation database for a given project, containing for each file the ;;; clang command to build it. Not needed if you use RTags's compiler ;;; wrapper scripts. ;;; ;;; Running rdm in a shell ;;; ====================== ;;; Run rdm' in a shell or in the background. Use -L to specify a log file. ;;; Use --help for the list of options. You can stop it gracefully with: rc -q ;;; ;;; You can control rdm with the rc client (use --help to see all options): ;;; $ rc -w ;;;     List the loaded projects and show the active one. ;;; $ rc -w proj ;;;     Make "proj" the active project ("proj" is a regex). ;;; $ rc -J ;;;     Reload the compilation DB from the current directory. ;;; $ rc -W proj ;;;     Delete project. ;;; $ rc --find-project-root /path/to/sourcefile.cpp ;;;     Print what it determines to be the correct project root. ;;; $ rc -T sourcefile.cpp ;;;     Say wether this component is indexed or not. ;;; $ rc -q ;;;     Shutdown rdm. ;;; ;;; Running rdm in Emacs ;;; ==================== ;;; M-x rtags-start'. A buffer will be created with rdm logs; you can show ;;; it with "C-c r l". ;;; M-x rtags-stop' to kill it. ;;; ;;; Setting up a new project ;;; ======================== ;;; 1. If the project root dir does not contain a .git or .svn repo, create a ;;;    file .rtags-config' in the root dir with the specified content: ;;; project: /path/to/project ;;; ;;; 2. The next step is to create the compilation database ;;; `compile_commands.json'. For that, use CMake or use module ;;; init-rtags-cdb.el.


# Rust Setup
https://rust-analyzer.github.io/manual.html#installation

 rustup update

 cargo update

cargo xtask install --server


# Setup Hunspell
https://stackoverflow.com/questions/8931580/hunspell-cant-open-affix-or-dictionary-files-for-dictionary-named-en-us
