;;;-*- Mode: Lisp; Package: A-SIMPLE-DEFSYSTEM -*-
;;----------------------------------------------------------------------
;; 
;; File:        $Id: a-simple-defsystem.lisp,v 1.3 2002/06/14 20:26:55 firby Exp $
;; Created:     25 April 1996
;; Author:      Will Fitzgerald
;; 
;; Description: A simple defsystem
;; 
;; Changes:     
;;
;; waf 11/27/01   Version 0.4. 
;;                Added REQUIRE-SYSTEM and FIND-SYSTEM. DEFINE-SYSTEM defaults
;;                root directory to (this-files-pathname) if not given. *load-pathname*
;;                used as first default for (this-files-pathname). Changed copyright and
;;                version number. Default binary directory is now "bin", not "random-bin".
;;
;; jjw  7/11/96   operate-on-file methods for LOAD and COMPILE operations
;;		  check whether source file exists before comparing its
;;		  date to the binary file's.
;;
;; jjw  7/18/96   Made this-files-pathname work for ACL/unix according
;;		  to Alain's suggestion.  Now define 
;;		  *a-simple-defsystem-version* with defparameter.
;;
;; jjw  10/16/96  This-files-directory now calls translate-logical-pathname
;;		  on the source pathname before trying to extract the
;;		  directory.  The problem is that some .system files use
;;		  the directory without worrying about the logical host.
;;		  Therefore, this fix is a kludge and the right thing is to
;;		  either remove the this-files-directory function and let
;;		  users do their own manipulations of the pathname or add
;;		  this-files-logical-host as a new function.
;; 
;;----------------------------------------------------------------------

#|
Copyright (c) 2002, I/NET, Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met: 

- Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

- Neither the name of the I/NET, Inc. nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#




;; DEFINE-SYSTEM:
;; 
;; a function (not a macro!) for defining systems:
;; 
;; (ASD:DEFINE-SYSTEM name
;;   :HOME-DIRECTORY directory 
;;   :DIRECTORY directory
;;   :MODULES list-of-modules
;;   :USE-BIN-DIR t or nil (default: T)
;;   :PACKAGE package (default: *package*)
;;    )
;;
;; 
;; The combination of the HOME-DIRECTORY and the DIRECTORY
;; form the "root" of the system. Either or both of these
;; can be used. The function (THIS-FILES-DIRECTORY) can
;; be used to determine a system definition file's directory.
;; To be portable, the HOME-DIRECTORY and DIRECTORY should be
;; in list format (:ABSOLUTE .. ) or (:RELATIVE ...). If
;; both HOME-DIRECTORY and DIRECTORY are defined, then the
;; DIRECTORY should be RELATIVE to the HOME-DIRECTORY. The home
;; directory can be overridden in the LOAD-SYSTEM, COMPILE-SYSTEM,
;; etc. forms.
;;
;;A module is one of the following:
;;
;;  a file name with no extension. File names with extensions
;;    are not guaranteed to work.
;;    in which case the file is acted upon (ie, compiled, etc)
;;
;;  a file specification with the syntax
;;    (:FILE :NAME name :TYPE type :DIRECTORY dir)
;;    :TYPE and :DIRECTORY are optional. If :DIRECTORY is specified it
;;    should be a 'relative directory list,' that is, of the form
;;    '(:RELATIVE ...).  The file specification is merged with the root
;;    directory of the system.  The file is acted upon (ie, compiled, etc);
;; 
;;  a system name with the syntax:
;;    (:SYSTEM name)
;;    in which case the submodules are acted upon;
;; 
;;  a system location
;;    (:SYSTEM name location)
;;    in which case the file at location is loaded, and then the system is
;;    acted upon. The location has to have the same syntax as the file
;;    specification described above;
;;
;;  a conditional module list
;;    (name module+)
;;    in which name is the name of the conditional module list, and name is
;;    followed by any number of the allowed module specifications above.
;;
;;
;; USE-BIN-DIR means to use a separate binary directory created,
;; if necessary, at the same level as the system directory.
;; The name of the system is implementation dependent, and can
;; be retrieved with the function (BINARY-SUBDIRECTORY-NAME)
;; and can be set with (SET-BINARY-SUBDIRECTORY-NAME).
;;
;;The files, etc. are loaded or compiled into PACKAGE.
;; 
;; 
;; Examples:
;; 
;; perhaps in: CODE/LISP/UTILITIES/UTILITIES.SYSTEM
;;(ASD:DEFINE-SYSTEM "UTILITIES"
;;  :DIRECTORY (this-files-directory)
;;  :MODULES
;;  '("PIPES"
;;    "SHOW"
;;    "TEST")
;;  :PACKAGE "UTILITIES")
;;
;; perhaps in: CODE/LISP/MEMORY/MEMORY.SYSTEM
;;(ASD:DEFINE-SYSTEM "MEMORY"
;;  :DIRECTORY (this-files-directory)
;;  :MODULES
;;  '("CLASS-DEFINITIONS"
;;    "ACCESSORS"
;;    "MACROS")
;;  :PACKAGE "MEMORY")
;; 
;; perhaps in: CODE/LISP
;; 
;; (ASD:DEFINE-SYSTEM "TOP"
;;   :DIRECTORY (this-files-directory)
;;   :MODULES
;;   '((:SYSTEM "UTILITIES" '(:DIRECTORY "UTILITIES" :NAME "UTILITIES" :TYPE "SYSTEM"))
;;     (:SYSTEM "MEMORY" '(:DIRECTORY "MEMORY" :NAME "MEMORY" :TYPE "SYSTEM")))
;;   )
;;
;; 
;;(LOAD-SYSTEM name) ... loads system if it's not already loaded. 
;;  binary files are loaded if they are newer than source files,
;;  otherwise the source files are loaded.
;; 
;;(COMPILE-SYSTEM name)... compiles system
;;
;;Optional arguments are:
;;
;;(LOAD-SYSTEM name (force nil) (load-source nil) (verbose t) (test nil) (home-directory nil))
;;  :force, if t, will load all files again
;;  :load-source, if t, will load source files instead of binary files
;;  :verbose, if t (default), will print messages along the way
;;  :test, if t, will just print messages, not actually do anything
;;  :home-directory, if specified, supercedes system's home directory
;;
;;(COMPILE-SYSTEM name (force nil) (recompile nil) (and-load? nil) (verbose t) (test nil) (home-directory nil))
;;  :force, if t, will recompile source files newer than source files
;;  :recompile, if t, will recompile every file
;;  :and-load?, if t, will load the file after compiling it
;;  :verbose, if t (default), will print messages along the way
;;  :test, if t, will just print messages, not actually do anything
;;  :home-directory, if specified, supercedes system's home directory
;; 
;; (EDIT-SYSTEM name) ... calls the system  editor on the files in system
;;
;; (COPY-SYSTEM name :output-directory output-directory-pathname)
;;                    ... copies the _source_ files to output directory.
;;
;; Optional arguments:
;; (COPY-SYSTEM name :output-directory output-directory-pathname (test nil) (verbose t) (home-directory nil))
;;  :verbose, if t (default), will print messages along the way
;;  :test, if t, will just print messages, not actually do anything
;;  :home-directory, if specified, supercedes system's home directory
;;
;;(DEFINED-SYSTEMS) returns a list of defined systems.
;;
;;(REMOVE-SYSTEM name) removes system 
;;
;;(SYSTEM-NAMED name) finds system with this name.
;;
;;(FIND-SYSTEM name) same as SYSTEM-NAMED
;;
;; (UNSET-SYSTEM-STATE state name) "unsets" a particular system state
;; to an unfinished state. For example, (UNSET-SYSTEM-STATE 'COMPILE name)
;; informs the system that the system can be recompiled; similarly with
;; (UNSET-SYSTEM-STATE 'LOAD name)
;;
;; The type of the typical source file is found in (SOURCE-FILE-TYPE),
;; and be set with (SET-SOURCE-FILE-TYPE). It defaults to "lisp".
;;
;; There are a number of implementation dependencies in this file; if  you
;; get an error of the type
;; "Add Code here to implement..."
;; this indicates  you've hit an implementation dependency.
;; Sorry.

(in-package "CL-USER")

(CL:defpackage "A-SIMPLE-DEFSYSTEM" 
  (:use "COMMON-LISP")
  (:nicknames "ASD")
  (:export
   "*A-SIMPLE-DEFSYSTEM-VERSION*"
   "*LOAD-ORPHAN-BINARY-SILENTLY*"
   "DEFINED-SYSTEMS"
   "DEFINE-SYSTEM"
   "REMOVE-SYSTEM"
   "SYSTEM-NAMED"
   "FIND-SYSTEM"
   "LOAD-SYSTEM"
   "COMPILE-SYSTEM"
   "EDIT-SYSTEM"
   "COPY-SYSTEM"
   "REQUIRE-SYSTEM"
   "SET-BINARY-SUBDIRECTORY-NAME"
   "BINARY-SUBDIRECTORY-NAME"
   "SET-SOURCE-FILE-TYPE"
   "SOURCE-FILE-TYPE"

   "UNSET-SYSTEM-STATE"
   "THIS-FILES-DIRECTORY"
   "THIS-FILES-PATHNAME"
   ))

(in-package "A-SIMPLE-DEFSYSTEM")

(defparameter *version* 0.4)


(defvar *systems* nil "All known systems.")

(defvar *binary-file-type* 
  (pathname-type (compile-file-pathname "test")))

(defparameter *BINARY-SUBDIRECTORY-NAME*
  (car (list #+genera                  "genera"   ;;; symbolics lisp machines
	     #+(and lucid mips lcl4.0) "4.0mbin"  ;;; Decstations (lucid)
	     #+(and lucid
		    mips (not lcl4.0)) "mbin"     ;;; Decstations (lucid)
	     #+(and lucid pa)          "hbin"     ;;; HP's Precision Architecture (lucid)
             #+(and lucid hp300)       "6bin"     ;;; HP300's (lucid)
	     #+(and lucid sparc)       "sparcbin" ;;; Sun 4s
	     #+(and lucid sun mc68000) "sun3bin"  ;;; Sun 3s
	     #+(and lucid sun)         "sunbin"   ;;; Other sun
	     #+next                    "nextbin"  ;;; NeXTs (Allegro)
	     #+(and kcl vax)           "kclvax"   ;;; KCL (Kyoto Common Lisp) on vax
	     #+(and kcl mips)          "kclmips"  ;;; KCL on mips
	     #+(and kcl ibmrt)         "kclrt"    ;;; KCL in IBM RT
	     #+(and :coral :ccl-1.3)   "Macl1.3.2"  ;;; MACL (Macintosh Allegro CL)
             #+(and :ccl-4 :powerpc)  "MCL-PPC4" ;;; Macintosh Common Lisp, v. 4, PowerPC           
             #+(and :ccl-3 :powerpc)  "MCL-PPC3" ;;; Macintosh Common Lisp, v. 3, PowerPC           
             #+(and :ccl-3)           "MCL3"     ;;; Macintosh Common Lisp, v. 3
             #+(and :ccl-2)           "MCL2"     ;;; Macintosh Common Lisp, v. 2
             #+(and :LISPWORKS4.2 :win32) "Lworks-Win-x86"
	     #+(and :ALLEGRO-V6.1 mswindows) "ACL61-Win-x86" 
	     #+(and allegro mswindows)      "ACL-Win-x86"
	     #+(and allegro linux86)  "ACL-Linux-x86"
	     #+(and clisp (or win32 pc386 os/2 dos)) "CLISP-Win-x86"   ;;; GNU Common Lisp
	     #+(and clisp unix)                      "CLISP-Unix"      ;;; GNU Common Lisp
	     "bin"))
  "The subdirectory in which to place binary files.")

(defvar *source-file-type* "lisp")

(defvar *not-finished-text* "Not finished")

(defstruct (system  (:copier nil))
  name            ; string  name of system
  root            ; root pathname
  submodules      ; submodules (files or systems)
  state           ; state of current system (compiled, loaded, etc)
  options         ; options 
  package         ; package to load files into
  )

(defmethod print-object ((self system) stream)
  (with-slots (name state) self
    (print-unreadable-object (self stream :type t :identity t)
       (format stream "~a :state ~a" name state)
      )))

;;----------------------------------------------------------------------
;; functions for setting/querying binary subdirectory name
;;----------------------------------------------------------------------
(defun BINARY-SUBDIRECTORY-NAME ()
  *BINARY-SUBDIRECTORY-NAME*)

(defun set-BINARY-SUBDIRECTORY-NAME (name)
  (check-type name string)
  (setf *BINARY-SUBDIRECTORY-NAME* name))

(defun source-file-type ()
  *source-file-type*)

(defun set-source-file-type (name)
  (check-type name string)
  (setf *source-file-type* name))

;;----------------------------------------------------------------------
;; functions for creating/querying/changing system states
;;----------------------------------------------------------------------


(defun make-system-state (state)
  (cons state *not-finished-text*))

(defun set-system-state (system-state state-value)
  (setf (rest system-state) state-value)
  )

(defun add-system-state (state system)
  (setf (system-state system)
        (cons state
              (system-state system))))

(defun system-state-value (state system)
  (rest (assoc state (system-state system))))

(defun set-system-state-value (state state-value system)
  (let ((system-state (assoc state (system-state system))))
    (if system-state
      (set-system-state system-state state-value)
      (let ((state (make-system-state state)))
        (set-system-state state state-value)
        (add-system-state state system)
        ))
    state-value))

(defun unset-system-state (state system)
  (when (not (system-p system)) (setf system (system-named system)))
  (set-system-state-value state *not-finished-text* system))

;;----------------------------------------------------------------------
;; functions on creating/querying systems
;;----------------------------------------------------------------------

(defun defined-systems () *systems*)

(defun add-system (new-system)
  "add a system structure to the list of systems"
  (let ((found-list (member (system-name new-system)
                            (defined-systems)
                            :test 'string-equal
                            :key 'system-name)))
    (if found-list
      (setf (first found-list) new-system)
      (setf *systems* (cons new-system *systems*)))))

(defun system-named (system-name)
  (let ((name-string (if (symbolp system-name)
		       (symbol-name system-name)
		       system-name)))
    (find name-string (defined-systems) 
	  :key 'system-name :test 'string-equal)))

(defun find-system (system-name)
  (system-named system-name))

(defun remove-system (system-or-name)
  (let ((found (find-system system-or-name)))
    (when found
	(setf *systems* (delete found *systems*)))
    (and found t)))

(defun define-system (name &key 
                           (directory nil directory-given-p)
                           home-directory
                           modules 
                           (use-bin-dir t)
                           (package *package*))
  (check-type name (or string symbol))
  (setf name (if (symbolp name) (symbol-name name) name))
  (when (not directory-given-p) (setq directory (asd:this-files-directory)))
  (add-system 
   (make-system
    :name (if (symbolp name) (symbol-name name) name)
    :root (make-system-root-pathname home-directory directory)
    :state ()
    :submodules modules
    :options (if use-bin-dir `(:use-bin-dir) nil)
    :package package))
  name)


;;----------------------------------------------------------------------
;; submodule specifications
;; (:SYSTEM name load-file)
;;----------------------------------------------------------------------

(defun system-spec? (l) (and (listp l) (eq (first l) :system)))
(defun system-name-of (system-spec) (second system-spec))
(defun system-pathname-specs-of (system-spec) (third system-spec))

;;----------------------------------------------------------------------
;; file specifications
;;----------------------------------------------------------------------

(defun file-spec? (l) (or (stringp l) 
                          (and (listp l) 
                               (eq (first l) :file))))

;;----------------------------------------------------------------------
;; Operating on systems.
;;----------------------------------------------------------------------

(defun operate-on-system (name operation force? result-flag test verbose home-directory keys)
  (let ((sys (system-named name)))
    (unless sys
      (error "Couldn't find a system named ~A" name))
    (if (and (equal (system-state-value operation sys) result-flag)  (null force?))
      (when verbose (format t "~%;; ~S is already ~A." name (system-state-value operation sys)))
      (let ((dir (if home-directory (make-system-root-pathname home-directory (system-root sys))
                                     (system-root sys)))
	    (modules (system-submodules sys))
            (package (system-package sys)))
        (when verbose (format t "~%;; Performing ~S on ~A." operation name ))
        (set-system-state-value operation *not-finished-text* sys)
	(process-modules sys dir package modules operation
			 force? result-flag test verbose keys)
        (when verbose (format t "~%;; Finished ~a on ~a." operation name))
        (unless test (set-system-state-value operation result-flag sys))))))


(defun process-modules (sys dir package modules operation force? result-flag test verbose keys)
  (load-undefined-submodules-first dir modules verbose)
  (dolist (module modules)
    (cond
     ((file-spec? module)
      (operate-on-file operation 
                       (source-pathname module dir)
                       (binary-pathname module dir (system-options sys))
                       package
                       test
                       verbose
                       keys))
     ((system-spec? module)
      (operate-on-system (system-name-of module)
                         operation
                         force?
                         result-flag
                         test
                         verbose
                         dir
                         keys))
     ((listp module)
      (when (or (key-in? (car module) keys)
                (key-in? :ALL-GROUPS keys))
        (process-modules sys dir package (cdr module) operation force?
                         result-flag test verbose keys)))
     (T (error "In system ~A, ~s is not an allowed module type."
               (system-name-of sys) module)))))


(defun load-undefined-submodules-first (dir modules verbose)
  (dolist (module modules)
    (when
      (and (system-spec? module)
           (system-pathname-specs-of module)
           (not (system-named (system-name-of module))))
      (load (make-merged-pathname dir (system-pathname-specs-of module))
            :verbose verbose))))
           

(defun key-in? (key keys)
  (find key keys :test #'eql))


;; CLISP doesn't define find-package correctly
(defun find-package-hack (name)
  (if (packagep name) name
      (find-package name)))

;;----------------------------------------------------------------------
;; operate on file methods
;;----------------------------------------------------------------------


(defvar *load-orphan-binary-silently* NIL)


(defmethod operate-on-file ((operation (eql 'LOAD))
                            source-pathname
                            binary-pathname
                            package
                            test
                            verbose
                            keys)
  (let (file-to-load)
    (if (or (null (probe-file binary-pathname))
            (key-in? :LOAD-SOURCE keys))
      (setf file-to-load source-pathname)
      (cond ((null (probe-file source-pathname))
             (unless *load-orphan-binary-silently*
	       (cerror "Load the binary ~s"
		       "The source file ~*~s does not exist, but the binary does."
		       binary-pathname source-pathname))
             (setq file-to-load binary-pathname))
            (T
             (setf file-to-load (which-file-newer source-pathname
                                                  binary-pathname)))))
    (if test
      (format t "~%;;TEST: loading ~S" file-to-load)
      (let ((*package* (find-package-hack package)))
        (load file-to-load :verbose verbose)))))

(defun load-system (name &key (force nil) (verbose t) (test nil) (load-source nil)
                           (home-directory nil) (group :ALL-GROUPS))
  (let ( (keys (append
                (if load-source '(:LOAD-SOURCE) '())
                (if (listp group) group (list group)))) )
    (operate-on-system name 'LOAD force "loaded" test verbose home-directory keys)
    (values)))
    
(defmethod operate-on-file ((operation (eql 'COMPILE))
                            source-pathname
                            binary-pathname
                            package
                            test
                            verbose
                            keys)
  (if (or (null (probe-file binary-pathname))
          (key-in? :RECOMPILE keys)
          (and (null (probe-file source-pathname))
               (error "Source file ~s does not exist." source-pathname))
          (eq source-pathname (which-file-newer source-pathname binary-pathname)))
    (if test
      (format t "~%;;TEST: Compiling ~A as ~A" source-pathname binary-pathname)
      (progn
	(asd-ensure-directories-exist binary-pathname)
        (let ((*package* (find-package-hack package)))
          (common-lisp:compile-file source-pathname :output-file binary-pathname :verbose verbose))
        (when (key-in? :AND-LOAD keys)
          (let ((*package* (find-package-hack package)))
            (common-lisp:load binary-pathname :verbose verbose)))))))

(defun compile-system (name &key (force nil) (verbose t) (test nil) 
                               (and-load? nil) (recompile nil)
                               (home-directory)
                               (group :ALL-GROUPS))
  (let ((keys (append 
               (if recompile '(:RECOMPILE) '())
               (if and-load? '(:AND-LOAD) '())
               (if (listp group) group (list group)))))
    (operate-on-system name 'COMPILE (or force recompile) "compiled" test verbose home-directory keys))
  (values))

(defun load-first-system (l test verbose)
  (if (null l)
    NIL
    (let ((pathname (car l)))
      (if (probe-file pathname)
	(progn
	  (if test 
	    (format t "~%;;; TEST: Loading ~A" pathname)
	    (load pathname :verbose verbose))
	  T)
	(load-first-system (cdr l) test verbose)))))

(defun require-system (name &key (test NIL) (pathname-list NIL) (verbose t))
  (if (not (or (system-named name) 
	       (and (load-first-system pathname-list test verbose)
		    (system-named name))))
    (if test
      (progn (format t "~%;;; TEST: System ~S required, but it is not available" name) NIL)
      (error "System ~S required, but it is not available" name))
    (progn
      (when test (format t "~%;;; TEST: System ~S found." name))
      T)))
	    
      
	   
(defmethod operate-on-file  ((operation (eql 'EDIT))
                            source-pathname
                            binary-pathname
                            package
                            test
                            verbose
                            keys)
  (declare (ignore binary-pathname package verbose keys))
  (if test
    (format t "~%;;TEST: Editing ~A" source-pathname)
    (ed source-pathname)))

(defun edit-system (name &key test home-directory (group :ALL-GROUPS))
  (let ((keys (if (listp group) group (list group))))
    (operate-on-system name 'EDIT nil "edited" test nil home-directory keys)))

(defun absolute->relative (dir-list)
  (cons :RELATIVE (rest dir-list)))

(defmethod operate-on-file ((operation (eql 'COPY))
                            source-pathname
                            binary-pathname
                            package
                            test
                            verbose
                            keys)
  (declare (ignore package binary-pathname))
  (let* ((to-dirpath (second (member :output-directory keys)))
         (to-pathname (merge-pathnames (make-pathname :name (pathname-name source-pathname)
                                                      :type (pathname-type source-pathname)
                                                      :directory (absolute->relative (pathname-directory source-pathname)))
                                       to-dirpath)))
    (asd-ensure-directories-exist to-dirpath) 
  (if (or (null (probe-file to-pathname))
          (eq source-pathname (which-file-newer source-pathname to-pathname)))
    (if test
      (format t "~%;;TEST: Copying ~A to ~A" source-pathname to-pathname)
      (asd-copy-file source-pathname to-pathname :if-exists :supersede :verbose verbose)))))

(defun copy-system (name &key output-directory test home-directory (verbose t))
  (unless output-directory (error "Output directory must be defined."))
  (unless (pathnamep output-directory) (error "Output directory must be a pathname."))
  (operate-on-system name 'COPY T "copied" test verbose home-directory `(:output-directory ,output-directory)))

(defmethod operate-on-file  (operation
                                source-pathname
                                binary-pathname
                                package
                                test
                                verbose
                                keys)
  (declare (ignore source-pathname binary-pathname package verbose keys test))
  (error "Don't know how to ~S." operation))
                            



;;----------------------------------------------------------------------
;; utility functions for working with pathnames, etc.
;;----------------------------------------------------------------------

(defun which-file-newer (file1 file2)
  (if (> (file-write-date file1) (file-write-date file2))
    file1 file2))

(defun make-system-root-pathname (home-directory directory)
  (cond
   ((null home-directory)
    (ensure-pathname-from-directory directory))
   ((null directory)
    (ensure-pathname-from-directory home-directory))
   (t (merge-pathnames
       (ensure-pathname-from-directory directory)
       (ensure-pathname-from-directory home-directory)))))

(defun ensure-pathname-from-directory (directory)
  (cond
   ((pathnamep directory) directory)
   ((listp directory) 
    (if (or (eq (car directory) :ABSOLUTE)
            (eq (car directory) :RELATIVE))
    (make-pathname :directory directory)
    (error "Invalid directory list: ~S 
(Should begin with :ABSOLUTE or :RELATIVE)." directory)))
   ((stringp directory)
    (let ((pn (parse-namestring directory)))
      (if (pathname-directory pn)
        pn
        (error "Could not parse a directory from ~S" directory))))))


(defun source-pathname (filename dir)
  ; filename is either a string or a list
  (let ((file-pathname 
         (if (stringp filename) 
           (parse-namestring filename)
           (apply 'make-pathname (cdr filename)))))
    (merge-pathnames 
     (make-pathname :name (pathname-name file-pathname)
                    :type  (or (pathname-type file-pathname) (source-file-type))
                    :directory (pathname-directory file-pathname)
                    )
     dir)))

(defun binary-pathname (filename dir options)
  (let ((file-pathname 
         (if (stringp filename) 
	     (parse-namestring filename)
           (apply 'make-pathname (cdr filename)))))
    (merge-pathnames 
     (make-pathname :name (pathname-name file-pathname)
                    :directory (if (find :use-bin-dir options)
				   `(:RELATIVE  "bin" ,*BINARY-SUBDIRECTORY-NAME*)
                                 (pathname-directory file-pathname))
                    :type *binary-file-type*)
     dir)))

(defun make-merged-pathname (dir specs)
  (if (pathnamep dir)
    (merge-pathnames (apply 'make-pathname
                            `(,@specs))
                     dir)
    (apply 'make-pathname `(,@specs :defaults ,(make-pathname :directory dir)))))

(defun directory-pathname (pathspec)
  (parse-namestring (directory-namestring pathspec)))
                 
#+MCL (defun mcl-ensure-directories-exist (pathspec &key verbose)
        (let* ((dir-path (directory-pathname pathspec))
              (created (if (probe-file dir-path) NIL
                           (ccl:create-file dir-path :if-exists NIL))))
          (when (and created verbose)
            (format *standard-output* ";; ~S created." created))
          (if created (values created T)
              (values dir-path NIL))))


(defun asd-ensure-directories-exist (pathspec &key verbose)
  (if (fboundp 'cl::ensure-directories-exist) ; this is the ANSI standard...
    (funcall 'cl::ensure-directories-exist pathspec :verbose verbose)
    #+MCL (mcl-ensure-directories-exist pathspec :verbose verbose)
    #-MCL (error "Add Code here to implement ensure-directories-exist.")))


(defun this-files-directory ()
  "returns a list of the current files directory"
  (let ((source-pn (this-files-pathname)))
    (if (pathnamep source-pn)
      (pathname-directory (translate-logical-pathname source-pn))
      NIL)))
      
      
(defun this-files-pathname ()
  "Returns the directory pathname of the file currently being loaded."
  (car 
   (list
    #+ansi-cl (truename *load-pathname*)
    #+(and :allegro :unix) excl:*source-pathname*
    #+lucid  lucid::*source-pathname*
    #+MCL  (parse-namestring ccl::*LOADING-FILE-SOURCE-FILE*)
    #+(and :coral (not MCL)) (car ccl::*loading-files*)
    #+genera (concatenate 'string
			  (host-namestring sys:fdefine-file-pathname)
			  ":"
			  sys:fdefine-file-pathname
			  )
    #+next  *source-pathname*)))


(defun asd-copy-file (old-pathname new-pathname &key if-exists verbose)
  #-MCL(DECLARE (IGNORE IF-EXISTS))
  (when verbose (format *standard-output* "~&;; Copying ~S to ~S" old-pathname new-pathname))
  #+MCL(ccl:copy-file old-pathname new-pathname :if-exists if-exists)
  #+ALLEGRO(system:copy-file old-pathname new-pathname)
  #+LISPWORKS(system::copy-file old-pathname new-pathname)
  #-(or MCL ALLEGRO)(error "Add Code here to implement asd-copy-file."))
         
