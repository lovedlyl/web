(in-package #:web)

(defpsmacro node-lib()
  `(progn 
     (var http (require "http"))
     (var fs (require "fs"))
     (var path (require "path"))
     (var url (require "url"))
     ,@*node-macros*
     ))
