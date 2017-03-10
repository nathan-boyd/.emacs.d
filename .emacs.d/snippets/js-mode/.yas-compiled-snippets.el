;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("v" "var ${1:element} = ${0:value}\n" "v" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/v" nil nil)
                       ("try" "try {\n  $1\n} catch (e) {\n  $0\n}\n" "try" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/try" nil nil)
                       ("switch" "switch ($1) {\n  $0\n}\n" "switch" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/switch" nil nil)
                       ("ret" "return $0\n" "ret" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/ret" nil nil)
                       ("proto" "\n${1:className}.prototype.${2:methodName} = function(${3:args}) {\n  $0\n}\n" "proto" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/proto" nil nil)
                       ("prop" "var ${1:name} = ${2:null};\n\nthis.__defineGetter__('$1', function () {\n  return $1;\n});\n\nthis.__defineSetter__('$1', function (${5:value}) {\n  $1 = $5;\n});" "prop" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/prop" nil nil)
                       ("promise" "return new Promise((resolve, reject) => {\n\n});\n" "promise" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/promise" nil nil)
                       ("mod" "(function(exports){\n  $0\n})(window.$1 = {});" "mod" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/mod" nil nil)
                       ("mocha-it" "it('$0', function (done) {\n\n});" "mocha-it" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/mocha-it" nil nil)
                       ("mocha-describe" "describe('$0', function () {\n\n});" "mocha-describe" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/mocha-describe" nil nil)
                       ("ins" "instanceof ${1:Function}$0\n" "ins" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/ins" nil nil)
                       ("if" "if (${1:element} === ${2:true}) {\n  $0\n}\n" "if" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/if" nil nil)
                       ("fore" "forEach(function (${1:e}) {\n  $0\n});\n" "fore" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/fore" nil nil)
                       ("for" "for (var ${1:i}=${2:1}, ${3:len}=$4; $1<$3; $1++) {\n  $0\n}\n" "for" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/for" nil nil)
                       ("fn" "function ${1:name} (${2:args}) {\n  $0\n}\n" "fn" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/fn" nil nil)
                       ("f" "function ($1) {\n  $0\n}\n" "f" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/f" nil nil)
                       ("exp" "var $1 = exports.$1 = $0\n" "exp" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/exp" nil nil)
                       ("comment" "/**\n * $0\n */\n" "comment" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/comment" nil nil)
                       ("classheader" "/**\n * $0\n *\n * @author ${1:Author} <${2:Email}>\n * @version ${3:1.0}\n * @class\n * @constructor\n */\n" "classheader" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/classheader" nil nil)
                       ("case" "case $1:\n	$0\n	break;\n\n" "case" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/case" nil nil)
                       ("README.md" "A Collection Of Javascript Snippets For YASnippet\n" "README.md" nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/README.md" nil nil)
                       ("?" "? ${1:true} : ${0:false}\n" "? ... : ..." nil nil nil "/Users/nboyd/.emacs.d/snippets/js-mode/?" nil nil)))


;;; Do not edit! File generated at Wed Mar  8 16:06:44 2017
