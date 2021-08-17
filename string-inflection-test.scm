;; Copyright 2020 Lassi Kortela
;; SPDX-License-Identifier: ISC

(import (scheme base) (srfi 64) (lassik string-inflection))

(test-begin "string-inflection")

(test-equal "command-output-to-string"
  (string-inflection-lisp "command_output_to_string"))

(test-equal "command_output_to_string"
  (string-inflection-underscore "command--output_to-string"))

(test-equal "a_single_line"
  (string-inflection-underscore "ASingleLine"))

(test-equal '("PHP" "Mode")
  (string-inflection-split "PHPMode"))

(test-equal '("Ends" "With" "PHP")
  (string-inflection-split "EndsWithPHP"))

(test-equal '("PHP" "And" "XML" "Too")
  (string-inflection-split "PHPAndXMLToo"))

(test-equal '("php" "And" "Xml" "Too")
  (string-inflection-split "phpAndXmlToo"))

;; This one does not have the expected split.
(test-equal '("PH" "Pand" "XM" "Ltoo")
  (string-inflection-split "PHPandXMLtoo"))

(test-equal '("list" "take" "right")
  (string-inflection-split "list-take-right"))

(test-equal '("list" "null" "?")
  (string-inflection-split "list-null?"))

(test-equal '("string" "=" "?")
  (string-inflection-split "string=?"))

(test-equal '("string" "ci" "<=" "?")
  (string-inflection-split "string-ci<=?"))

(test-equal '("what" "happened" "?" "!")
  (string-inflection-split "what-happened?!"))

(test-equal '()
  (string-inflection-split ""))

(test-equal ""
  (string-inflection-caps-upper ""))

(test-equal "FooBarBaz"
  (string-inflection-caps-upper "foo-bar-baz"))

(test-equal "fooBarBaz"
  (string-inflection-caps-lower "foo-bar-baz"))

(test-equal "XmlToJson"
  (string-inflection-caps-upper "XML-to-JSON"))

(test-end "string-inflection")
