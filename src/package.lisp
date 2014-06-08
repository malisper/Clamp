(defpackage "CLAMP"
  (:nicknames "CLAMP")
  (:use "COMMON-LISP")
  (:shadow "DO" "MAP" "IF" "REM" "="
	   "LET" "FIND" "COUNT" "SUBST" "SORT")
  (:export
   ;;; defalias
   "DEFALIAS"
   ;;; aliases
   "FN" "DEF" "--" "MVB" "MVL" "DO" "DO1" "DO2" "="
   "IS" "ISO" "NO" "LEN" "MAP" "ISA" "UNIQ" "EVEN" "ODD"
   "REDUP" "DEDUP" "TABLE" "RAND" "TRUNC" "JOIN" "CUT"
   "REV" "NREV"
   
   ;;; base
   "SINGLE" "PAIR" "CONST" "IF"

   ;;; binding
   "WITH" "LET" "RET" "FLET1" "WITHS"

   ;;; print
   "PR" "PRN"

   ;;; hof
   "TESTIFY" "REM" "KEEP" "MEM" "FIND" "COUNT" "POS"
   "MAPPEND" "SUBST"

   ;;; conditions
   "IFLET" "WHENLET" "AIF" "IT" "AWHEN" "AAND" "AIF2"

   ;;; list
   "RANGE" "FIRSTN" "LAST1" "LEN<" "LEN>"

   ;;; macros
   "MAC" "W/UNIQ"

   ;;; iter
   "REC" "RECUR" "REPEAT" "UP" "DOWN" "WHILE" "UNTIL" "EACH"

   ;;; fns
   "RFN" "AFN"
   
   ;;; misc
   "ADO" "ACCUM" "MULTIPLE" "CHECK" "ZAP" "OR=" "IN" "CART"

   ;;; fnops
   "COMPOSE" "FIF" "ANDF" "ORF" "CURRY" "RCURRY"

   ;;; sort
   "COMPARE" "BEST" "BESTN" "SORT"

   ;;; memoize
   "MEMO" "DEFMEMO"

   ;;; tables
   "KEYS" "VALS" "LISTTAB" "TABLIST" "OBJ" "ALREF"
   ))
