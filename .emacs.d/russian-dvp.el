;; This is an input method for Russian, which assumes that:
;;  1) Host keyboard layout is Programmer Dvorak (DVP)
;;  2) Russian layout follows JCUKEN, but with several crucial differences:
;;     - there is no letter Ё (I don't use that one anyway)
;;     - number/punctuation row is mostly unchanged from DVP (to prevent
;;       switching between two numeric input methods), but some unshifted
;;       positions are changed to accomodate symbols "evicted" from
;;       greater keyboard area due to larger Russian alphabet size (32
;;       without Ё vs 26):
;;       + "=" remaps to ","
;;       + "*" remaps to "."
;;       + "&/%" remaps to ";/:" (effectively shifting its DVP location
;;         one row up, but still within pinky reach)
;;       + "$/~" remaps to "-/№" (to accomodate two useful symbols not on
;;         DVP number row)
;;       + "`" remaps to double quote
;;       + "\" remaps to "/", "|" remaps to "~"
;;       + "z" remaps to "%"
;;  3) This layout reflects my typing preferences - I'm a touch typist in
;;     DVP and not in JCUKEN, therefore I value preserving DVP muscle
;;     memory more than the loss of speed due to strange locations and
;;     occasional need to C-\ out of the input method for "rare" symbols.

(quail-define-package
 "russian-dvp" "Russian" "RU" nil
 "ЙЦУКЕН Mac Russian Programmer Dvorak layout (with some tweaks)"
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("$" ?-)
 ("&" ?\;)
 ("[" ?[)
 ("{" ?{)
 ("}" ?})
 ("(" ?()
 ("=" ?,)
 ("*" ?.)
 (")" ?))
 ("+" ?+)
 ("]" ?])
 ("!" ?!)
 ("#" ?#)
 ("~" ?№)
 ("%" ?:)
 ("7" ?7)
 ("5" ?5)
 ("3" ?3)
 ("1" ?1)
 ("9" ?9)
 ("0" ?0)
 ("2" ?2)
 ("4" ?4)
 ("6" ?6)
 ("8" ?8)
 ("`" ?\")
 (";" ?й)
 ("," ?ц)
 ("." ?у)
 ("p" ?к)
 ("y" ?е)
 ("f" ?н)
 ("g" ?г)
 ("c" ?ш)
 ("r" ?щ)
 ("l" ?з)
 ("/" ?х)
 ("@" ?ъ)
 (":" ?Й)
 ("<" ?Ц)
 (">" ?У)
 ("P" ?К)
 ("Y" ?Е)
 ("F" ?Н)
 ("G" ?Г)
 ("C" ?Ш)
 ("R" ?Щ)
 ("L" ?З)
 ("?" ?Х)
 ("^" ?Ъ)
 ("a" ?ф)
 ("o" ?ы)
 ("e" ?в)
 ("u" ?а)
 ("i" ?п)
 ("d" ?р)
 ("h" ?о)
 ("t" ?л)
 ("n" ?д)
 ("s" ?ж)
 ("-" ?э)
 ("\\" ?/)
 ("A" ?Ф)
 ("O" ?Ы)
 ("E" ?В)
 ("U" ?А)
 ("I" ?П)
 ("D" ?Р)
 ("H" ?О)
 ("T" ?Л)
 ("N" ?Д)
 ("S" ?Ж)
 ("_" ?Э)
 ("|" ?~)
 ("'" ?я)
 ("q" ?ч)
 ("j" ?с)
 ("k" ?м)
 ("x" ?и)
 ("b" ?т)
 ("m" ?ь)
 ("w" ?б)
 ("v" ?ю)
 ("z" ?%)
 ("\"" ?Я)
 ("Q" ?Ч)
 ("J" ?С)
 ("K" ?М)
 ("X" ?И)
 ("B" ?Т)
 ("M" ?Ь)
 ("W" ?Б)
 ("V" ?Ю)
 ("Z" ??))
