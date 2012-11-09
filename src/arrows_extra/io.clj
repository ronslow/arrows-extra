
(ns arrows-extra.io)
(import [java.io FileOutputStream PrintStream])

(def-arr arr-outputstream [input]
 (FileOutputStream. input)
)

(def-arr arr-printstream [input]
 (PrintStream. input)

)
