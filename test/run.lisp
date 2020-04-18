#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload :boots :silent t)
(time (asdf:test-system :boots))
(quit)
