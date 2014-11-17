(in-package :clamp)

(def since (t1)
  "Returns the number of seconds since universal time T1."
  (- (seconds) t1))

(def minutes-since (t1)
  "Returns the number of minutes since universal time T1."
  (/ (since t1) 60))

(def hours-since (t1)
  "Returns the number of hours since universal time T1."
  (/ (since t1) 3600))

(def days-since (t1)
  "Returns the number of days since universal time T1."
  (/ (since t1) 86400))
