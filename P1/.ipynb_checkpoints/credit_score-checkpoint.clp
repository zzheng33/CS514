; credit_score.clp
; Educational credit scoring expert system (TransUnion-style factors)

(deftemplate applicant
  (slot name)
  (slot total_accounts (type INTEGER))
  (slot credit_age_years (type NUMBER))
  (slot payment_history_pct (type NUMBER))
  (slot credit_usage_pct (type NUMBER))
  (slot derogatory_marks (type INTEGER))
  (slot hard_inquiries (type INTEGER))
)

(deftemplate credit_result
  (slot name)
  (slot score)
  (slot grade)
)

; Weights tuned: payment history high, credit usage high,
; derogatory high, credit age medium, accounts and inquiries low
(defglobal
  ?*w_payment* = 35
  ?*w_usage*   = 30
  ?*w_derog*   = 20
  ?*w_age*     = 10
  ?*w_accts*   = 3
  ?*w_inq*     = 2
)

(deffunction clamp (?x ?lo ?hi)
  (if (< ?x ?lo) then (return ?lo))
  (if (> ?x ?hi) then (return ?hi))
  (return ?x)
)

(deffunction score_payment (?pct)
  (return (round (clamp ?pct 0 100)))
)

(deffunction score_usage (?pct)
  (if (<= ?pct 7)   then (return 100))
  (if (<= ?pct 10)  then (return 95))
  (if (<= ?pct 20)  then (return 85))
  (if (<= ?pct 30)  then (return 70))
  (if (<= ?pct 50)  then (return 50))
  (if (<= ?pct 75)  then (return 30))
  (if (<= ?pct 100) then (return 10))
  (return 0)
)

(deffunction score_age (?yrs)
  (if (>= ?yrs 10) then (return 100))
  (if (>= ?yrs 8)  then (return 90))
  (if (>= ?yrs 5)  then (return 80))
  (if (>= ?yrs 3)  then (return 60))
  (if (>= ?yrs 2)  then (return 45))
  (if (>= ?yrs 1)  then (return 30))
  (return 10)
)

(deffunction score_accts (?n)
  (if (and (>= ?n 3) (<= ?n 6)) then (return 100))
  (if (or (and (>= ?n 1) (<= ?n 2)) (and (>= ?n 7) (<= ?n 9))) then (return 70))
  (if (= ?n 0)   then (return 30))
  (if (>= ?n 10) then (return 50))
  (return 60)
)

(deffunction score_derog (?n)
  (if (= ?n 0) then (return 100))
  (if (= ?n 1) then (return 80))
  (if (= ?n 2) then (return 40))
  (if (>= ?n 3) then (return 10))
  (return 0)
)

(deffunction score_inq (?n)
  (if (= ?n 0) then (return 100))
  (if (= ?n 1) then (return 90))
  (if (= ?n 2) then (return 75))
  (if (= ?n 3) then (return 55))
  (if (= ?n 4) then (return 35))
  (if (>= ?n 5) then (return 10))
  (return 0)
)

(deffunction grade_of (?s)
  (if (>= ?s 800) then (return excellent))
  (if (>= ?s 740) then (return very_good))
  (if (>= ?s 670) then (return good))
  (if (>= ?s 580) then (return fair))
  (return poor)
)

(defrule compute_credit
  ?a <- (applicant
          (name ?n)
          (total_accounts ?accts)
          (credit_age_years ?age)
          (payment_history_pct ?php)
          (credit_usage_pct ?usage)
          (derogatory_marks ?der)
          (hard_inquiries ?inq))
  =>
  (bind ?sp (score_payment ?php))
  (bind ?su (score_usage ?usage))
  (bind ?sa (score_age ?age))
  (bind ?so (score_accts ?accts))
  (bind ?sd (score_derog ?der))
  (bind ?si (score_inq ?inq))

  (bind ?weighted
    (+ (* ?*w_payment* ?sp)
       (* ?*w_usage*   ?su)
       (* ?*w_derog*   ?sd)
       (* ?*w_age*     ?sa)
       (* ?*w_accts*   ?so)
       (* ?*w_inq*     ?si)))

  (bind ?points (/ ?weighted 100.0))
  (bind ?score (+ 300 (round (* ?points 5.5))))
  (bind ?grade (grade_of ?score))

  (printout t crlf)
  (printout t "Applicant: " ?n crlf)
  (printout t "  Inputs:" crlf)
  (printout t "    Payment history %: " ?php crlf)
  (printout t "    Credit usage %:   " ?usage crlf)
  (printout t "    Credit age:       " ?age " years" crlf)
  (printout t "    Total accounts:   " ?accts crlf)
  (printout t "    Derogatory marks: " ?der crlf)
  (printout t "    Hard inquiries:   " ?inq crlf)
  (printout t "  Score: " ?score crlf)
  (printout t "  Grade: " ?grade crlf crlf)

  (assert (credit_result (name ?n) (score ?score) (grade ?grade)))
  (retract ?a)
)

; four test applicants covering the four grade bands
(deffacts samples
  (applicant
    (name "User 4")
    (total_accounts 5)
    (credit_age_years 12)
    (payment_history_pct 99.5)
    (credit_usage_pct 5)
    (derogatory_marks 0)
    (hard_inquiries 0))

  (applicant
    (name "User 3")
    (total_accounts 4)
    (credit_age_years 7)
    (payment_history_pct 95.0)
    (credit_usage_pct 18)
    (derogatory_marks 1)
    (hard_inquiries 1))

  (applicant
    (name "User 2")
    (total_accounts 3)
    (credit_age_years 4)
    (payment_history_pct 91.0)
    (credit_usage_pct 28)
    (derogatory_marks 1)
    (hard_inquiries 2))

  (applicant
    (name "User 1")
    (total_accounts 2)
    (credit_age_years 2)
    (payment_history_pct 85.0)
    (credit_usage_pct 55)
    (derogatory_marks 2)
    (hard_inquiries 3))
)
