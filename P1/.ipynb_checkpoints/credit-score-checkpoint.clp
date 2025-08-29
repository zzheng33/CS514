; credit_score.clp
; Educational credit scoring expert system
; Inputs use 0 to 100 percentages for payment_history_pct and utilization_pct

(deftemplate applicant
  (slot name)
  (slot num_open_cards (type INTEGER))
  (slot avg_card_age_years (type FLOAT))
  (slot payment_history_pct (type FLOAT))
  (slot utilization_pct (type FLOAT))
  (slot derogatory_count (type INTEGER))
  (slot hard_inquiries_24m (type INTEGER))
)

(deftemplate credit_result
  (slot name)
  (slot score)
  (slot grade)
)

; weights set by assistant
(defglobal
  ?*w_payment* = 35
  ?*w_util*    = 30
  ?*w_age*     = 15
  ?*w_open*    = 8
  ?*w_derog*   = 7
  ?*w_inq*     = 5
)

(deffunction clamp (?x ?lo ?hi)
  (if (< ?x ?lo) then (return ?lo))
  (if (> ?x ?hi) then (return ?hi))
  (return ?x)
)

(deffunction score_payment (?pct)
  (return (round (clamp ?pct 0 100)))
)

(deffunction score_utilization (?pct)
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

(deffunction score_open (?n)
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
          (num_open_cards ?noc)
          (avg_card_age_years ?age)
          (payment_history_pct ?php)
          (utilization_pct ?util)
          (derogatory_count ?der)
          (hard_inquiries_24m ?inq))
  =>
  (bind ?sp (score_payment ?php))
  (bind ?su (score_utilization ?util))
  (bind ?sa (score_age ?age))
  (bind ?so (score_open ?noc))
  (bind ?sd (score_derog ?der))
  (bind ?si (score_inq ?inq))

  (bind ?weighted
    (+ (* ?*w_payment* ?sp)
       (* ?*w_util*    ?su)
       (* ?*w_age*     ?sa)
       (* ?*w_open*    ?so)
       (* ?*w_derog*   ?sd)
       (* ?*w_inq*     ?si)))

  (bind ?points (/ ?weighted 100.0))
  (bind ?score (+ 300 (round (* ?points 5.5))))
  (bind ?grade (grade_of ?score))

  (printout t crlf)
  (printout t "Applicant " ?n crlf)
  (printout t "Breakdown" crlf)
  (printout t "  payment history " ?sp " weight " ?*w_payment* crlf)
  (printout t "  utilization " ?su " weight " ?*w_util* crlf)
  (printout t "  average age " ?sa " weight " ?*w_age* crlf)
  (printout t "  open cards " ?so " weight " ?*w_open* crlf)
  (printout t "  derogatory " ?sd " weight " ?*w_derog* crlf)
  (printout t "  inquiries " ?si " weight " ?*w_inq* crlf)
  (printout t "Final score " ?score " grade " ?grade crlf crlf)

  (assert (credit_result (name ?n) (score ?score) (grade ?grade)))
  (retract ?a)
)

; optional quick tests
(deffacts samples
  (applicant
    (name "Alex")
    (num_open_cards 4)
    (avg_card_age_years 6.2)
    (payment_history_pct 98.5)
    (utilization_pct 12)
    (derogatory_count 0)
    (hard_inquiries_24m 1))
  (applicant
    (name "Blair")
    (num_open_cards 2)
    (avg_card_age_years 2.5)
    (payment_history_pct 92)
    (utilization_pct 48)
    (derogatory_count 1)
    (hard_inquiries_24m 3))
)
