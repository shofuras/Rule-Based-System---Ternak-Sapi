;;;======================================================
;;;     Sistem Pakar Penyakit Sapi
;;;
;;;     Sistem pakar ini mendiagnosis
;;;     penyakit sapi
;;;======================================================

;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question ya tidak y n))
   (if (or (eq ?response ya) (eq ?response y))
       then yes 
       else no))

;;;***************
;;;* QUERY RULES *
;;;***************

(defrule pertanyaan-menunjukkan-gejala ""
   (not (menunjukkan-gejala ?))
   (not (penyakit ?))
   =>
   (assert (menunjukkan-gejala (yes-or-no-p "Apakah sapi Anda menunjukkan gejala penyakit? (ya/tidak): "))))
   
(defrule pertanyaan-nafsu-makan-berkurang ""
   (menunjukkan-gejala yes)
   (not (penyakit ?))
   =>
   (assert (nafsu-makan-berkurang (yes-or-no-p "Apakah nafsu makan sapi Anda berkurang? (ya/tidak): "))))

(defrule pertanyaan-demam-nafsumakanturun ""
   (nafsu-makan-berkurang yes)
   (not (penyakit ?))   
   =>
   (assert (makan-turun-demam (yes-or-no-p "Apakah sapi Anda mengalami demam? (ya/tidak): "))))

(defrule pertanyaan-demam-nafsumakantetap ""
   (nafsu-makan-berkurang no)
   (not (penyakit ?))   
   =>
   (assert (makan-tetap-demam (yes-or-no-p "Apakah sapi Anda mengalami demam? (ya/tidak): "))))

(defrule pertanyaan-ngorok ""
   (makan-turun-demam yes)
   (not (penyakit ?))   
   =>
   (assert (ngorok (yes-or-no-p "Apakah sapi Anda mengalami ngorok? (ya/tidak): "))))

(defrule pertanyaan-sering-batuk ""
   (ngorok no)
   (not (penyakit ?))   
   =>
   (assert (sering-batuk (yes-or-no-p "Apakah sapi Anda sering batuk? (ya/tidak): "))))

(defrule pertanyaan-kencing-merah ""
   (sering-batuk no)
   (not (penyakit ?))   
   =>
   (assert (kencing-merah (yes-or-no-p "Apakah sapi Anda mengalami kencing merah? (ya/tidak): "))))

(defrule pertanyaan-luka-kuku ""
   (kencing-merah no)
   (not (penyakit ?))   
   =>
   (assert (luka-kuku (yes-or-no-p "Apakah sapi Anda memiliki luka di antara kuku? (ya/tidak): "))))

(defrule pertanyaan-susu-menggumpal ""
   (makan-turun-demam no)
   (not (penyakit ?))   
   =>
   (assert (susu-menggumpal (yes-or-no-p "Apakah air susu sapi Anda menggumpal ketika diperah? (ya/tidak): "))))

(defrule pertanyaan-menggosok-badan ""
   (susu-menggumpal no)
   (not (penyakit ?))   
   =>
   (assert (menggosok-badan (yes-or-no-p "Apakah sapi Anda sering menggosokkan badannya ke dinding kandang? (ya/tidak): "))))

(defrule pertanyaan-perut-mengembang ""
   (menggosok-badan no)
   (not (penyakit ?))   
   =>
   (assert (perut-mengembang (yes-or-no-p "Apakah perut bagian kiri sapi Anda mengembang? (ya/tidak): "))))

(defrule pertanyaan-perut-buncit ""
   (perut-mengembang no)
   (not (penyakit ?))   
   =>
   (assert (perut-buncit (yes-or-no-p "Apakah perut sapi Anda buncit? (ya/tidak): "))))

(defrule pertanyaan-cairan-berlebih ""
   (makan-tetap-demam yes)
   (not (penyakit ?))   
   =>
   (assert (cairan-berlebih (yes-or-no-p "Apakah sapi Anda mengeluarkan cairan berlebih pada mata, hidung, dan mulut? (ya/tidak): "))))

(defrule pertanyaan-bengkak ""
   (cairan-berlebih no)
   (not (penyakit ?))   
   =>
   (assert (bengkak (yes-or-no-p "Apakah sapi Anda mengalami pembengkakan pada leher, dada dan perut? (ya/tidak): "))))

(defrule pertanyaan-mata-merah ""
   (makan-tetap-demam no)
   (not (penyakit ?))   
   =>
   (assert (mata-merah (yes-or-no-p "Apakah sapi Anda memiliki mata yang memerah? (ya/tidak): "))))

(defrule pertanyaan-feses-putihcair ""
   (mata-merah no)
   (not (penyakit ?))   
   =>
   (assert (feses-putihcair (yes-or-no-p "Apakah sapi Anda mmemiliki feses cair dan berwarna putih? (ya/tidak): "))))

;;;******************
;;;* RULES PENYAKIT *
;;;******************

(defrule sapi-sehat ""
   (menunjukkan-gejala no)
   (not (penyakit ?))
   =>
   (assert (penyakit "Berdasarkan sistem, sapi Anda sehat. Untuk memastikan, periksakan ke dokter/ahli.")))

(defrule sapi-septicaemia-eizooticae ""
   (ngorok yes)
   (not (penyakit ?))
   =>
   (assert (penyakit "Sapi Anda memiliki penyakit Septicaemia Eizooticae.")))

(defrule sapi-pneumonia ""
   (sering-batuk yes)
   (not (penyakit ?))
   =>
   (assert (penyakit "Sapi Anda memiliki penyakit pneumonia.")))

(defrule sapi-leptospirosis ""
   (kencing-merah yes)
   (not (penyakit ?))
   =>
   (assert (penyakit "Sapi Anda memiliki penyakit leptospirosis.")))

(defrule sapi-pmk ""
   (luka-kuku yes)
   (not (penyakit ?))
   =>
   (assert (penyakit "Sapi Anda memiliki penyakit mulut dan kuku (PMK).")))

(defrule sapi-mastitis ""
   (susu-menggumpal yes)
   (not (penyakit ?))
   =>
   (assert (penyakit "Sapi Anda memiliki penyakit mastitis.")))

(defrule sapi-scabies ""
   (menggosok-badan yes)
   (not (penyakit ?))
   =>
   (assert (penyakit "Sapi Anda memiliki penyakit Scabies atau kudis.")))

(defrule sapi-bloat ""
   (perut-mengembang yes)
   (not (penyakit ?))
   =>
   (assert (penyakit "Sapi Anda memiliki penyakit bloat.")))

(defrule sapi-cacingan ""
   (perut-buncit yes)
   (not (penyakit ?))
   =>
   (assert (penyakit "Sapi Anda memiliki penyakit cacingan.")))

(defrule sapi-ephemeral-fever ""
   (cairan-berlebih yes)
   (not (penyakit ?))
   =>
   (assert (penyakit "Sapi Anda memiliki penyakit Ephemeral Fever.")))

(defrule sapi-anthrax ""
   (bengkak yes)
   (not (penyakit ?))
   =>
   (assert (penyakit "Sapi Anda memiliki penyakit anthrax atau radang limpa.")))

(defrule sapi-pinkeye ""
   (mata-merah yes)
   (not (penyakit ?))
   =>
   (assert (penyakit "Sapi Anda memiliki penyakit pink eye.")))

(defrule sapi-colibaclillosis ""
   (feses-putihcair yes)
   (not (penyakit ?))
   =>
   (assert (penyakit "Sapi Anda memiliki penyakit colibaclillosis.")))

(defrule no-penyakit ""
  (declare (salience -10))
  (not (penyakit ?))
  =>
  (assert (penyakit "Penyakit tidak diketahui. Periksakan ke dokter/ahli.")))

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "Sistem Pakar Penyakit Sapi")
  (printout t crlf crlf))

(defrule print-penyakit ""
  (declare (salience 10))
  (penyakit ?item)
  =>
  (printout t crlf crlf)
  (printout t "Hasil diagnosis:")
  (printout t crlf crlf)
  (format t " %s%n%n%n" ?item))