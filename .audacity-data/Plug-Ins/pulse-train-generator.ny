;nyquist plug-in
;version 3
;type generate
;name "Pulse Train Generator..."
;action "Pulse Train Generator..."

;control pulse-frequency "First Harmonic" real "Hz" 8000.0 1.0 20000.0
;control sound-duration "Duration" real "seconds" 3.0 0.001 10.0

;; replacement for the "frequency" slider in the original plugin

;control freq "Frequency" real "Hz" 100 10 10000
;control dur "Length" real "seconds" 3 1 100

;; bandwidth limited pulse *pulse-table*


(setq *pulse-table* (s-rest 1))               ; initialise *pulse-table*
(setq len (* dur *sound-srate*))              ; make progress bar
(setq wlength (/ *sound-srate* freq))         ; wavelength in samples
(setq nyqf (/ *sound-srate* 2.1))             ; below Nyquist frequency

(defun maketable (freq)
  (let ((wl (* 2 (/ freq))))
    (do ((i 1 (1+ i)))
        ((or (>= (* i freq) nyqf)(> i 2048)) *pulse-table*)
      (setq *pulse-table*
        (sim *pulse-table*
          (osc (hz-to-step (* freq i)) wl *table* 90))))
    ;; trim start from sound
    (setq *pulse-table*
      (extract-abs (/ freq) wl *pulse-table*)))
  (setq *pulse-table*
    (mult *pulse-table*
      (/ (peak *pulse-table* (truncate (* 2 wlength))))))
  (list *pulse-table* (hz-to-step freq) T))

(osc (hz-to-step freq) dur (maketable freq))
