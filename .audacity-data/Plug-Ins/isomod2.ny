;nyquist plug-in
;version 3
;type process
;categories "http://lv2plug.in/ns/lv2core#ModulatorPlugin"
;name "IsoMod 2..."
;action "Modulating..."
;info "Isochronic modularotor with a choice of pulse shapes."
;author "Steve Daulton"
;copyright "Released under terms of the GNU General Public License version 2" 


;; isomod2.ny by Steve Daulton Dec 2014.
;; Implementing the idea for sine pulse by Carlos H. Ramarez G.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html

;control wave "Pulse type" choice "Trapezoid,Triangle,Sine" 0
;control var "Pulse shape variation" int "" 50 0 100
;control pw "Pulse width" int "%" 30 1 100
;control start-Hz "Initial Modulation Frequency" real "Hz" 7 1 20
;control end-Hz "Final Modulation Frequency" real "Hz" 2 1 20
;control start-depth "Initial Modulation Depth" int "%" 50 0 100
;control end-depth "Final Modulation Depth" int "%" 50 0 100



;limit input variables to sensible range
(defmacro limit (val minv maxv)
  `(setf ,val (min (max ,val ,minv) ,maxv)))

;; Convert % to range 0 to 1
(defmacro pcent (x)
  `(setq ,x (/ ,x 100.0)))

; Avoid division by zero errors
(pcent var)
(limit var 0.01 0.99)

; To prevent aliasing, the pulses must be a reasonable length
(pcent pw)
(limit pw 0.01 1)

; Limit frequencies to half Nyquist frequency
(limit start-Hz 0.01 (/ *sound-srate* 4.0))
(limit end-Hz 0.01 (/ *sound-srate* 4.0))

; Limit depth 0 to 100%
(pcent start-depth)
(pcent end-depth)
(limit start-depth 0 1)
(limit end-depth 0 1)


;;; Define wavetables

(defun sinewav (width bias)
  (setf pulse (seq
    (osc (hz-to-step (/  (* 2  bias width))) (* bias width) *sine-table* 270)
    (osc (hz-to-step (/  (* 2 (- 1 bias) width))) (* (- 1 bias) width) *sine-table* 90)))
  (setf pulse (sum 0.5 (mult 0.5 pulse)))
  (list (sim pulse (s-rest 1))(hz-to-step 1) T))

(defun triwav (width bias)
  (let* ((bias (* 2 (- 0.5 bias)))
         (width (/ width 2.0))
         (bias (* bias width)))
    ;make pulse shape
    (setq pulse (pwl (- width bias) 1 (* width 2) 0 1 0))
    ;make wave-table
    (list pulse (hz-to-step 1.0) t)))

(defun sqwav (pw ft)
  ; fade time = half variation (allow for fade-in + fade-out)
  (let* ((ft (/ ft 2.0))
         ;fade time as proportion of pulse width
         (ft (* pw ft)))
    ;make pulse shape
    (setq pulse (pwl ft 1 (- pw ft) 1 pw 0 1 0))
    ;make wave-table
    (list pulse (hz-to-step 1.0) t)))


;; Generate sweep tone
(defun sweep (sf ef)
  (fmlfo (pwlv sf 1.0 ef) *waveform*))

;; Select required wavetable
(setq *waveform*
  (abs-env 
    (case wave
      (0 (sqwav pw var))
      (1 (triwav pw var))
      (T (sinewav pw var)))))

;; Process audio
(let* ((wet (pwlv start-depth 1 end-depth))
       (dry (sum 1 (mult wet -1))))
  (mult s
    (sum dry
      (mult wet (sweep start-Hz end-Hz)))))
