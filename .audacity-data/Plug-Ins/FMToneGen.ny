;nyquist plug-in
;version 3
;type generate
;categories "http://lv2plug.in/ns/lv2core#GeneratorPlugin"
;name "FM Tone Generator..."
;action "Generating Tone..."
;info "by Steve Daulton (www.easyspacepro.com).\nReleased under GPL v2.\n"

;;control mod-type "Modulation Type" choice "Amplitude,Frequency" 0
;control baseband "Base frequency (message)" real "Hz" 440 10 2000
;control bwave "Base frequency type" choice "Sine,Square,Triangle,Saw,Inverse Saw" 0
;control carrier "Modulation Frequency (carrier)" real "Hz" 10 0 1000
;control cwave "Modulation frequency type" choice "Sine,Square,Triangle,Saw,Inverse Saw" 0
;;control am-depth "AM Depth" real "0 to 1" 1 0 1
;control fm-width "Modulation width (deviation)" real "Hz" 10 0 100
;control duration "Duration" real "seconds" 10 0 100
;control gain "Amplitude" real "0 to 1" 0.8 0 1

;; FMToneGen.ny by Steve Daulton April 2013,
;; based on ModToneGen.ny by Steve Daulton April 2013.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; http://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

;; Allow parameters to be commented out
(if (not (boundp 'mod-type))
    (setq mod-type 1)) ; Frequency Modulation
(if (not (boundp 'am-depth))
    (setq am-depth 0))
(if (not (boundp 'fm-width))
    (setq fm-width 0))

(defun wavetable (hz type)
  (case type
        (1 (hq-sq-table hz))
        (2 (hq-tri-table hz))
        (3 (hq-saw-table hz))
        (4 (hq-inv-saw-table hz))
        (T *sine-table*)))

;; Square table (no alias)
(defun hq-sq-table (f)
  (setq f (float f))
  (let* ((wave (build-harmonic 1 2048)))
    (do ((i 3.0 (setq i (+ 2.0 i))))           ; odd harmonics
        ((or (>= (* i f)(/ *sound-srate* 2.0))(> i 2048)) wave)
      (setq wave (sum wave (mult (/ i) (build-harmonic i 2048)))))
    (setq gain (/ (peak wave ny:all)))
    (maketable (mult gain wave))))

;; Triangle table  (no alias)
(defun hq-tri-table (f)
  (setq f (float f))
  (let ((wave (build-harmonic 1 2048)))
    (do ((i 3.0 (setq i (+ 2.0 i)))            ; odd harmonics
         (alt -1.0 (setq alt (* -1.0 alt))))   ; reverse phase of alternate harmonics
        ((or (>= (* i f)(/ *sound-srate* 2.0))(> i 2048)) wave)
      (setq wave 
        (sum wave (mult (/ alt(* i i))(build-harmonic i 2048)))))
    (setq gain (/ (peak wave ny:all)))
    (maketable (mult gain wave))))

;; Saw table (no alias)
(defun hq-saw-table (f)
  (setq f (float f))
  (let* ((wave (build-harmonic 1 2048)))
    (do ((i 2.0 (setq i (1+ i))))              ; all harmonics
        ((or (>= (* i f)(/ *sound-srate* 2.0))(> i 2048)) wave)
      (setq wave (sum wave (mult (/ i) (build-harmonic i 2048)))))
    (setq gain (/ -1 (peak wave ny:all)))
    (maketable (mult gain wave))))

;; Inverse Saw table (no alias)
(defun hq-inv-saw-table (f)
  (setq f (float f))
  (let* ((wave (build-harmonic 1 2048)))
    (do ((i 2.0 (setq i (1+ i))))              ; all harmonics
        ((or (>= (* i f)(/ *sound-srate* 2.0))(> i 2048)) wave)
      (setq wave (sum wave (mult (/ i) (build-harmonic i 2048)))))
    (setq gain (/ (peak wave ny:all)))
    (maketable (mult gain wave))))

;; Amplitude Modulation
(defun AM (bhz chz bwave cwave depth)
  (mult (hzosc bhz (wavetable bhz bwave))
    (sum (- 1 depth)
      (mult 0.5
        depth
        (sum 1 (hzosc chz (wavetable chz cwave)))))))

;; Frequency Modulation
(defun FM (bhz chz bwave cwave width)
  (fmosc (hz-to-step bhz)
         (mult 0.5
           width
           (hzosc chz (wavetable chz cwave)))
         (wavetable bhz bwave)))


(stretch-abs duration
  (mult gain
    (case mod-type
      (0 (if (and (> carrier 0)(> am-depth 0))
             (AM baseband carrier bwave cwave am-depth)
             (hzosc baseband (wavetable baseband bwave))))
      (T (if (and (> fm-width 0)(> carrier 0))
             (FM baseband carrier bwave cwave fm-width)
             (hzosc baseband (wavetable baseband bwave)))))))
