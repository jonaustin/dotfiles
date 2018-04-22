;nyquist plug-in
;version 1
;type generate
;categories "http://lv2plug.in/ns/lv2core/#OscillatorPlugin"
;name "Binaural Tones..."
;action "Generating binaural tones..."
;info "By Steve Daulton (www.easyspacepro.com)\nReleased under GPL v2\n\n*** IMPORTANT *** A stereo track must be selected."

;; By Steve Daulton. September 2010.
;; Multi-period uopdate: December 2010.
;; Released under terms of the GNU General Public License version 2 
;; http://www.opensource.org/licenses/gpl-license.php

;control freq "Average Frequency" real "Hz" 110 0 2000
;control b1 "Beat Frequency" real "Hz" 8 0 30
;control p1 "First Period Duration" real "seconds" 60 0 1000
;control b2 "Beat Frequency" real "Hz" 3 0 30
;control p2 "Second Period Duration" real "seconds" 0 0 1000
;control b3 "Beat Frequency" real "Hz" 0 0 30
;control p3 "Third Period Duration" real "seconds" 0 0 1000
;control b4 "Beat Frequency" real "Hz" 0 0 30
;control p4 "Fourth Period Duration" real "seconds" 0 0 1000
;control b5 "Beat Frequency" real "Hz" 0 0 30

(setq err "");initialise error message
(when(and(= p1 0)(= p2 0)(= p3 0)(= p4 0))(setq err "All periods are zero.\n"))
(when(or(< p1 0)(< p2 0)(< p3 0)(< p4 0))(setq err(strcat err "Periods cannot be negative.\n")))
(if(> freq (/ *sound-srate* 2.0))
  (setq err(strcat err "Average frequency too high for sample rate.\n"))
  (when (> (+ freq (max b1 b2 b3 b4 b5))(/ *sound-srate* 2.0))(setq err(strcat err "Generated tone contains frequencies too high for sample rate.\n"))))

(if (>(length err)0)
  (format nil "Error!~%~a" err); output error message
  (progn
    (setq duration (+ p1 p2 p3 p4))
    (setq LEN (* duration *sound-srate*))
    (setf breakpoints-left (list 0(/ b1 2.0) p1(/ b2 2.0) p2(/ b3 2.0) p3(/ b4 2.0) p4(/ b5 2.0) 0))
    (setf breakpoints-right (list 0(/ b1 -2.0) p1(/ b2 -2.0) p2(/ b3 -2.0) p3(/ b4 -2.0) p4(/ b5 -2.0) 0))
    (setf f-shift-left (pwlr-list breakpoints-left))
    (setf f-shift-right (pwlr-list breakpoints-right))
    (stretch-abs duration 
      (mult 0.8 
        (vector
          (fmosc (hz-to-step freq)f-shift-left)
          (fmosc (hz-to-step freq)f-shift-right))))))

