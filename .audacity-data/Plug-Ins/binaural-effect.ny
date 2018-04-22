;nyquist plug-in
;version 1
;type effect
;categories "http://lv2plug.in/ns/lv2core/#OscillatorPlugin"
;name "Binaural Tone..."
;action "Generating binaural tones..."
;info "By Steve Daulton (www.easyspacepro.com)\nReleased under GPL v2\n\n*** IMPORTANT *** A stereo track must be selected."

;; By Steve Daulton. September 2010.
;; Released under terms of the GNU General Public License version 2 
;; http://www.opensource.org/licenses/gpl-license.php

;control freq "Average Frequency" real "Hz" 110 0 2000
;control i-bfreq "Initial Beat Frequency" real "Hz" 8 0 30
;control f-bfreq "Final Beat Frequency" real "Hz" 3 0 30
;control amp "Amplitude (0 to 1)" real "" 0.8 0 1
	
(setq dur 1)

(setq err "");initialise error message
(when(= dur 0)(setq err "Duration is set at zero.\n"))
(when(< dur 0)(setq err(strcat err "Duration cannot be negative.\n")))
(if(> freq (/ *sound-srate* 2.0))
  (setq err(strcat err "Average frequency too high for sample rate.\n"))
  (when (> (+ freq (max i-bfreq f-bfreq))(/ *sound-srate* 2.0))(setq err(strcat err "Generated tone contains frequencies too high for sample rate.\n"))))

(if (>(length err)0)
  (format nil "Error!~%~a" err); output error message
  (progn
 ;   (setq LEN (* dur *sound-srate*))
    (setf f-shift-left (pwlr 0(/ i-bfreq 2.0) dur(/ f-bfreq 2.0) 0))
    (setf f-shift-right (pwlr 0(/ i-bfreq -2.0) dur(/ f-bfreq -2.0) 0))
    (stretch dur
      (mult amp 
        (vector
          (fmosc (hz-to-step freq)f-shift-left)
          (fmosc (hz-to-step freq)f-shift-right))))))

