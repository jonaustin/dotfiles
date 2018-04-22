;nyquist plug-in
;version 3
;type generate
;categories "http://lv2plug.in/ns/lv2core#GeneratorPlugin"
;name "Tone Generator..."
;action "Generating..."
;info "by Steve Daulton (www.easyspacepro.com)\nReleased under terms of GPLv2.\n\nAvailable variables for Custom Oscillators:\namp (0 to 1), hz, duration, var.\nSee 'Help' for more information."

;; tone-generator.ny Steve Daulton. October 2010.
;; Released under terms of the GNU General Public License version 2 
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; http://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


;control help "View Help? " choice "No,Plug-in Help,Custom Oscillator Help,Advanced Examples" 0
;control type "Waveform" choice "Sine,Square,Triangle,Saw,White Noise,Silence,Custom" 0
;control custom-osc "Custom Oscillator" string "Nyquist code" "(hzosc hz)"
;control ampscale "Amplitude Scale" choice "dB,Linear (0-1),%" 0
;control amp "Amplitude" string "" 0
;control hz "Frequency" real "Hz" 440 0 10000
;control duration "Duration" real "seconds" 1 0 60
;control var "Custom Variable" real "var" 100 0 1000

(defun help ()
  (format nil
"TONE GENERATOR HELP. ~%
When the amplitude is set in dB;
lower values produce lower amplitude.
Valid range: any negative number.~%
The linear amplitude scale is in the range 0 to 1.
Valid range: 0.0 (silence) to 1.0~%
The % amplitude scale is in the range 0 to 100
Valid range: 0% (silence) to 100%.~%
Frequencies greater than 10000 Hz may be entered
by typing, but must be less than half the sample
rate.~%
Sounds longer than 60 seconds may be generated
by typing a longer 'Duration', but be aware that 
generating very long sounds may be slow.~%
The 'Custom Oscillator' text input and 'Custom 
Variable' slider are ignored unless 'Custom' is
selected in the 'Waveform' drop-down list.~%
Use 'Debug' to view additional error messages."))

(defun custom ()
  (format nil
"Custom tone generators may be created by selecting
'Custom' in the 'Waveform' drop-down, and typing
suitable Nyquist code in the Custom Oscillator text
box.~%
The 'Frequency' slider sets the variable 'hz'.
'Amplitude' sets the variable 'amp' as a linear value.
'Duration' sets the variable 'duration' in seconds.~%
Custom Oscillators should be written to generate
a sound of duration 1 and amplitude 1.
The sound will then be stretched to 'Duration' and
scaled by 'Amplification'.~%
The 'Custom Variable' control allows an additional
variable 'var' to be used in Custom Oscillators.~%
SIMPLE EXAMPLES:
Hint - use copy/paste to try these examples.~%
Sine Wave - code below:
(hzosc hz)~%
Pluck - code below:
(pluck (hz-to-step hz))"))

(defun examples ()
  (format nil
"ADVANCED EXAMPLES:~%
Three Harmonics:
(scale 0.4 (sim (hzosc hz)(hzosc (/ hz 2.0))(hzosc (/ hz 3.0))))~%
Narrow Band Noise:
(mult 4.0 (sine (hz-to-step hz))(lowpass4 (noise) var))~%
Reverse Saw Wave:
(setf *wave* (abs-env (list (pwlv 1 1 -1)(hz-to-step 1) T)))(hzosc hz *wave*)~%
Smooth Fade In/Out:
(mult (hzosc hz)(sum 0.5 (mult 0.5 (lfo (/ duration) 1.0 *sine-table* -90))))~%
10Hz Modulated Tone:
(mult (hzosc hz)(sum 0.5 (mult 0.5 (lfo 10 1.0 *sine-table* -90))))~%
Comb Filtered Noise:
(setq reson 2)(scale 0.05 (comb (sim (noise 0.2)(s-rest 1)) reson var))~%
Buzz:
(let ((harmnx 20)(mod (const var 1)))(buzz harmnx (hz-to-step hz) mod))"))

(setq err "") ; initialise error message
(if (> help 0)
    (format T "To generate tone, set 'View Help' to 'No'.~%")
    (if (= type 6)
        (format T "~a~%" custom-osc))) ; output string to debug window

;; error check frequency
(if (> hz (/ *sound-srate* 2.0))
   (setq err (format nil "~a~%~a Hz is too high for current sample rate.~%" err hz))
   (if (< hz 0)
       (setq err (strcat err "\nFrequency must be greater than zero Hz.\n"))))
      
;; error check duration
(if (< duration 0)
    (setq err (strcat err "\nDuration must be positive.\n")))

;; convert the string "amp" to a numerical value
(setq amp (read (make-string-input-stream amp)))
;; error check
(if (not (numberp amp))
    (setq err (strcat err "\nAmplification must be a number.\n")))

;; function to evaluate custom tone
(defun wavegen (string)
  (setq string (strcat "(progn " string "\n)"))
  (eval (read (make-string-input-stream string))))

;; error check - will pick up 'some' custom oscillator errors
(if (and (= (length err) 0)(= type 6)(not (soundp (wavegen custom-osc))))
    (setq err (strcat err "\nCustom Oscillator does not return sound.\n")))

;; error check amplification amount and convert to linear
(case ampscale
  (0 (if (> amp 0) ; dB
         (setq err (strcat err "\nAmplification must be less than 0 dB.\n"))
         (setq amp (db-to-linear amp))))
  (1 (if (> amp 1.0) ; linear
         (setq err (strcat err "\nAmplification must be less than 1 (linear scale).\n"))
         (if (< amp 0)
             (setq err (strcat err "\nAmplification must be greater than 0 (linear scale).\n")))))
  (T (if (> amp 100.0) ; percent
         (setq err (strcat err "\nAmplification must be less than 100%.\n"))
         (if (< amp 0)
             (setq err (strcat err "\nAmplification must be greater than 0%.\n"))
             (setq amp (/ amp 100.0))))))

;; Generate tone
(defun tone (wave dur osc-str)
  (stretch dur
    (case wave
      (0 (hzosc hz))
      (1 (osc-pulse hz 0))
      (2 (osc-tri hz))
      (3 (osc-saw hz))
      (4 (noise))
      (5 (s-rest 1))
      (T (wavegen osc-str)))))

(case help
   (1 (help))
   (2 (custom))
   (3 (examples))
   (T (if (= (length err) 0)
          ;; if no errors, amplify tone
          (scale amp (tone type duration custom-osc))
          ;; else output error(s)
          (format NIL "ERROR.~%~a" err))))