;nyquist plug-in
;version 2
;type generate
;name "Binaural Tones from List..."
;action "Generating binaural tones from list.."
;info "bitonelist.ny by Scott Hendrickson based on code from David R. Sky \nOpen blank stereo track in Audacity before using this plug-in.\nUse this plug-in at your own risk - see warning in text of this plug-in.\nApproximate brainwave frequency ranges: beta 14-21 hz; alpha 7-14 hz; thheta 4-7 hz; delta 0-4 hz\nReleased under terms of the GNU Public License"

;control f "Left channel tone frequency [hz]" real "" 100 50 1000
;control string1 "Beat List Values frequency1 [hz], duration1 [minutes], time change to frequency2 [minutes]" string "" "17.5 0.25 0.25"
;control fade "Fade-in and fade-out times [seconds]" int "" 10 0 120

(setf choice 0)
(setf ratio 1.0)

; function to convert input string into a list
; input string is str, output list is assigned to symname
(defun eval-string-to-list (symname str)
  (let ((form (strcat "(setq " symname " (list " str "))")))    
(eval (read (make-string-input-stream form)))))

; convert string inputs into lists
(eval-string-to-list "beat-list" string1)

; dur - total duration
(setf dur (first (last beat-list)))
;(setf dur 105.0)


; prepend a zero onto beat-list
(push 0 beat-list)


;((and (= choice 0) (= ratio 1.0)) ; tones without surf
; apply fade-in and fade-out envelope
(mult (pwl 0 0 fade 1.0 (- dur fade) 1.0 dur 0 dur)
(vector
(osc (hz-to-step f) dur *sine-table*)
(fmosc(hz-to-step f) (pwl-list beat-list))
) ; end tones vector
) ; end mult pwl
) ; end tones without surf