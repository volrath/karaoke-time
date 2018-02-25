;;; karaoke-mode.el --- Karaoke Mode! because why not? -*- lexical-binding: t; -*-
;; 
;; Filename: karaoke-mode.el
;; Description: Simple Karaoke Mode for .lrc files
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2018 Daniel Barreto
;; Created: Sat Feb 24 08:23:17 2018 (+0100)
;; Version: 0.1.0
;; Package-Requires: ()
;; URL: https://github.com/volrath/karaoke-mode
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; Simple Karaoke Mode for .lrc files that let's you play a song (any sound
;; file) while displaying the lyrics in the .lrc files accordingly.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'seq)
(require 'subr-x)


(defgroup karaoke-time '()
  "Emacs mode for having a good karaoke time."
  :prefix "karaoke-"
  :group 'karaoke-time)


(defvar karaoke-time--buffer-name "*Karaoke Time!*"
  "Name of the Karaoke Time buffer.")


(defvar-local karaoke-time--scheduled-lines nil
  "List of scheduled lines timers.")


(defun karaoke--time-mark-to-seconds (time-mark-str)
  "Transform TIME-MARK-STR to a float amount of seconds."
  (seq-let [minutes seconds] (mapcar #'string-to-number (split-string time-mark-str ":"))
    (+ (* minutes 60)
       seconds)))


(defun karaoke--time-passed (from-time)
  "Calculate the amount of time passed as a float number of seconds from FROM-TIME."
  (- (float-time) from-time))


(defun karaoke--line-time (time-mark from-time)
  "Return a number of seconds (float) from TIME-MARK starting from FROM-TIME."
  (- (karaoke--time-mark-to-seconds time-mark)
     (karaoke--time-passed from-time)))


(defun karaoke--match-next-line! ()
  "Match next line in the .lrc file buffer and return its time mark and line."
  (when (re-search-forward "\\[\\([0-9]\\{2\\}:[0-9]\\{2\\}\.[0-9]\\{2\\}\\)\\]\\(.*\\)$" nil t)
    (forward-char)
    (cons (match-string 1)
          (match-string 2))))


(defun karaoke--show-line! (line)
  "Show LINE in karaoke buffer."
  (when-let (karaoke-time-buffer (get-buffer karaoke-time--buffer-name))
    (when-let (karaoke-time-window (display-buffer karaoke-time-buffer))
      (with-current-buffer karaoke-time-buffer
        (erase-buffer)
        (insert line)
        (insert "\n")
        (with-selected-window karaoke-time-window
          (let* ((window-height (window-body-height karaoke-time-window t))
                 (window-width (/ (window-body-width karaoke-time-window) 3))
                 (content-height (cdr (posn-x-y (posn-at-point))))
                 (padding-top (/ (- window-height content-height) 2))
                 (padding-left (ceiling (/ (- window-width (length line)) 2))))
            (goto-char (point-min))
            (insert (propertize "\n" 'line-height padding-top))
            (indent-to-column padding-left)
            (goto-char (point-max))))))))


(defun karaoke--schedule-next-line! (from-time time-mark line)
  "Schedule LINE at TIME-MARK after FROM-TIME."
  (add-to-list 'karaoke-time--scheduled-lines
               (run-at-time (karaoke--line-time time-mark from-time)
                            nil
                            #'karaoke--show-line! line)))


(defun karaoke--play! (song-filename)
  "Play SONG-FILENAME."
  (message "Start playing %S" song-filename))


(defun karaoke--read-and-schedule-lyrics! (lyrics-buffer)
  "Read and schedule all lines in LYRICS-BUFFER."
  (with-current-buffer lyrics-buffer
    (let (next-line
          (now (float-time)))
      (goto-char (point-min))
      (while (setq next-line (karaoke--match-next-line!))
        (let ((time-mark (car next-line))
              (line (cdr next-line)))
          (karaoke--schedule-next-line! now time-mark line))))))


(defun karaoke-init (song-filename)
  "Ask user for a SONG-FILENAME, and schedule song lyrics from current buffer.
Current buffer is expected to be a .lrc file.  All lines of this file are
schedule right away."
  (interactive "fSong file: ")
  (let ((karaoke-buffer (get-buffer-create karaoke-time--buffer-name))
        (lyrics-buffer (current-buffer)))
    (window-configuration-to-register '_)
    (delete-other-windows)
    (switch-to-buffer karaoke-buffer)
    (karaoke-time-mode)
    (karaoke--play! song-filename)
    (karaoke--read-and-schedule-lyrics! lyrics-buffer)))


(defun karaoke-quit ()
  "Quit karaoke time and stop music."
  (interactive)
  (when-let (karaoke-buffer (get-buffer karaoke-time--buffer-name))
    (kill-buffer karaoke-buffer)
    (mapc #'cancel-timer karaoke-time--scheduled-lines)
    ;; TODO: Stop music
    (jump-to-register '_)))


;; Karaoke Time mode
;; ----------------------------------------------------------------------------

(defvar karaoke-time-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'karaoke-quit)
    ;; TODO: pause
    map))

(defvar karaoke-time-mode-hook nil
  "Hook for `karaoke-time-mode'.")


(define-derived-mode karaoke-time-mode nil "Karaoke Time!"
  "Major mode for enjoying Karaoke Time.

\\{karaoke-time-mode-map}"
  (buffer-face-set 'org-level-1)
  (text-scale-adjust 7)
  (run-hooks 'karaoke-time-mode-hook))


(provide 'karaoke-mode)

;;; karaoke-mode.el ends here
