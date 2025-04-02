;;; org-teaching-scheduler.el --- Generate class schedules in Org mode  -*- lexical-binding: t; -*-

;; Author: Pablo Cobelli <pablo.cobelli@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: org, teaching, schedule
;; URL: https://github.com/pablocobelli/org-teaching-scheduler

;;; Commentary:
;;
;; This package provides functions to generate class schedules in Org mode.
;; It allows users to specify teaching days and automatically create tables
;; with class dates, taking into account holidays from an academic calendar.
;;
;; Usage:
;;   (require 'org-teaching-scheduler)
;;   M-x generate-class-schedule-table
;;
;;; Code:

(defcustom academic-calendar-org-file nil
  "Path to the Org file containing academic holidays."
  :type 'string
  :group 'org-teaching-scheduler)

(defun extract-heading-before-parenthesis (heading)
  "Return HEADING without any text after the first parenthesis, trimming trailing spaces."
  (if (string-match "^\\([^(\n]+\\)" heading)
      (string-trim-right (match-string 1 heading))
    heading))

(defun check-holiday-for-date (date)
  "Check if DATE appears under the 'FERIADOS' heading in the academic calendar.
If found, return the subheading describing the holiday; otherwise, return nil."
  (let ((found nil)
        (subheading nil))
    (with-current-buffer (find-file-noselect academic-calendar-org-file)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\*+ FERIADOS" nil t)
          (org-map-entries
           (lambda ()
             (save-excursion
               (when (search-forward date nil t)
                 (setq found t)
                 (setq subheading (extract-heading-before-parenthesis
                                   (substring-no-properties (org-get-heading t t)))))))
           nil 'tree))))
    (when found subheading)))

(defun translate-time-to-spanish (english-name)
  "Translate English weekday or month name to Spanish."
  (let ((translation-table '(("Monday" . "Lunes")    ("Tuesday" . "Martes")
                             ("Wednesday" . "Miércoles") ("Thursday" . "Jueves")
                             ("Friday" . "Viernes")   ("Saturday" . "Sábado")
                             ("Sunday" . "Domingo")
                             ("January" . "Enero")   ("February" . "Febrero")
                             ("March" . "Marzo")     ("April" . "Abril")
                             ("May" . "Mayo")        ("June" . "Junio")
                             ("July" . "Julio")      ("August" . "Agosto")
                             ("September" . "Septiembre") ("October" . "Octubre")
                             ("November" . "Noviembre") ("December" . "Diciembre"))))
    (or (cdr (assoc english-name translation-table)) english-name)))

;;;###autoload
(defun generate-class-schedule-table (weekdays start-date end-date)
  "Generate an Org-mode table of class dates based on WEEKDAYS between START-DATE and END-DATE.
The function checks for holidays and adjusts the schedule accordingly."
  (interactive
   (list (split-string (read-string "Días de cursada (e.g., Monday, Wednesday): ") "," t " ")
         (org-read-date nil nil nil "Fecha de inicio de semestre: ")
         (org-read-date nil nil nil "Fecha de fin de semestre: ")))
  (let* ((valid-days (mapcar #'capitalize weekdays))
         (start-date (date-to-time start-date))
         (end-date (date-to-time end-date))
         (current-date start-date)
         (counter 1)
         (last-month "")
         (repeat-month (y-or-n-p "Repetir el nombre del mes en cada línea? (y/n): "))
         (table "#+NAME: cronograma\n| Clase # | Mes | Fecha | Día | Tema |\n|-\n"))
    (while (time-less-p current-date end-date)
      (let* ((date-str (format-time-string "%Y-%m-%d" current-date))
             (date-list (decode-time current-date))
             (day-of-week (capitalize (format-time-string "%A" current-date)))
             (month (format-time-string "%B" current-date))
             (day (nth 3 date-list))
             (month-to-show (if (or repeat-month (not (string= month last-month))) month "")))
        (when (member day-of-week valid-days)
          (let ((holiday (check-holiday-for-date date-str)))
            (if holiday
                (setq table (concat table (format "|   | %s | %d | %s | %s |\n"
                                                  (translate-time-to-spanish month-to-show)
                                                  day
                                                  (translate-time-to-spanish day-of-week)
                                                  holiday)))
              (setq table (concat table (format "| %d | %s | %d | %s |  |\n"
                                                counter
                                                (translate-time-to-spanish month-to-show)
                                                day
                                                (translate-time-to-spanish day-of-week))))
              (setq counter (1+ counter))))
          (setq last-month month)))
      (setq current-date (time-add current-date (days-to-time 1))))
    (insert table)
    (org-table-align)))

(provide 'org-teaching-scheduler)
;;; org-teaching-scheduler.el ends here
