(setq rd/skeleton-42-header "\
/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*                                                      :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By:                                            +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created:                                          #+#    #+#             */
/*   Updated:                                         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */
")

(setq rd/username "aalves")

(defun rd/header-file-name ()
  "Return filename processed for header."
  (let ((filename (file-name-nondirectory (buffer-file-name)))
	(max-size 41))
    (if (> (string-width filename) max-size)
	(concat
	 (substring filename 0 (- max-size 3))
	 "...")
      filename)))

(defun rd/header-author-line (name)
  "Return a author line with name as (NAME)."
  (concat rd/username " <" rd/username "@student.42.fr>"))

(defun rd/header-date-line (name)
  "Return a date line with name as (NAME)."
  (concat (time-stamp-string "%:y/%02m/%02d %02H:%02M:%02S") " by " rd/username))

(defun rd/insert-over (str)
  "Insert a string as (STR) in overwrite fashion."
  (insert str)
  (delete-char (string-width str)))

(defun rd/header-42 ()
  "Return full header ready to be inserted."
  (let ((filename (rd/header-file-name)))
    (with-temp-buffer
      (insert rd/skeleton-42-header)
      (goto-char 249)
      (rd/insert-over filename)
      (goto-char 415)
      (rd/insert-over (rd/header-author-line rd/username))
      (goto-char 582)
      (rd/insert-over (rd/header-date-line rd/username))
      (goto-char 663)
      (rd/insert-over (rd/header-date-line rd/username))
      (buffer-string))))

(defun rd/set-header-42 ()
  "Set a up to date header in the file."
  (interactive)
  (save-excursion
    (if (buffer-modified-p)
	(progn
	  (goto-char (point-min))
	  (if (search-forward "Updated:" nil t) ;Replace this with a regex for more accuracy
	      (progn
		(forward-char 1)
		(rd/insert-over (rd/header-date-line rd/username))
		(message "Header up to date."))
	    (progn
	      (insert (rd/header-42))
	      (message "New header inserted.")))))))

(provide 'header-42)
;;; header-42 ends here
