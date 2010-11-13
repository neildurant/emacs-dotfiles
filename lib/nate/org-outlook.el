;;; org-outlook.el - Support for links to Outlook items in Org
(require 'org)

(org-add-link-type "outlook" 'org-outlook-open)

(defun org-outlook-open (id)
   "Open the Outlook item identified by ID.  ID should be an Outlook GUID."
   (w32-shell-execute "open" "outlook" (concat "/select " "outlook:" id)))

(provide 'org-outlook)

;;; org-outlook.el ends here
