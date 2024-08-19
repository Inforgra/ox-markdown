;;; ox-markdown.el --- Alternative ox-md -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(require 'ox-md)

(defcustom ox-markdown-posts-path ""
  "Publish to path."
  :group 'ox-markdown
  :type 'string)

(org-export-define-derived-backend 'md 'html
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :menu-entry
  '(?m "Export to Markdown"
       ((?M "To temporary buffer"
            (lambda (a s v b) (org-md-export-as-markdown a s v)))
        (?m "To file" (lambda (a s v b) (org-md-export-to-markdown a s v)))
        (?p "Publish to file" (lambda (a s v b) (my/org-export-to-markdown a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-md-export-to-markdown t s v)
                (org-open-file (org-md-export-to-markdown nil s v)))))))
  :translate-alist '((bold . org-md-bold)
                     (center-block . org-md--convert-to-html)
         (code . org-md-verbatim)
         (drawer . org-md--identity)
         (dynamic-block . org-md--identity)
         (example-block . org-markdown-example-block)
         (export-block . org-md-export-block)
         (fixed-width . org-md-example-block)
         (headline . org-md-headline)
         (horizontal-rule . org-md-horizontal-rule)
         (inline-src-block . org-md-verbatim)
         (inlinetask . org-md--convert-to-html)
         (inner-template . org-md-inner-template)
         (italic . org-md-italic)

         (item . org-md-item)
         (keyword . org-md-keyword)
                     (latex-environment . org-md-latex-environment)
                     (latex-fragment . org-md-latex-fragment)
         (line-break . org-md-line-break)
         (link . my/org-md-link)
         (node-property . org-md-node-property)
         (paragraph . org-md-paragraph)
         (plain-list . org-md-plain-list)
         (plain-text . org-md-plain-text)
         (property-drawer . org-md-property-drawer)
         (quote-block . org-md-quote-block)
         (section . org-md-section)
         (special-block . org-md--convert-to-html)
         (src-block . org-markdown-src-block)
         (table-cell . org-gfm-table-cell)
         (table-row . org-gfm-table-row)
         (table . org-gfm-table)
         (template . org-md-template)
         (verbatim . org-md-verbatim))
  :options-alist
  '((:md-footnote-format nil nil org-md-footnote-format)
    (:md-footnotes-section nil nil org-md-footnotes-section)
    (:md-headline-style nil nil org-md-headline-style)
    (:md-toplevel-hlevel nil nil org-md-toplevel-hlevel)))

(defun org-markdown-example-block (example-block _contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-export-format-code-default example-block info))

(defun org-markdown-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element into Github Flavored Markdown format.
_CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "```" lang "\n"))
         (suffix "```"))
    (concat prefix code suffix)))

;;; https://github.com/larstvei/ox-gfm/blob/master/ox-gfm.el

(defvar width-cookies nil)
(defvar width-cookies-table nil)

(defconst gfm-table-left-border "|")
(defconst gfm-table-right-border " |")
(defconst gfm-table-separator " |")

(defun org-gfm-table-col-width (table column info)
  "Return width of TABLE at given COLUMN.
INFO is a plist used as communication channel.  Width of a column
is determined either by inquerying `width-cookies' in the column,
or by the maximum cell with in the column."
  (let ((cookie (when (hash-table-p width-cookies)
                  (gethash column width-cookies))))
    (if (and (eq table width-cookies-table)
             (not (eq nil cookie)))
        cookie
      (progn
        (unless (and (eq table width-cookies-table)
                     (hash-table-p width-cookies))
          (setq width-cookies (make-hash-table))
          (setq width-cookies-table table))
        (let ((max-width 0)
              (specialp (org-export-table-has-special-column-p table)))
          (org-element-map
              table
              'table-row
            (lambda (row)
              (setq max-width
                    (max (length
                          (org-export-data
                           (org-element-contents
                            (elt (if specialp (car (org-element-contents row))
                                   (org-element-contents row))
                                 column))
                           info))
                         max-width)))
            info)
          (puthash column max-width width-cookies))))))

(defun org-gfm-make-hline-builder (table info char)
  "Return a function to build horizontal line in TABLE with given CHAR.
INFO is a plist used as a communication channel."
  (lambda (col)
    (let ((max-width (max 3 (org-gfm-table-col-width table col info))))
      (when (< max-width 1)
        (setq max-width 1))
      (make-string max-width char))))

(defun org-gfm-table-cell (table-cell contents info)
  "Transcode TABLE-CELL element from Org into GFM.
CONTENTS is content of the cell.  INFO is a plist used as a
communication channel."
  (let* ((table (org-export-get-parent-table table-cell))
         (column (cdr (org-export-table-cell-address table-cell info)))
         (width (org-gfm-table-col-width table column info))
         (left-border (if (org-export-table-cell-starts-colgroup-p table-cell info) "| " " "))
         (right-border " |")
         (data (or contents "")))
    (setq contents
          (concat data
                  (make-string (max 0 (- width (string-width data)))
                               ?\s)))
    (concat left-border contents right-border)))

(defun org-gfm-table-row (table-row contents info)
  "Transcode TABLE-ROW element from Org into GFM.
CONTENTS is cell contents of TABLE-ROW.  INFO is a plist used as a
communication channel."
  (let ((table (org-export-get-parent-table table-row)))
    (when (and (eq 'rule (org-element-property :type table-row))
               ;; In GFM, rule is valid only at second row.
               (eq 1 (cl-position
                      table-row
                      (org-element-map table 'table-row 'identity info))))
      (let* ((table (org-export-get-parent-table table-row))
             (build-rule (org-gfm-make-hline-builder table info ?-))
             (cols (cdr (org-export-table-dimensions table info))))
        (setq contents
              (concat gfm-table-left-border
                      (mapconcat (lambda (col) (funcall build-rule col))
                                 (number-sequence 0 (- cols 1))
                                 gfm-table-separator)
                      gfm-table-right-border))))
    contents))

(defun org-gfm-table (table contents info)
  "Transcode TABLE element into Github Flavored Markdown table.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let* ((rows (org-element-map table 'table-row 'identity info))
         (no-header (or (<= (length rows) 1)))
         (cols (cdr (org-export-table-dimensions table info)))
         (build-dummy-header
          (lambda ()
            (let ((build-empty-cell (org-gfm-make-hline-builder table info ?\s))
                  (build-rule (org-gfm-make-hline-builder table info ?-))
                  (columns (number-sequence 0 (- cols 1))))
              (concat gfm-table-left-border
                      (mapconcat (lambda (col) (funcall build-empty-cell col))
                                 columns
                                 gfm-table-separator)
                      gfm-table-right-border "\n" gfm-table-left-border
                      (mapconcat (lambda (col) (funcall build-rule col))
                                 columns
                                 gfm-table-separator)
                      gfm-table-right-border "\n")))))
    (concat (and no-header (funcall build-dummy-header))
            (replace-regexp-in-string "\n\n" "\n" contents))))

;;;###autoload
(defun my/org-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let* ((filename (file-name-base (buffer-file-name)))
         (in-dir (file-name-directory (buffer-file-name)))
         (pub-dir (concat (file-name-as-directory ox-markdown-posts-path) filename "/"))
         (outfile (concat pub-dir filename ".md")))
    (unless (file-exists-p pub-dir)
      (make-directory pub-dir))
    (my/copy-files in-dir pub-dir)
    (org-export-to-file 'md outfile async subtreep visible-only)))

(defun my/copy-files (src-dir dst-dir)
  "SRC-DIR DST-DIR."
  (seq-do (lambda (file)
            (message "%s%s %s" src-dir file dst-dir)
            (unless (file-exists-p (concat dst-dir file))
              (copy-file (concat src-dir file) dst-dir)))
          (seq-filter (lambda (file) (seq-contains-p '("png" "jpg" "gif" ) (file-name-extension file)))
                      (directory-files src-dir))))


(defun my/org-md-link (link desc info)
  "Transcode LINK object into Markdown format.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
       (let* ((link-org-files-as-md
               (lambda (raw-path)
                 ;; Treat links to `file.org' as links to `file.md'.
                 (if (string= ".org" (downcase (file-name-extension raw-path ".")))
                     (concat (file-name-sans-extension raw-path) ".md")
                   raw-path)))
              (type (org-element-property :type link))
              (raw-path (org-element-property :path link))
              (path (cond
                     ((member type '("http" "https" "ftp" "mailto"))
                      (concat type ":" raw-path))
                     ((string-equal  type "file")
                      (org-export-file-uri (funcall link-org-files-as-md raw-path)))
                     (t raw-path))))
         (cond
          ;; Link type is handled by a special function.
          ((org-export-custom-protocol-maybe link desc 'md info))
          ((member type '("custom-id" "id" "fuzzy"))
           (let ((destination (if (string= type "fuzzy")
                                  (org-export-resolve-fuzzy-link link info)
                                (org-export-resolve-id-link link info))))
             (pcase (org-element-type destination)
               (`plain-text			; External file.
                (let ((path (funcall link-org-files-as-md destination)))
                  (if (not desc) (format "<%s>" path)
                    (format "[%s](%s)" desc path))))
               (`headline
                (format
                 "[%s](#%s)"
                 ;; Description.
                 (cond ((org-string-nw-p desc))
                       ((org-export-numbered-headline-p destination info)
                        (mapconcat #'number-to-string
                                   (org-export-get-headline-number destination info)
                                   "."))
                       (t (org-export-data (org-element-property :title destination)
                                           info)))
                 ;; Reference.
                 (or (org-element-property :CUSTOM_ID destination)
                     (org-export-get-reference destination info))))
               (_
                (let ((description
                       (or (org-string-nw-p desc)
                           (let ((number (org-export-get-ordinal destination info)))
                             (cond
                              ((not number) nil)
                              ((atom number) (number-to-string number))
                              (t (mapconcat #'number-to-string number ".")))))))
                  (when description
                    (format "[%s](#%s)"
                            description
                            (org-export-get-reference destination info))))))))
          ((org-export-inline-image-p link org-html-inline-image-rules)
           (let ((path (cond ((not (string-equal type "file"))
                              (concat type ":" raw-path))
                             ((not (file-name-absolute-p raw-path)) raw-path)
                             (t (expand-file-name raw-path))))
                 (caption (org-export-data
                           (org-export-get-caption
                            (org-export-get-parent-element link))
                           info)))
             (format "![%s](%s)" caption path)))
          ((string= type "coderef")
           (format (org-export-get-coderef-format path desc)
                   (org-export-resolve-coderef path info)))
          ((string= type "radio")
           (let ((destination (org-export-resolve-radio-link link info)))
             (if (not destination) desc
               (format "<a href=\"#%s\">%s</a>"
                       (org-export-get-reference destination info)
                       desc))))
          (t (if (not desc) (format "<%s>" path)
               (format "[%s](%s)" desc path))))))

(provide 'ox-markdown)
;;; ox-markdown.el ends here
