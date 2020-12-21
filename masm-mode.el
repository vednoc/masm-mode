;;; masm-mode.el --- Major mode for editing Mindustry Assembly -*- lexical-binding: t; -*-

;; Copyright (C) 2020 vednoc

;; Author: vednoc <https://github.com/vednoc>
;; Maintainer: vednoc <vednoc@protonmail.com>
;; Homepage: https://github.com/vednoc/masm-mode
;; Created: December 21, 2020
;; Modified: December 21, 2020
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides syntax highlighting for Mindustry Assembly.
;; Docs: https://mindustrygame.github.io/wiki/logic/0-introduction/

;;; Code:

(defvar masm-keywords
  (let* ((types '("set" "op" "noop"))
         (keywords '("drawflush" "printflush" "getlink" "control" "radar" "sensor"))
         (functions '("read" "write" "draw" "print" "jump" "end"))
         (keywords-regexp (regexp-opt keywords 'words))
         (types-regexp (regexp-opt types 'words))
         (functions-regexp (regexp-opt functions 'words)))
    `((,functions-regexp . font-lock-function-name-face)
      (,keywords-regexp . font-lock-keyword-face)
      (,types-regexp . font-lock-type-face))))

;;;###autoload
(define-derived-mode masm-mode fundamental-mode "MASM"
  "Major mode for editing Mindustry Assembly."
  ;; (setq font-lock-defaults 'masm-keywords))
  (setq-local font-lock-defaults '((masm-keywords))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.masm\\'" . masm-mode))

(provide 'masm-mode)
;;; masm-mode.el ends here
