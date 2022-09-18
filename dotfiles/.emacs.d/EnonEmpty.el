;; 2022-04-27

;; open the files of command args, if they are not empty

;; save this file as do.el
;; run it in terminal like this:
;; emacs --script do.el file1 file2 etc
;; written by Xah for me

(while argv
  (setq xPath (pop argv))
  (setq xSize (nth 7 (file-attributes xPath)))
  (if (eq 0 xSize)
      nil
    (progn
      (find-file xPath))))
