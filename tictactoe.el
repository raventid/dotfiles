;;; ~/.doom.d/tictactoe.el -*- lexical-binding: t; -*-

(defun tictactoe ()
  "Start playng tictactoe"
  (interactive)
  (switch-to-buffer "tictactoe")
  (tictactoe-mode)
  (tictactoe-init))

(define-derived-mode tictactoe-mode special-mode "tic-tac-toe"
  (define-key tictactoe-mode-map (kbd "SPC") 'tictactoe-mark)
  )

(map! :leader :desc "This is unfortunate, `s` for `strike!`" "s" 'tictactoe-mark)

(defun tictactoe-init ()
  "Start a new game"
  (setq *tictactoe-board* (make-vector (* *tictactoe-size* *tictactoe-size*) ?\.))
  (setq *tictactoe-current-player* ?\X)
  (tictactoe-print-board))

(defvar *tictactoe-board* nil
  "The board itself.")

(defconst *tictactoe-size* 3
  "the size of the board")

(defun tictactoe-print-board ()
  "Prints the board to the screen"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *tictactoe-size*)
      (dotimes (column *tictactoe-size*)
        (insert (tictactoe-get-square row column)))
      (insert "\n"))))

(defun tictactoe-get-square (row column)
  "Get the value in the (row, column) square"
  (elt *tictactoe-board* (+ column (* row *tictactoe-size*))))

(defun tictactoe-set-square (row column value)
  "Get the value in the (row, column) square"
  (aset *tictactoe-board* (+ column (* row *tictactoe-size*)) value))

(defun tictactoe-mark ()
  "Mark the current square"
  (interactive)
  (let ((row (1- (line-number-at-pos)))
        (column (current-column)))
    (tictactoe-set-square row column *tictactoe-current-player*))
  (tictactoe-print-board)
  (tictactoe-swap-players))

(defvar *tictactoe-current-player* nil
  "Current player character")

(defun tictactoe-swap-players ()
  "Swap players, so no it's O or X turn"
  (setq *tictactoe-current-player* (if (char-equal *tictactoe-current-player* ?\X)
                                       ?\O
                                       ?\X)))


(defun prize1 (expected actual)
  (cond ((guessed-score? expected actual) 2)
        ((guessed-winner-or-draw? expected actual) 1)
        ((did-not-guess) 0)))

(defun did-not-guess () t)

(defun guessed-score? (expected actual)
  (equal expected actual))

(defun guessed-winner-or-draw? (expected actual)
  (equal (game-result expected) (game-result actual)))

(defun game-result (game)
  (let* ((splited (split-string game "\\:"))
         (left-team (f splited))
         (right-team (s splited)))
    (cond ((equal left-team right-team) "draw")
          ((string< left-team right-team) "right-team-won")
          ((string> left-team right-team) "left-team-won"))))

(if (equal (prize1 "2:1" "2:1") 2) (message "Winner"))
(if (equal (prize1 "2:1" "3:2") 1) (message "Not bad"))
(if (equal (prize1 "1:1" "2:2") 1) (message "Not bad"))
(if (equal (prize1 "1:4" "3:5") 1) (message "Not bad"))
(if (equal (prize1 "2:1" "0:0") 0) (message "Failure"))


(defun prize (expected actual)
  (if (equal expected actual)
      2
    (if (or (guessed-withdraw? expected actual)
            (guessed-winner? expected actual))
        1
        0)))

(defun guessed-withdraw? (expected actual)
  (let ((expecteda (split-string expected "\\:"))
        (actuala (split-string actual "\\:")))
    (and
     (equal (f expecteda) (s expecteda))
     (equal (f actuala) (s actuala)))))

(defun guessed-winner? (expected actual)
  (let ((expected (split-string expected "\\:"))
                 (actual (split-string actual "\\:")))
    (or
     (and
      (string> (f expected) (s expected))
      (string> (f actual) (s actual)))

     (and
      (string< (f expected) (s expected))
      (string< (f actual) (s actual))))
    )
  )

(defun f (val)
  (car val)
)

(defun s (val)
  (car (cdr val))
)

(if (equal (prize "2:1" "2:1") 2) (message "Winner"))
(if (equal (prize "2:1" "3:2") 1) (message "Not bad"))
(if (equal (prize "1:1" "2:2") 1) (message "Not bad"))
(if (equal (prize "1:4" "3:5") 1) (message "Not bad"))
(if (equal (prize "2:1" "0:0") 0) (message "Failure"))
