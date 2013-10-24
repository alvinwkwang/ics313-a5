;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name:Alvin Wang, Brent Yoshida, Taylor Kennedy   Date: 10/23/13
;;; Course: ICS313        Assignment: 5 
;;; File: alvinw5.lisp


;;global constant containing name
(defparameter +ID+ "Alvin Wang")

; wizards_game part 1

;;variables for description of in-game locations
(defparameter *nodes* '((living-room (you are in the living-room.
                            a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))
                        (bedroom (you are in the bedroom. there is a big bed.))))

;;describes location
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

;;variables to show connections between locations
(defparameter *edges* '((living-room (garden west door)  
                                     (attic upstairs ladder)
                                     (bedroom east door))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))
                        (bedroom (living-room west door))))

;;desribes connecting edges
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;;used to describe multiple edges
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;list of visible objects
(defparameter *objects* '(whiskey bucket frog chain book sword-grip sword-blade sword-guard sword-pommel))

;;list of visible objects and where they are located
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (sword-grip living-room)
                                   (sword-blade attic)
                                   (sword-guard garden)
                                   (chain garden)
                                   (frog garden)
                                   (sword-pommel bedroom)
                                   (book bedroom)))

;;list objects visible from a given location
(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

;;describe visible objects at a given location
(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;;variable used to track plater's current position, default location at living room
(defparameter *location* 'living-room)

;;look function calls all description functions: location, paths, and objects
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;;the walk function takes a direction and lets us walk there
(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

;;lets us pick up objects in the world.
(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that.))))

;;shows what objects we are carrying
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun have (object) 
    (member object (cdr (inventory))))

;  wizards_game part 2

;;used to start game and initialize a custom REPL
;(defun game-repl ()
    ;(let ((cmd (game-read)))
        ;(unless (eq (car cmd) 'quit)
            ;(game-print (game-eval cmd))
            ;(game-repl))))

(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
          (cond
            ((or (eq (car cmd) 'help) (eq (car cmd) 'h) (eq (car cmd) '?))
             (help-menu)
             (game-repl))
            (t
            (game-print (game-eval cmd))
            (game-repl))))))

;;displays help-menu
(defun help-menu ()
  (princ "Help and Hints!")
  (terpri)
  (princ "Enter look, pickup (object), walk (direction),inventory")
  (terpri))

;;custom read function that concatenates parenthese around commands and a quote infront of parameters
(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;;variable of allowed commands
(defparameter *allowed-commands* '(look walk pickup inventory))

;;game-eval allows only certain commands to go through
(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

;;tweak-text looks at each character in the list and modifies it as need.
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;;game-print converts symbol-based writing into properly capitalized text.
(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))

(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

(defmacro game-action2 (command obj obj2 obj3 obj4 place &body body) ; NUMBER 2!!!!!!!!!
  `(progn (defun ,command (object object1 object2 object3)
            (if (and (eq *location* ',place)
                     (eq object ',obj)
                     (eq object1 ',obj2)
                     (eq object2 ',obj3)
                     (eq object3 ',obj4)
                     (have ',obj)
                     (have ',obj2)
                     (have ',obj3)
                     (have ',obj4))
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))
(defparameter *chain-welded* nil)

(game-action weld chain bucket attic
             (if (and (have 'bucket) (not *chain-welded*))
                 (progn (setf *chain-welded* 't)
                        '(the chain is now securely welded to the bucket.))
               '(you do not have a bucket.)))

(defparameter *bucket-filled* nil)

(game-action dunk bucket well garden
             (if *chain-welded* 
                 (progn (setf *bucket-filled* 't)
                        '(the bucket is now full of water))
               '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
             (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
                   ((have 'frog) '(the wizard awakens and sees that you stole his frog. 
                                   he is so upset he banishes you to the 
                                   netherworlds- you lose! the end.))
                   (t '(the wizard awakens from his slumber and greets you warmly. 
                        he hands you the magic low-carb donut- you win! the end.))))

(defparameter *sword-welded* nil)

(game-action2 weld2 sword-pommel sword-blade sword-guard sword-grip attic
 (if (and (have 'sword-pommel) (have 'sword-blade) (have 'sword-guard)
          (have 'sword-grip) (not *sword-welded*))
                 (progn (setf *sword-welded* 't)
                        '(the sword is now welded.))
               '(you do not have all the items needed.)))
