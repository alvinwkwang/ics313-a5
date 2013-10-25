;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name:Alvin Wang, Brent Yoshida, Taylor Kennedy   Date: 10/23/13
;;; Course: ICS313        Assignment: 5 
;;; File: alvinw5.lisp


;;global constant containing name
(defparameter +ID+ "Alvin Wang, Brent Yoshida, and Taylor Kennedy")

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
  (terpri)
  (princ "General commands:")
  (terpri)
  (princ "look - Gives a description of your surroundings.")
  (terpri)
  (princ "walk <direction> - Moves your character to new area of given direction")
  (terpri)
  (princ "pickup <object> - Picks up an object in the area and puts it in your inventory.")
  (terpri)
  (princ "inventory - Shows what is currently objects you are currently carrying")
  (terpri)
  (princ "help/h/? - Opens the help and hints menu.")
  (terpri)
  (princ "quit - Quit game.")
  (terpri)
  (terpri)
  (princ "Special commands:")
  (terpri)
  (princ "weld <object> <object> - Welds one object to another.")
  (terpri)
  (princ "dunk <object> <object> - Dunk an object into another.")
  (terpri)
  (princ "splash <object> <object> - Splash an object onto another.")
  (terpri)
  (princ "forge <object> <object> <object> <object> - Forge four objects into one.")
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

(defmacro game-action2 (command obj obj2 obj3 obj4 place &body body) ; Macro for the sword forge.
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

(defparameter *sword-forged* nil) ; sets sword-forged to not forged.

(game-action2 forge sword-pommel sword-blade sword-guard sword-grip attic
 (if (and (have 'sword-pommel) (have 'sword-blade) (have 'sword-guard)
          (have 'sword-grip) (not *sword-forged*)); check if all items are present and sword not already forged.
                 (progn (setf *sword-forged* 't)
                        '(the sword is now forged.))
               '(you do not have all the items needed.)))

(defmacro new-location (location location-description) ; adds new location.
    `(if (and (listp ',location-description)
              (not (assoc ',location *nodes*))); checks to see if is a list and location does not already exist
       (pushnew '(,location ,location-description) *nodes*))) ; adds location to nodes.

(defmacro new-object (obj location)
    `(if (assoc ',location *nodes*); check if location exists.
       (progn
         (pushnew ',obj *objects*) ; adds object to list of objects.
         (pushnew '(,obj ,location) *object-locations*)))); adds object to list of object locations.

(defmacro new-path (location1 direction path-type location2 &optional direction2 path-type2)
  `(if
     (and 
       (assoc ',location1 *nodes*) ; checks to see if location1 already exists.
       (assoc ',location2 *nodes*)) ;checks to see if location2 already exists.
     (progn
       (cond
         ((and ',direction2 ; checks to see if optionals are filled in.
               ',path-type2)
          (pushnew '(,location1 (,location2 ,direction ,path-type)) *edges*) ; if optionals are filled in adds first edge
          (pushnew '(,location2 (,location1 ,direction2 ,path-type2)) *edges*)) ; if optionals are filled in adds second edge
        (t
          (pushnew '(,location1 (,location2 ,direction ,path-type)) *edges*)))))); if optionals aren't filled in adds edge



(new-location cave (You are in a cave. It's kind of dark in here wish I had an HM05.))
(new-object donkey cave)
(new-path garden right hole cave left hole)


