;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname adventure) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(require "adventure-define-struct.rkt")
(require "macros.rkt")
(require "utilities.rkt")

;;;
;;; OBJECT
;;; Base type for all in-game objects
;;;

(define-struct object
  ;; adjectives: (listof string)
  ;; List of adjectives to be printed in the description of this object
  (adjectives)
  
  #:methods
  ;; noun: object -> string
  ;; Returns the noun to use to describe this object.
  (define (noun o)
    (type-name-string o))

  ;; description-word-list: object -> (listof string)
  ;; The description of the object as a list of individual
  ;; words, e.g. '("a" "red" "door").
  (define (description-word-list o)
    (add-a-or-an (append (object-adjectives o)
                         (list (noun o)))))
  ;; description: object -> string
  ;; Generates a description of the object as a noun phrase, e.g. "a red door".
  (define (description o)
    (words->string (description-word-list o)))
  
  ;; print-description: object -> void
  ;; EFFECT: Prints the description of the object.
  (define (print-description o)
    (begin (printf (description o))
           (newline)
           (void))))

;;;
;;; CONTAINER
;;; Base type for all game objects that can hold things
;;;

(define-struct (container object)
  ;; contents: (listof thing)
  ;; List of things presently in this container
  (contents)
  
  #:methods
  ;; container-accessible-contents: container -> (listof thing)
  ;; Returns the objects from the container that would be accessible to the player.
  ;; By default, this is all the objects.  But if you want to implement locked boxes,
  ;; rooms without light, etc., you can redefine this to withhold the contents under
  ;; whatever conditions you like.
  (define (container-accessible-contents c)
    (container-contents c))
  
  ;; prepare-to-remove!: container thing -> void
  ;; Called by move when preparing to move thing out of
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-remove! container thing)
    (void))
  
  ;; prepare-to-add!: container thing -> void
  ;; Called by move when preparing to move thing into
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-add! container thing)
    (void))
  
  ;; remove!: container thing -> void
  ;; EFFECT: removes the thing from the container
  (define (remove! container thing)
    (set-container-contents! container
                             (remove thing
                                     (container-contents container))))
  
  ;; add!: container thing -> void
  ;; EFFECT: adds the thing to the container.  Does not update the thing's location.
  (define (add! container thing)
    (set-container-contents! container
                             (cons thing
                                   (container-contents container))))

  ;; describe-contents: container -> void
  ;; EFFECT: prints the contents of the container
  (define (describe-contents container)
    (begin (local [(define other-stuff (remove me (container-accessible-contents container)))]
             (if (empty? other-stuff)
                 (printf "There's nothing here.~%")
                 (begin (printf "You see:~%")
                        (for-each print-description other-stuff))))
           (void))))

;; move!: thing container -> void
;; Moves thing from its previous location to container.
;; EFFECT: updates location field of thing and contents
;; fields of both the new and old containers.
(define (move! thing new-container)
  (begin
    (prepare-to-remove! (thing-location thing)
                        thing)
    (prepare-to-add! new-container thing)
    (prepare-to-move! thing new-container)
    (remove! (thing-location thing)
             thing)
    (add! new-container thing)
    (set-thing-location! thing new-container)))

;; destroy!: thing -> void
;; EFFECT: removes thing from the game completely.
(define (destroy! thing)
  ; We just remove it from its current location
  ; without adding it anyplace else.
  (remove! (thing-location thing)
           thing))

;;;
;;; ROOM
;;; Base type for rooms and outdoor areas
;;;

(define-struct (room container)
  ())

;; new-room: string -> room
;; Makes a new room with the specified adjectives
(define (new-room adjectives)
  (make-room (string->words adjectives)
             '()))



;;;
;;; THING
;;; Base type for all physical objects that can be inside other objects such as rooms
;;;

(define-struct (thing container)
  ;; location: container
  ;; What room or other container this thing is presently located in.
  (location)
  
  #:methods
  (define (examine thing)
    (print-description thing))

  ;; prepare-to-move!: thing container -> void
  ;; Called by move when preparing to move thing into
  ;; container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-move! container thing)
    (void)))

;; initialize-thing!: thing -> void
;; EFFECT: adds thing to its initial location
(define (initialize-thing! thing)
  (add! (thing-location thing)
        thing))

;; new-thing: string container -> thing
;; Makes a new thing with the specified adjectives, in the specified location,
;; and initializes it.
(define (new-thing adjectives location)
  (local [(define thing (make-thing (string->words adjectives)
                                    '() location))]
    (begin (initialize-thing! thing)
           thing)))

;;;
;;; DOOR
;;; A portal from one room to another
;;; To join two rooms, you need two door objects, one in each room
;;;

(define-struct (door thing)
  ;; destination: container
  ;; The place this door leads to
  (destination)
  
  #:methods
  ;; go: door -> void
  ;; EFFECT: Moves the player to the door's location and (look)s around.
  (define (go door)
    (begin (move! me (door-destination door))
           (look))))

;; join: room string room string
;; EFFECT: makes a pair of doors with the specified adjectives
;; connecting the specified rooms.
(define (join! room1 adjectives1 room2 adjectives2)
  (local [(define r1->r2 (make-door (string->words adjectives1)
                                    '() room1 room2))
          (define r2->r1 (make-door (string->words adjectives2)
                                    '() room2 room1))]
    (begin (initialize-thing! r1->r2)
           (initialize-thing! r2->r1)
           (void))))

(define (joinn! room2 adjectives2 room3 adjectives3)
  (local [(define r2->r3 (make-door (string->words adjectives2)
                                    '() room2 room3))
          (define r3->r2 (make-door (string->words adjectives3)
                                    '() room3 room2))]
    (begin (initialize-thing! r2->r3)
           (initialize-thing! r3->r2)
           (void))))

;;;
;;; PERSON
;;; A character in the game.  The player character is a person.
;;;

(define-struct (person thing)
  ())

;; initialize-person: person -> void
;; EFFECT: do whatever initializations are necessary for persons.
(define (initialize-person! p)
  (initialize-thing! p))

;; new-person: string container -> person
;; Makes a new person object and initializes it.
(define (new-person adjectives location)
  (local [(define person
            (make-person (string->words adjectives)
                         '()
                         location))]
    (begin (initialize-person! person)
           person)))

;; This is the global variable that holds the person object representing
;; the player.  This gets reset by (start-game)
(define me empty)

;;;
;;; PROP
;;; A thing in the game that doesn't serve any purpose other than to be there.
;;;

(define-struct (prop thing)
  (;; noun-to-print: string
   ;; The user can set the noun to print in the description so it doesn't just say "prop"
   noun-to-print
   ;; examine-text: string
   ;; Text to print if the player examines this object
   examine-text
   )
  
  #:methods
  (define (noun prop)
    (prop-noun-to-print prop))

  (define (examine prop)
    (display-line (prop-examine-text prop))))

;; new-prop: string container -> prop
;; Makes a new prop with the specified description.
(define (new-prop description examine-text location)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define prop (make-prop (adjectives '() location noun examine-text)))]
    (begin (initialize-thing! prop)
           prop)))

;;;
;;; ADD YOUR TYPES HERE!
;;;

(define-struct (bug thing)
  (species
   size
   roomloc)


  #:methods
  (define (observe bug)
    (if (string=? (bug-species bug) "ladybug")
        (display-line "It is a friedly bug, let it live")
        (display-line "THAT'S A PEST")))

  (define (catch bug)
    (if (string=? (bug-size bug) "big")
        (display-line "It doesn't fit in your pocket")
        (begin (display-line "The bug says 'ouch'")
               (move! bug me))))

  (define (mesmerize bug)
    (if (string=? (bug-roomloc bug) "dark")
        (set-bug-species! bug "ladybug")
        (display-line "There's too much light for the mesmerization"))))
         
        
(define (new-bug adjectives location species size roomloc)
  (local [(define bug
            (make-bug (string->words adjectives)
                            '()
                            location
                            species
                            size
                            roomloc))]
    (begin (initialize-thing! bug)
           bug)))


;;; PESITICIDE
(define-struct (pesticide thing)
  (brand)

  #:methods
  (define (use pesticide)
   (if (string=? (pesticide-brand pesticide) "Talstar pro")
      (begin (destroy! pesticide)
             (destroy! (the bug))
             (display-line "The bug is now dead!"))
      (display-line "This pesticide doesn't work")))


  (define (throw pesticide)
    (if (string=? (pesticide-brand pesticide) "bloom buddy")
        (display-line "This pesticide is too light to kill the weta")
        (begin (destroy! (the black bug))
               (display-line "The weta gets crushed tragically and vanishes")))))


  
(define (new-pesticide adjectives location brand)
  (local [(define pesticide
            (make-pesticide (string->words adjectives)
                            '()
                            location
                            brand))]
    (begin (initialize-thing! pesticide)
           pesticide)))
           
;;;; SEEDS

(define-struct (seed thing)
  (family length)
  
  #:methods
  (define (prepare-to-move! seed thing)
    (error "You cannot move seeds out of their natural habitat"))

  (define (study-seed thing)
    (printf "This seed will bloom into a ~A ~A.\n"
            (seed-length (the seed))
            (seed-family (the seed)))))

(define (new-seed adjectives location family length)
  (local [(define seed
            (make-seed (string->words adjectives)
                            '()
                            location
                            family
                            length))]
    (begin (initialize-thing! seed)
           seed)))


;;;; PLANTS
(define-struct (plant thing)
  (family length)

  #:methods

  (define (study-plant thing)
    (printf "This plant is a ~A ~A.\n"
            (plant-length (the plant))
            (plant-family (the plant))))
  
  (define (prepare-to-move! plant thing)
    (error "You cannot move this plant because the roots are too deep in the soil.")))

(define (new-plant adjectives location family length)
  (local [(define plant
            (make-plant (string->words adjectives)
                            '()
                            location
                            family
                            length))]
    (begin (initialize-thing! plant)
           plant)))

;;; WATER

(define-struct (water thing)
  (source)

  #:methods

  (define (spray thing)
    (if (string=? (water-source thing) "Fresh water")
        (display-line "The plant loves this water and is now growing!")
        (display-line "You are dehydrating the plant, give it fresh water!")))

  (define (spray-seed-w/ thing)
    (if (string=? (water-source thing) "Salt water")
        (display-line "This isn't helping the plant grow")
        (begin (add! (new-plant "" (here) (seed-family (the seed)) (seed-length (the seed)))
                     (display-line "A new plant has bud"))
               (destroy! (the seed))))))
                        
(define (new-water adjectives location source)
  (local [(define water
            (make-water (string->words adjectives)
                            '()
                            location
                            source))]
    (begin (initialize-thing! water)
           water)))


;;; FERTILIZER

(define-struct (fertilizer thing)
  (chemical make)

  #:methods
 (define (check-out thing)
    (if (string=? (fertilizer-chemical thing) "organic")
            (display-line "This fertilizer is organic.")
            (display-line "This fertilzer is inorganic."))) 

  (define (spread thing)
    (if (string=? (fertilizer-chemical thing) "organic")
        (if (string=? (fertilizer-make thing) "Compost")
            (printf "This fertilizer is nourishing the ~A very well!\n"
                    (plant-family (the plant)))
            (printf "This fertilzer isn't the best choice for the ~A.\n"
                    (plant-family (the plant))))
        ((begin
    (destroy! (the plant))
    (error "This killed the plant!"))))))
    


(define (new-fertilizer adjectives location chemical make)
  (local [(define fertilizer
            (make-fertilizer (string->words adjectives)
                            '()
                            location
                            chemical
                            make))]
    (begin (initialize-thing! fertilizer)
           fertilizer)))


;;; POND

(define-struct (pond thing)
  (depth
   state)
    
  #:methods
  
  
  ; add water
  (define (fill pond)
    (if (string=? (pond-depth pond) "shallow")
        (begin
          (set-pond-depth! pond "deep")
          (display-line "the pond is now deep enough to swim in!"))
                  (display-line "You dont need to fill this!")
))

    ; swim
  (define (swim pond)
    (if (string=? (pond-depth pond) "shallow")
        (error "this is not deep enough to swim in. FILL pond")
            (if (string=? (pond-state pond) "infested")
                (error" this pond is infested!You cant swim in here. Clean the pond")
                (display-line "THAT WAS SO FUN!"))))
  ;; clean
  (define (clean pond)
    (if (string=? (pond-state pond) "infested")
        (begin
          (set-pond-state! pond "clean")
          (display-line "YAY you cleaned the pond! Now all the creatures can swim freely!"))
        (if (string=? (pond-state pond) "clean")
            (display-line "this is aleady clean")
            (display-line "you cant clean this")
         
            )))
  ;;drink
  (define (drink pond)
    (if (string=? (pond-state pond) "infested")
        (error "this pond is filthy. You should CLEAN it before you drink water from it")
        (if (string=? (pond-state pond) "clean") 
            (display "mmhm refreshing")
            (display "you shouldnt drink that"))
        )))

(define (new-pond adjectives location size state)
  (local [(define pond (make-pond (string->words adjectives)
                                  '()
                                  location
                                  size
                                  state))]

    (begin (initialize-thing! pond)
           pond)))

(define-struct (bird thing)
  (mood
   type)
    
  #:methods
  ;; hit
  (define (hit bird)
    (if (string=? (bird-mood bird) "kind")
        (error "this is a kind bird. Let it live ")
        (if (string=? (bird-mood bird) "malicious")
            (begin (destroy! bird)
                   (display "Congrats, you killed the malicious vulture"))
                (display "This is not something you can kill")))
    ))
;;;talk
(define (talk bird)
  (if (string=? (bird-type bird) "hummingbird")
      (display "the hummingbird: 'chirp chirp' ")
      (if (string=? (bird-type bird) "parrot")
          (display "the parrot says: 'hi here! squawk!' ")
          (if (string=? (bird-type bird) "Vulture")
          (display "this angry bird does not want to speak to you")
          (display "this does not speak "))
          )))
      
      
      

(define (new-bird adjectives location size type)
  (local [(define bird (make-bird (string->words adjectives)
                                  '()
                                  location
                                  size
                                  type))]

    (begin (initialize-thing! bird)
           bird)))


;;; TREE

(define-struct (tree thing)
  (species size)

  #:methods
  (define (admire tree)
    (if (string=? (tree-species tree) "Redwood")
        (display-line "Hyperion is the name of the tallest Redwood tree in the world.")
        (display-line "This is a normal tree")))
  
  (define (hug tree)
    (if (string=? (tree-species tree) "Redwood")
        (display-line "Unable to hug tree")
        (error "This tree is too big to hug"))))

 
(define (new-tree adjectives location species size)
  (local [(define tree
            (make-tree (string->words adjectives)
                            '()
                            location
                            species
                            size))]
    (begin (initialize-thing! tree)
           tree)))






;;;
;;; USER COMMANDS
;;;

(define (look)
  (begin (printf "You are in ~A.~%"
                 (description (here)))
         (describe-contents (here))
         (void)))

(define-user-command (look) "Prints what you can see in the room")

(define (inventory)
  (if (empty? (my-inventory))
      (printf "You don't have anything.~%")
      (begin (printf "You have:~%")
             (for-each print-description (my-inventory)))))

(define-user-command (inventory)
  "Prints the things you are carrying with you.")

(define-user-command (examine thing)
  "Takes a closer look at the thing")

(define (take thing)
  (move! thing me))

(define-user-command (take thing)
  "Moves thing to your inventory")

(define (drop thing)
  (move! thing (here)))

(define-user-command (drop thing)
  "Removes thing from your inventory and places it in the room
")

(define (put thing container)
  (move! thing container))

(define-user-command (put thing container)
  "Moves the thing from its current location and puts it in the container.")

(define (help)
  (for-each (λ (command-info)
              (begin (display (first command-info))
                     (newline)
                     (display (second command-info))
                     (newline)
                     (newline)))
            (all-user-commands)))

(define-user-command (help)
  "Displays this help information")

(define-user-command (go door)
  "Go through the door to its destination")

(define (check condition)
  (if condition
      (display-line "Check succeeded")
      (error "Check failed!!!")))

(define-user-command (check condition)
  "Throws an exception if condition is false.")

;;;
;;; ADD YOUR COMMANDS HERE!
;;;
(define-user-command (observe bug) 
  "sees if it is a pest")

(define-user-command (catch bug) 
  "puts the bug in the inventory if it's not big")

(define-user-command (mesmerize bug) 
  "change the bug to a ladybug in the dark room")

(define-user-command (use pesticide) 
  "kills the bug")

(define-user-command (throw pesticide) 
  "use any pesticide except bloom buddy to kill the black bug")

(define-user-command (clean)
  "cleans a pond")

(define-user-command (drink)
  "drinks from a pond")

(define-user-command (hit)
  "User smacks a bird")

(define-user-command (talk)
  "user talks to a bird")
  
  (define-user-command (fill pond) "fills the pond with more water")
    
  (define-user-command (swim pond) "user swims in the pond")

(define-user-command (admire tree) "Looks at tree and admires it")

(define-user-command (hug tree) "Tries to hug tree")

(define-user-command (study-seed thing) "tells you what type of plant the seed will bloom into")

(define-user-command (study-plant thing) "tells you what type of plant it is")

(define-user-command (spray thing) "sprays water on the plant and only works on plants")

(define-user-command (spray-seed-w/ thing) "sprays water on the seed causing it to bloom and only works for seeds")

(define-user-command (check-out thing) "tells you whether the fertilizer is organic or inorganic")

(define-user-command (spread thing) "fertilizes the plant, therefore it will only work if there is a plant")

;;;
;;; THE GAME WORLD - FILL ME IN
;;;

;; start-game: -> void
;; Recreate the player object and all the rooms and things.
(define (start-game)
  ;; Fill this in with the rooms you want
  (local [(define starting-room (new-room "bright"))
          (define room2 (new-room "dark"))
          (define room3 (new-room "snowy"))]

    (begin (set! me (new-person "" starting-room))
           ;; Add join commands to connect your rooms with doors
           (join! starting-room "glass"
                  room2 "glass")
           (joinn! room2 "wood"
                   room3 "wood")
           ;; Add code here to add things to your rooms
           (new-pesticide "white" starting-room "Talstar pro")
           (new-pesticide "yellow" starting-room "Raid")
           (new-pesticide "green" starting-room "bloom buddy")
           (new-pesticide "red" room2 "ortho")
           (new-pesticide "green" room2 "bloom buddy")
           (new-pesticide "yellow" room2 "Raid")
           (new-bug "red" starting-room "ladybug" "small" "bright")
           (new-bug "black" room2 "weta" "big" "dark")
           (new-bug "brown" room2 "grasshopper" "big" "dark")
           (new-bug "white" room3 "aphid" "small" "snowy")
           (new-water "clear" starting-room "Fresh water")
           (new-water "cloudy" starting-room "Salt water")
           (new-fertilizer "blue" starting-room "inorganic" "Synthetic")
           (new-fertilizer "brown" starting-room "organic" "Compost")
           (new-fertilizer "red" starting-room "organic" "Blood meal")
           (new-fertilizer "tan" starting-room "organic" "Bone meal")
           (new-seed "green" starting-room "cactus" "big")
           (new-seed "red" room2 "rose" "small")
           (new-seed "pink" room3 "lotus" "small")
           (new-pond "blue" room2 "shallow" "clean")
           (new-pond "green" starting-room "deep" "infested")
           (new-bird "orange" starting-room "kind" "hummingbird")
           (new-bird "black" room2 "malicious" "Vulture")
           (new-bird "yellow" room3 "kind" "parrot")
           (new-tree "green" starting-room "Redwood" "Big")
           (void))))

;;;
;;; PUT YOUR WALKTHROUGHS HERE
;;;

(define-walkthrough win
  (observe (the red bug))
  (take (the white pesticide))
  (catch (the red bug))
  (check-out (the brown fertilizer))
  (check-out (the blue fertilizer))
  (study-seed (the seed))
  (spray-seed-w/ (the cloudy water))
  (spray-seed-w/ (the clear water))
  (study-plant (the plant))
  (spray (the clear water))
  (spray (the cloudy water))
  (spread (the tan fertilizer))
  (spread (the brown fertilizer))
  (take (the red fertilizer))
  (take (the tan fertilizer))
  (take (the brown fertilizer))
  (take (the blue fertilizer))
  (take (the clear water))
  (take (the cloudy water))
  (clean(the green pond))
  (drink (the green pond))
  (talk(the orange bird))
  (hit(the orange bird))
  
  (go (the glass door))
  (observe (the black bug))
  (throw (the green pesticide))
  (throw (the red pesticide))
  (observe (the brown bug))
  (use (the yellow pesticide))
  (mesmerize (the brown bug))
  (observe (the brown bug))
  (catch (the brown bug))
  (drop (the red bug))
  (study-seed (the seed))
  (spray-seed-w/ (the clear water))
  (spray (the clear water))
  (spread (the brown fertilizer))
  (talk(the black bird))
  (hit(the black bird))
  (fill(the blue pond))
  (swim(the blue pond))
  
  (go (the wood door))
  (observe (the white bug))
  (mesmerize (the white bug))
  (drop (the white pesticide))
  (use (the white pesticide))
  (study-seed (the seed))
  (spray-seed-w/ (the clear water))
  (study-plant (the plant))
  (spray (the clear water))
  (spread (the brown fertilizer))
  (talk(the yellow bird)))






;;;
;;; UTILITIES
;;;

;; here: -> container
;; The current room the player is in
(define (here)
  (thing-location me))

;; stuff-here: -> (listof thing)
;; All the stuff in the room the player is in
(define (stuff-here)
  (container-accessible-contents (here)))

;; stuff-here-except-me: -> (listof thing)
;; All the stuff in the room the player is in except the player.
(define (stuff-here-except-me)
  (remove me (stuff-here)))

;; my-inventory: -> (listof thing)
;; List of things in the player's pockets.
(define (my-inventory)
  (container-accessible-contents me))

;; accessible-objects -> (listof thing)
;; All the objects that should be searched by find and the.
(define (accessible-objects)
  (append (stuff-here-except-me)
          (my-inventory)))

;; have?: thing -> boolean
;; True if the thing is in the player's pocket.
(define (have? thing)
  (eq? (thing-location thing)
       me))

;; have-a?: predicate -> boolean
;; True if the player as something satisfying predicate in their pocket.
(define (have-a? predicate)
  (ormap predicate
         (container-accessible-contents me)))

;; find-the: (listof string) -> object
;; Returns the object from (accessible-objects)
;; whose name contains the specified words.
(define (find-the words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (accessible-objects)))

;; find-within: container (listof string) -> object
;; Like find-the, but searches the contents of the container
;; whose name contains the specified words.
(define (find-within container words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (container-accessible-contents container)))

;; find: (object->boolean) (listof thing) -> object
;; Search list for an object matching predicate.
(define (find predicate? list)
  (local [(define matches
            (filter predicate? list))]
    (case (length matches)
      [(0) (error "There's nothing like that here")]
      [(1) (first matches)]
      [else (error "Which one?")])))

;; everything: -> (listof container)
;; Returns all the objects reachable from the player in the game
;; world.  So if you create an object that's in a room the player
;; has no door to, it won't appear in this list.
(define (everything)
  (local [(define all-containers '())
          ; Add container, and then recursively add its contents
          ; and location and/or destination, as appropriate.
          (define (walk container)
            ; Ignore the container if its already in our list
            (unless (member container all-containers)
              (begin (set! all-containers
                           (cons container all-containers))
                     ; Add its contents
                     (for-each walk (container-contents container))
                     ; If it's a door, include its destination
                     (when (door? container)
                       (walk (door-destination container)))
                     ; If  it's a thing, include its location.
                     (when (thing? container)
                       (walk (thing-location container))))))]
    ; Start the recursion with the player
    (begin (walk me)
           all-containers)))

;; print-everything: -> void
;; Prints all the objects in the game.
(define (print-everything)
  (begin (display-line "All objects in the game:")
         (for-each print-description (everything))))

;; every: (container -> boolean) -> (listof container)
;; A list of all the objects from (everything) that satisfy
;; the predicate.
(define (every predicate?)
  (filter predicate? (everything)))

;; print-every: (container -> boolean) -> void
;; Prints all the objects satisfying predicate.
(define (print-every predicate?)
  (for-each print-description (every predicate?)))

;; check-containers: -> void
;; Throw an exception if there is an thing whose location and
;; container disagree with one another.
(define (check-containers!)
  (for-each (λ (container)
              (for-each (λ (thing)
                          (unless (eq? (thing-location thing)
                                       container)
                            (error (description container)
                                   " has "
                                   (description thing)
                                   " in its contents list but "
                                   (description thing)
                                   " has a different location.")))
                        (container-contents container)))
            (everything)))

;; is-a?: object word -> boolean
;; True if word appears in the description of the object
;; or is the name of one of its types
(define (is-a? obj word)
  (let* ((str (if (symbol? word)
                  (symbol->string word)
                  word))
         (probe (name->type-predicate str)))
    (if (eq? probe #f)
        (member str (description-word-list obj))
        (probe obj))))

;; display-line: object -> void
;; EFFECT: prints object using display, and then starts a new line.
(define (display-line what)
  (begin (display what)
         (newline)
         (void)))

;; words->string: (listof string) -> string
;; Converts a list of one-word strings into a single string,
;; e.g. '("a" "red" "door") -> "a red door"
(define (words->string word-list)
  (string-append (first word-list)
                 (apply string-append
                        (map (λ (word)
                               (string-append " " word))
                             (rest word-list)))))

;; string->words: string -> (listof string)
;; Converts a string containing words to a list of the individual
;; words.  Inverse of words->string.
(define (string->words string)
  (string-split string))

;; add-a-or-an: (listof string) -> (listof string)
;; Prefixes a list of words with "a" or "an", depending
;; on whether the first word in the list begins with a
;; vowel.
(define (add-a-or-an word-list)
  (local [(define first-word (first word-list))
          (define first-char (substring first-word 0 1))
          (define starts-with-vowel? (string-contains? first-char "aeiou"))]
    (cons (if starts-with-vowel?
              "an"
              "a")
          word-list)))

;;
;; The following calls are filling in blanks in the other files.
;; This is needed because this file is in a different langauge than
;; the others.
;;
(set-find-the! find-the)
(set-find-within! find-within)
(set-restart-game! (λ () (start-game)))
(define (game-print object)
  (cond [(void? object)
         (void)]
        [(object? object)
         (print-description object)]
        [else (write object)]))

(current-print game-print)
   
;;;
;;; Start it up
;;;

(start-game)
(look)

