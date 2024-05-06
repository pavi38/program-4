#lang racket

;Game-state: list of all the items in all rooms and the objectives completed by the player
;Player-state: List of items in players bag

;funcation to remove the last element of the list 
(define (remove-last lst)
    (if (null? (cdr lst))
        '()
        (cons (car lst) (remove-last (cdr lst)))))


;funcation deals with the game end senerio
;we have multiple ending based on user behavior

(define (game-end game-state player-state)
  (define got-plan (member "war-plan" player-state))
  (define got-gold (member "gold" player-state))
  (define objectives (last game-state))
  (cond
    [(and (equal? (first objectives) #t) got-plan)(displayln "Assassin Master: Excellent work! assassin")]
    [(and (equal? (first objectives) #f) (equal? (second objectives) #f) got-gold)
     (displayln "You were BEHEADED for treason against the king albert VI.")]
    [(and (equal? (first objectives) #f) (equal? (second objectives) #f) (not got-plan))(displayln "Assassin Master: Did you go there on vacation?")]
    [else (displayln "Assassin Master: You need to work on your skills assassin!!")]
    )
  (display "\n\n\n")
  (game-start)
  )

;displays the main menu every time user types anything

(define (display-menu)
  (display "\n")
  (displayln "Movement: Enter direction north, south, east, west")
  (displayln "Need Help?: Enter \"help\" ")
  )

;displays the help menu when user type help
;displays the user location and all the available command

(define (display-help-menu)
  (displayln "Inventory: Enter \"bag\" ")
  (displayln "Pick-up Item: Enter pick + item name")
  (displayln "Search area: Enter \"search\" ")
  (displayln "List item in the Bag: Enter \"inventory\" ")
  (displayln "Current Location: Enter \"location\" ")
  )
(define (help current-location)
  (displayln (~a "Your Location: " (symbol->string current-location)))
  (displayln "\nAvailable commands:")
  
  (cond
    [(eq? 'chamber current-location)(displayln "KILL KING: Enter \"stab king\"")]
    [(eq? 'kitchen current-location)(displayln "POISON: Enter \"poison food\"")]
    )
  (display-help-menu)
  )
(define (updated-location player-state new-location)
  (define new-player-state (list new-location (last player-state)))
  new-player-state
  )

;list all itmes in user bag

(define (inventroy-items player-state)
     (displayln "Items in your inventory are:")
     (for-each (lambda (arg)
              (printf "* ~a\n" arg)
              23)          
          player-state)
  )

;handle command for search
;list all the items in the room

(define (search-location game-state location)
  (define room-state (first (filter (lambda (x) (eq? (first x) location)) game-state)))
  (define items-in-room (rest room-state))
  (displayln "Items found in room:")
   (for-each (lambda (arg)
              (printf "* ~a\n" arg)
              23)          
          items-in-room)
  )

;Pick Up item command
;removes the item from player-state and add it to game-state

(define (pickup-item game-state player-state location item)
  (define room-state (first (filter (lambda (x) (eq? (first x) location)) game-state)))
  (cond
    [(not (member item room-state))(displayln "No such item in the room")(list game-state player-state)]
    [else
      (define modified-room-state (remove item room-state)) ;add check to see if the item is in room state
      (define modified-game-state (cons modified-room-state (remove room-state game-state)))
      (define modified-player-state (cons item player-state))
      (displayln (~a "You picked up " item))
      (list modified-game-state modified-player-state)]
    )
  )

;Drop item command
;removes the item from game-state and add it to player-state

(define (drop-item game-state player-state location item)
  
  (cond
    [(not (member item player-state))(displayln "No such item in the bag")(list game-state player-state)]
    [else
       (define room-state (first (filter (lambda (x) (eq? (first x) location)) game-state)))
       (define modified-room-state (append room-state (list item))) ;add check to see if the item is in room state
       (define modified-game-state (cons modified-room-state (remove room-state game-state)))
       (define modified-player-state (remove item player-state))
       (list modified-game-state modified-player-state)]
    )
  )
(define (stab-king game-state player-state)
  (define objectives (last game-state))
  (cond
    [(first objectives)(displayln "Stabbing a death body is a taboo")(list game-state player-state)]
    [(not (member "knife" player-state))(displayln "Find a weapon...")(list game-state player-state)]
    [else
          (displayln "King Edward III: AHHHHHHHHH........BLAH")
          (displayln "king is Dead")
          (define modified-objectives (list #t (last objectives)))
          (define modified-game-state (append (remove-last game-state) (list modified-objectives)))
          (list modified-game-state player-state)
          ]
    )
        
)
(define (posion-food game-state player-state)
  (define objectives (last game-state))
  (cond
    [(second objectives)(displayln "Food has already been poisoned.")(list game-state player-state)]
    [(not (member "poison-flower" player-state))(displayln "Find a posion")(list game-state player-state)]
    [else
          (displayln "Food has been poisoned.")
          (define modified-objectives (list (first objectives) #t))
          (define modified-game-state (append (remove-last game-state) (list modified-objectives)))
          (define modified-player-state (remove "poison-flower" player-state))
          (list modified-game-state modified-player-state)
          ]
    )
        
)

;Fucation to handle all the game choices made by the player

(define (game-logic game-state player-state current-location direactions) ;current location is a tag
  ;(displayln  direactions)
  (display-menu)
  (define a (read-line (current-input-port) 'any))
  (display "\n")
  (cond
     [(equal? a "west") ((first direactions) game-state player-state)]
     [(equal? a "north") ((second direactions) game-state player-state)]
     [(equal? a "east") ((third direactions) game-state player-state)]
     [(equal? a "south") ((fourth direactions) game-state player-state)]
     [(equal? a "bag")(inventroy-items player-state)(game-logic game-state player-state current-location direactions)]
     [(equal? a "location")(displayln (~a "Your current location is: " (symbol->string current-location)))(game-logic game-state player-state current-location direactions)]
     [(equal? a "help")(help current-location)(game-logic game-state player-state current-location direactions)]
     [(equal? a "search")(search-location game-state current-location)(game-logic game-state player-state current-location direactions)]
     [(string-contains? a "pick")(define item (last (string-split a)))
                                      (apply game-logic (append (pickup-item game-state player-state current-location item) (list current-location direactions)))]
     [(string-contains? a "drop")(define item (last (string-split a)))
                                      (apply game-logic (append (drop-item game-state player-state current-location item) (list current-location direactions)))]
     [(and (eq? current-location 'chamber) (string-contains? a "stab"))
        (apply game-logic (append (stab-king game-state player-state) (list current-location direactions)))]
     [(and (eq? current-location 'kitchen) (string-contains? a "poison"))
        (apply game-logic (append (posion-food game-state player-state) (list current-location direactions)))]
     [(and (eq? current-location 'courtyard) (equal? a "s"))
        (game-end game-state player-state)]
     [else (displayln "No such command found!!")(game-logic game-state player-state current-location direactions)]
     
     
    )
  ;(list game-state player-state)
  )
(define (throne-room game-state player-state)
  (displayln " echoes of power reverberate off stone walls adorned with countless animal heads depicting epic tales,
 while the imposing seat of authority, bathed in golden light, awaits the ruler's solemn presence amidst an aura of majesty and legacy")
  (displayln "Current Location: Throne Room")
  (game-logic game-state player-state 'throne-room (list treasry kitchen kings-chamber location-courtyard)) 
 )
 
(define (treasry game-state player-state)
  (define direactions (list kings-chamber throne-room location-courtyard kitchen))
  (cond
    [(not (member "key" player-state))(displayln "Safe locked find a key.")(game-logic game-state player-state 'tresery direactions)]
    [else (displayln "Safe unlocked!")
          (define room-state (first (filter (lambda (x) (eq? (first x) 'tresery)) game-state)))
          (define modified-room-state (append room-state (list "gold"))) ;add check to see if the item is in room state
          (define modified-game-state (cons modified-room-state (remove room-state game-state)))
          (game-logic modified-game-state player-state 'tresery direactions)]
  )
)


(define (kitchen game-state player-state)
  (displayln "Walls adorned with gleaming copper pots. Amidst the tranquil darkness,
the lingering scent of spices and hearth fire painted a picture of warmth and nourishment in the silent halls of the sleeping fortress.")
  (game-logic game-state player-state 'kitchen (list treasry location-courtyard kings-chamber throne-room))
)


(define (kings-chamber game-state player-state)
  (displayln "draped in regal splendor, the monarch slumbers beneath canopies of velvet, his breathing a rhythmic cadence in the quiet of the night,
guarded by the shadows cast by loyal sentinels and the flickering glow of candlelight.")
  (game-logic game-state player-state 'chamber (list location-courtyard throne-room treasry kitchen))
  )


(define (location-courtyard game-state player-state)
        (define current-location 'courtyard)
       
        (displayln "\nThe gates of the castle stood sentinel, imposing and forbidding, their ancient stones echoing tales of glory and defiance. Beyond them, a world of mystery and adventure beckoned,")
        (display "\n")
        (displayln "Enter \"s\": enter the sewers to exit castle")
        (game-logic game-state player-state 'courtyard (list treasry throne-room kings-chamber kitchen))
        
        
 )

;Main game loop
(define (game-start)
  ;list of all the items at the location and objectives completed by player
  (define game-state '((courtyard "poison-flower")(chamber "key")(throne-room "war-plan")(kitchen "knife")(tresery)(#f #f))) 
  (define player-state  '())
  (displayln "Welcome to the assasin's adventure!")
  (display "\n")
  (displayln "Year 1406\n")
  (displayln "You are a assasin working for king Albert VI. Who has been in war with King Edward III. Albert's been on the losing front for past years. Now you are his only hope in this raging war.")
  (display "\n")
  (displayln "Objectives for this mission:")
  (displayln "1. Assinate King aurther")
  (displayln "2. steal War-scroll\n\n")
  (displayln "type P to Play.")
  (displayln "Type Q to quit.")
  (define a (read-line (current-input-port) 'any))
  (cond
    [(equal? a "P") (location-courtyard game-state player-state)]
    [else "Thanks For Playing!!"])  
)

;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(game-start)