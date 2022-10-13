; ---------------------------------------------
; ---------------- CHICKEN RUN ----------------
; ---------------------------------------------

; We use the w key as jump key
; The s key to restart the game when dead
; the p key to pause the game

; --- COLOUR CODES ---
; $00 == black
; $01 == white
; $02 == red
; $03 == blue
; $05 == green
; $07 == yellow
; $09 == brown
; $0b == dark grey
; $0d == light green
; $0f == light grey

; ---- DECLARATION OF VARIABLES ---
; determines where the variables are saved in memory
; - Chicken variables -
define chickenPosL			$00 ; screen location of chicken, low byte
define chickenPosH			$01 ; screen location of chicken, high byte
define chickenDrawPosL		$02 ; screen location of draw chicken, low byte
define chickenDrawPosH		$03 ; screen location of draw chicken, high byte
define chickenActionState	$04 ; action state: possible values are below
define animationTime 		$05 ; animation time of the chicen
define animationTimeMax		$06 ; maximum animation time of the chicken
define timeFlying			$07 ; time chicken spends in the air
define maxTimeFlying		$08 ; maximum time that the chicken may spend in the air

; - Flower variables -
define flowerPosL			$10 ; screen location of the flower, low byte
define flowerPosH			$11 ; screen location of the flower, high byte
define flowerDrawPosL		$12 ; screen location of draw flower, low byte
define flowerDrawPosH		$13 ; screen location of draw flower, high byte
define flowerCountdownSpeed $14 ; update flower every x number of frames

; - Cloud variables -
define cloudPosL			$20
define cloudPosH			 $21
define cloudCountdownSpeed	$22

; - General variables -
define displayPosL 			$30 ; display position to draw background, low byte
define displayPosH 			$31 ; display position to draw background, high byte
define substractValue		$32 ; substract value to use in the function getPixelPosAbove
define addValue				$34 ; add value to use in the function getPixelPosBelow
define lowOrderByte			$35 ; low byte in the funtions getPixelPosAbove, getPixelPosBelow
define highOrderByte		$36 ; high byte in the functions getPixelPosAbove, getPixelPosBelow
define colourText			$37 ; colour of the text

; - System variables -
define sysRandom    $fe
define sysLastKey   $ff

; ---- DEFINE MULTIPLE POSSIBILITIES FOR VARIABLES ----
; - Possible values for chicken action state -
define jumping		1
define flying		2
define falling		4
define running		8

; - ASCII values of keys controlling the game
define ASCII_w     	$77 ; chicken jump
define ASCII_s		$73 ; start game
define ASCII_p		$70 ; pause game


; ---------------- GAME ----------------
  jsr init
  jmp gameOver ; to draw the start screen

startGame:
  jsr init
  jsr initObstacle
continueGame:
  jsr loop

pauseGame:
  lda #$0b
  sta colourText
  jsr drawPausedText
  lda sysLastKey
  cmp #ASCII_p
  beq liftPause
  jmp pauseGame
liftPause:
  lda #$00
  sta sysLastKey
  lda #$03
  sta colourText
  jsr drawPausedText
  jmp continueGame

gameOver:
  jsr drawInfo
  jsr readSKey
  jmp gameOver

; ---------------- INITIALISATION GAME ----------------
init:
  ; - system variables initialisation -
  lda #$00
  sta sysLastKey
  
  jsr initSky
  jsr drawBackground ; already draw background
  jsr initChicken
  jsr drawChicken ; draw chicken
  
  rts
; - initialisation sky -
initSky:
  lda #$02
  sta displayPosH
  lda #$00
  sta displayPosL
  rts

; - initialisation chicken - 
initChicken:
  lda #$00
  sta animationTime
  sta timeFlying

  lda #$08
  sta animationTimeMax
  
  lda #$01
  sta maxTimeFlying
  
  lda #running
  sta chickenActionState ; initialise chickenDirec
  
  lda #$a5 	; load low bit pos of chickenstart
  sta chickenPosL
  lda #$04
  sta chickenPosH ;load high bit pos of chickenstart
  rts

; --- initialisation obstacle ---   
initObstacle:
  lda sysRandom
  and #1
  cmp #$00
  bne initFlower
  jmp initCloud

; - flower -
initFlower:
  lda #$BE
  sta flowerPosL
  sta flowerDrawPosL
  lda #$04
  sta flowerPosH
  sta flowerDrawPosH
  lda #1		; seconds countdown
  sta flowerCountdownSpeed
  ; drawFlower
  jsr drawFlower

  ; init highorder byte of cloud
  lda #$01
  sta cloudPosH
  rts

; - cloud -
initCloud:
  lda #$de
  sta cloudPosL
  lda #$03
  sta cloudPosH
  lda #3
  sta cloudCountdownSpeed
  ; draw cloud for the first time
  jsr drawCloud

  ; init highorder byte of flower
  lda #$01
  sta flowerPosH
  rts

; ---------------- INITIALISATION DRAWING FUNCTIONS ----------------
drawBackground:
	jsr drawSky
	jsr drawGround
	rts
	
drawSky:
	lda #$20
	sta addValue
	
	lda displayPosL
	ldx displayPosH
	
	sta lowOrderByte
	stx highOrderByte
	
	loopSky:
	  ldx #$03 ; colour sky
	  jsr drawLine ; draw one line of display
	  jsr getPixelPosBelow
	  sta displayPosL
	  stx displayPosH
	  cpx #$04
	  bne loopSky
	  ldy displayPosL
	  cpy #$c0
	bne loopSky ; if it's not equal then you can draw another line
	rts; stop the loop

drawGround:
	ldx #$05 ; colour grass
	jsr drawLine
	jsr getPixelPosBelow
	sta displayPosL
	stx displayPosH
	
	loopGround:
	  ldx #$09 ; colour ground
	  jsr drawLine
	  jsr getPixelPosBelow
	  sta displayPosL
	  stx displayPosH
	  cpx #$05
	  bne loopGround
	  ldy displayPosL ; check if memory address is completely the same as the first pixel of the next row
	  cpy #$e0
	  bne loopGround
	  ldx #$09 ; colour ground
	  jsr drawLine
	rts

; --- DRAW CHICKEN ---
drawChicken:
  ldy #0
  lda #$07		; yellow
  sta (chickenPosL), y	; draw yellow feet
  jsr drawChickenBody	; draw rest of body
  rts

drawChickenBody:
  ;draw first line above feet
  lda chickenPosL	; load low bit, we're gonna substract
  ldx chickenPosH
  sta lowOrderByte
  stx highOrderByte
  sta chickenDrawPosL
  stx chickenDrawPosH
  lda #$21
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  ldy #0
  lda #$01		; white
  sta (chickenDrawPosL), y; draw pixel
  iny
  sta (chickenDrawPosL), y
  iny
  sta (chickenDrawPosL), y

  ; draw second line
  lda #$20
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  ldy #0
  lda #$01		; white
  sta (chickenDrawPosL), y; draw pixel
  iny
  lda #$0F		; grey
  sta (chickenDrawPosL), y
  iny
  lda #$01		; white
  sta (chickenDrawPosL), y

  ;draw third line above feet
  lda #$20		; substract with carry
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  ldy #0
  lda #$01		; white
  sta (chickenDrawPosL), y; draw pixel
  iny
  sta (chickenDrawPosL), y
  iny
  sta (chickenDrawPosL), y
  iny
  sta (chickenDrawPosL), y
  iny
  lda #$07		; yellow
  sta (chickenDrawPosL), y; draw beak

  ;draw fourth line above feet
  lda #$1e	; substract with carry
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  ldy #0
  lda #$01		; white
  sta (chickenDrawPosL), y; draw pixel
  iny
  lda #$0B		; eye
  sta (chickenDrawPosL), y

  ;draw fifth line above feet
  lda #$20
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  ldy #0
  lda #$02		; red
  sta (chickenDrawPosL), y; draw pixel
  iny
  sta (chickenDrawPosL), y

  rts

; --- DRAW OBSTACLES ---
; - cloud -
drawCloud:
  lda cloudPosL
  sta lowOrderByte
  lda cloudPosH
  sta highOrderByte

  ; draw first line of cloud
  dec lowOrderByte
  ldy #0
  lda #$01 ; colour white
  sta (lowOrderByte), y
  iny
  sta (lowOrderByte), y
  iny
  sta (lowOrderByte), y

  ; draw second line of cloud
  lda #$1f
  sta substractValue
  jsr getPixelPosAbove
  lda #$01 ; colour white
  ldy #0
  sta (lowOrderByte), y
  rts

  ; - flower -
drawFlower:
  ldy #0
  lda #13		; ligh green
  sta (flowerPosL), y	; draw green
  lda flowerPosH	; copy flowerPosH
  sta flowerDrawPosH	; copy Hpos to drawHpos
  jsr drawBodyFlower	; draw rest of flower
  rts

drawBodyFlower:
  ; complete flower
  ; first row above
  lda flowerPosL	; load low bit, we're gonna substract
  sec			; set carry flag
  sbc #$20		; substract with carry
  bcc substractHighBit	; branch if carry flag is set off
  sta flowerDrawPosL
  lda #$07		; yellow
  sta (flowerDrawPosL), y; draw pixel
  ; second row above
  lda flowerPosL
  sec
  sbc #$41
  bcc substractHighBit
  sta flowerDrawPosL
  lda #$07		; yellow
  sta (flowerDrawPosL), y; draw pixel
  inc flowerDrawPosL
  lda #$09		; brown
  sta (flowerDrawPosL), y; draw pixel
  inc flowerDrawPosL
  lda #$07		; yellow
  sta (flowerDrawPosL), y; draw pixel
  ; third row above
  lda flowerPosL
  sec
  sbc #$60
  bcc substractHighBit
  sta flowerDrawPosL
  lda #$07		; yellow
  sta (flowerDrawPosL), y; draw pixel
  rts
  
substractHighBit:
  dec flowerDrawPosH	; decrease high bit by 1
  rts

; ---------------- GAME LOOP ----------------
loop:
  jsr readPKey
  inc animationTime
  jsr updateObstacle
  jsr readWKey
  jsr updateChickenActionState
  jsr updateChickenPos
  jsr checkCollision
  jsr updateChickenDrawing
  jmp loop


; ---------------- UPDATE FUNCTIONS ----------------
; --- UPDATE CHICKEN ---


updateChickenActionState:
  ldx animationTime
  cpx animationTimeMax
  beq changeActionState
  rts
  
changeActionState:
  lda #0
  sta animationTime ; reset animationTime
  lda #jumping ; if it's equal to jumping then set to flying
  cmp chickenActionState
  beq changeActionStateToFlying
  lda #flying ; if it's equal to flying then set to falling if the timer allows it
  cmp chickenActionState
  beq changeActionStateToFalling
  lda #falling ; if it's equal to falling then set to running
  cmp chickenActionState
  beq changeActionStateToRunning
  ; if it's equal to running then don't change it
  rts

changeActionStateToJumping:
  lda #0
  sta animationTime ; reset animation time
  sta sysLastKey ; reset last key input, else it keeps jumping
  lda #jumping
  sta chickenActionState
  rts
changeActionStateToFlying:
  lda #flying
  sta chickenActionState
  rts
changeActionStateToFalling:
  ldx timeFlying
  cpx maxTimeFlying
  beq changeToFalling ; if it's equal then change the action state to falling else increment the timeFlying variable
  inx
  stx timeFlying
  rts
changeToFalling:
  ldx #0
  stx timeFlying
  lda #falling
  sta chickenActionState
  rts
changeActionStateToRunning:
  lda #running
  sta chickenActionState
  rts

updateChickenPos:
  lda #$20
  sta substractValue
  sta addValue
  
  lda chickenPosL
  ldx chickenPosH
  ; for GetPixel... functions
  sta lowOrderByte
  stx highOrderByte
  
  ldy #jumping
  cpy chickenActionState
  beq moveUp
  
  ldy #falling
  cpy chickenActionState
  beq moveDown

  ; when chicken is running or flying then don't alter the chickenPos
  rts
  
moveUp:
  jsr getPixelPosAbove
  stx chickenPosH
  sta chickenPosL
  rts
moveDown:
  jsr getPixelPosBelow
  stx chickenPosH
  sta chickenPosL
  rts

; --- UPDATE OBSTACLES --- 
; - cloud -
updateObstacle:
  jsr updateFlower
  jsr updateCloud
  rts

updateCloud:
  lda cloudPosH
  cmp #$01
  beq endUpdateCloud
  dec cloudCountdownSpeed
  lda cloudCountdownSpeed
  cmp #0
  beq updateCloudPos
endUpdateCloud:
  rts

updateCloudPos:
  lda cloudPosL
  cmp #$c1
  beq clearCloud
  ; update the position and let it draw accordingly so that the cloud looks updated
  dec cloudPosL
  jsr drawUpdatedCloud
  lda #3
  sta cloudCountdownSpeed
  rts
  
clearCloud:
  ; draw cloud blue
  lda cloudPosL
  sbc #1 ; substract with carry -> shouldn't be a carry raised
  sta lowOrderByte
  lda cloudPosH
  sta highOrderByte
  ldy #0
  lda #$03 ; colour blue
  sta (lowOrderByte), y
  iny
  sta (lowOrderByte), y
  iny 
  sta (lowOrderByte), y
  lda #$1f
  sta substractValue
  jsr getPixelPosAbove
  ldy #0
  lda #$03 ; colour blue
  sta (lowOrderByte), y

  ; set high bit to something it could never be
  lda #$01
  sta cloudPosH

  ; initialise a new object
  jsr initObstacle
  rts

; - flower -
updateFlower:
  ; first look if there's a flower to be updated
  lda flowerPosH
  cmp #$01
  beq endUpdateFlower

  dec flowerCountdownSpeed
  lda flowerCountdownSpeed
  cmp #00
  beq updateFlowerPos ; move flower + reset counter
endUpdateFlower:
  rts

updateFlowerPos:
  dec flowerPosL
  lda flowerPosL
  cmp #$a0
  beq resetFlower  ; sets flower to begin, out of bounds here
  jsr drawUpdatedFlower	; only draw when pos is changed
  lda #3		; seconds countdown
  sta flowerCountdownSpeed
  rts

resetFlower:
  jsr clearFlower ; draw flower in blue so it dissapears
  jsr initObstacle ; initialise new obstacle
  rts
  
clearFlower:
  inc flowerPosL
; color the flower back in background color: light blue
  ldy #0
  lda #$03		; blue
  sta (flowerPosL), y	; draw 
  lda flowerPosH	; copy 
  sta flowerDrawPosH	; copy Hpos to drawHpos
  lda flowerPosL	; load low bit, we're gonna substract
  sec			; set carry flag
  sbc #$20		; substract with carry
  bcc substractHighBitClearFlower	; branch if carry flag is set off
  sta flowerDrawPosL
  lda #$03		; blue
  sta (flowerDrawPosL), y; draw pixel
  
; second row above
  lda flowerPosL
  sec
  sbc #$41
  bcc substractHighBitClearFlower
  sta flowerDrawPosL
  lda #$03		; blue
  sta (flowerDrawPosL), y; draw pixel
  inc flowerDrawPosL
  lda #$03		; blue
  sta (flowerDrawPosL), y; draw pixel
  inc flowerDrawPosL
  lda #$03		; blue
  sta (flowerDrawPosL), y; draw pixel
  
; third row above
  lda flowerPosL
  sec
  sbc #$60
  bcc substractHighBitClearFlower
  sta flowerDrawPosL
  lda #$03		; blue
  sta (flowerDrawPosL), y; draw pixel
  rts

substractHighBitClearFlower:
  dec flowerDrawPosH	; decrease high bit by 1
  rts

; ---------------- UPDATE DRAWING FUNCTIONS ----------------
; --- UPDATE DRAWING CHICKEN ---
updateChickenDrawing:
  ldy #jumping
  cpy chickenActionState
  beq updateChickenDrawingUp
  ldy #falling
  cpy chickenActionState
  beq updateChickenDrawingDown
  jmp drawFullChicken
  rts
drawFullChicken:
  jsr drawChicken
  rts
updateChickenDrawingUp:
  jsr updateChickenDrawingUpFunction
  rts
updateChickenDrawingDown:
  jsr updateChickenDrawingDownFunction
  rts
updateChickenDrawingUpFunction:
  lda #$20 ; initialise add and substract value on dec value 32 -> 1 row
  sta addValue
  sta substractValue
  
  ldx chickenPosH	; copy chickenPosH
  lda chickenPosL ; copy chickenPosL
  ; for GetPixel... functions
  sta lowOrderByte
  stx highOrderByte
  
  jsr getPixelPosBelow
  ; now in register A and register X is the pos for the yellow foot in the previous frame
  sta chickenDrawPosL
  stx chickenDrawPosH

; line 1 of the chicken  
  ; colour to blue
  lda #$03
  ldy #0
  sta (chickenDrawPosL), y
  
; line 2 of the chicken
  lda #$21
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  ; colour white
  lda #$03
  sta (chickenDrawPosL), y
  iny
  iny
  sta (chickenDrawPosL), y
  dey
  lda #$07 ; colour yellow
  sta (chickenDrawPosL), y
  
; line 3 of the chicken
  lda #$1f
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  ldy #0
  lda #$01 ; colour white
  sta (chickenDrawPosL), Y

; line 4 of the chicken
  lda #$20
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  lda #$0f ; colour light grey
  ldy #0
  sta (chickenDrawPosL), Y
  iny
  iny
  lda #$03 ; colour blue
  sta (chickenDrawPosL), y
  iny
  sta (chickenDrawPosL), y
; line 5 of the chicken
  lda #$21
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  lda #$01 ; colour white
  ldy #0
  sta (chickenDrawPosL), y
  iny
  sta (chickenDrawPosL), y
  iny
  iny
  sta (chickenDrawPosL), y
  iny
  lda #$07 ; colour yellow
  sta (chickenDrawPosL), y
  
; line 6 of the chicken
  lda #$1e
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  lda #$01 ; colour white
  ldy #0
  sta (chickenDrawPosL), y
  iny
  lda #$0b ; colour dark grey
  sta (chickenDrawPosL), y
  
; line 7 of the chicken
  lda #$20
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  lda #$02 ; colour red
  ldy #0
  sta (chickenDrawPosL), y
  iny
  sta (chickenDrawPosL), y
  
  rts
updateChickenDrawingDownFunction:
  lda #$20 ; initialise add and substract value on dec value 32 -> 1 row
  sta addValue
  sta substractValue
  
  ldx chickenPosH	; copy chickenPosH
  lda chickenPosL ; copy chickenPosL
  ; for GetPixel... functions
  sta lowOrderByte
  stx highOrderByte
  
  ; now in register A and register X is the pos for the yellow foot in the previous frame
  sta chickenDrawPosL
  stx chickenDrawPosH

; line 1 of the chicken  
  ; colour to yellow
  lda #$07
  ldy #0
  sta (chickenDrawPosL), y
  
; line 2 of the chicken
  lda #$21
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  ; colour white
  lda #$01
  sta (chickenDrawPosL), y
  iny
  sta (chickenDrawPosL), y
  iny
  sta (chickenDrawPosL), y
  
; line 3 of the chicken
  lda #$1f
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  ldy #0
  lda #$0f ; colour light grey
  sta (chickenDrawPosL), Y

; line 4 of the chicken
  lda #$20
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  lda #$01 ; colour white
  ldy #0
  sta (chickenDrawPosL), Y
  iny
  iny
  sta (chickenDrawPosL), y
  iny
  lda #$07
  sta (chickenDrawPosL), y
; line 5 of the chicken
  lda #$21
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  lda #$03 ; colour blue
  ldy #0
  sta (chickenDrawPosL), y
  iny
  sta (chickenDrawPosL), y
  iny
  lda #$0b; colour dark grey
  iny
  sta (chickenDrawPosL), y
  iny
  lda #$03 ; colour blue
  sta (chickenDrawPosL), y
  
; line 6 of the chicken
  lda #$1E
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  lda #$02 ; colour red
  ldy #0
  sta (chickenDrawPosL), y
  iny
  sta (chickenDrawPosL), y
  
; line 7 of the chicken
  lda #$20
  sta substractValue
  jsr getPixelPosAbove
  sta chickenDrawPosL
  stx chickenDrawPosH
  
  lda #$03 ; colour blue
  ldy #0
  sta (chickenDrawPosL), y
  iny
  sta (chickenDrawPosL), y
  
  rts

; --- DRAW UPDATED OBSTACLES ---
; - cloud -
drawUpdatedCloud:
  lda cloudPosH
  sta highOrderByte
  lda cloudPosL
  sta lowOrderByte
  
; draw first line of cloud
  dec lowOrderByte
  lda #$01 ; colour white
  ldy #0
  sta (lowOrderByte), y
  iny
  sta (lowOrderByte), y
  iny
  sta (lowOrderByte), y
  iny
  lda #$03 ; colour blue
  sta (lowOrderByte), y

; draw second line of cloud
  lda #$22
  sta substractValue
  jsr getPixelPosAbove
  lda #$01 ; colour white
  sta (lowOrderByte), y
  iny
  lda #$03 ; colour blue
  sta (lowOrderByte), y

; - flower -
drawUpdatedFlower:
  ldy #0
  lda #13		; ligh green
  sta (flowerPosL), y	; draw green
  iny
  lda #$03
  sta (flowerPosL), y
  lda flowerPosH	; copy flowerPosH
  sta flowerDrawPosH	; copy Hpos to drawHpos
  ; complete flower
  ; first row above
  lda flowerPosL	; load low bit, we're gonna substract
  sec			; set carry flag
  sbc #$20		; substract with carry
  bcc substractHighBitUpdated	; branch if carry flag is set off
  sta flowerDrawPosL
  ldy #0
  lda #$07		; yellow
  sta (flowerDrawPosL), y; draw pixel
  inc flowerDrawPosL
  lda #$03 ; colour blue
  sta (flowerDrawPosL), y
  ; second row above
  lda flowerPosL
  sec
  sbc #$41
  bcc substractHighBitUpdated
  sta flowerDrawPosL
  lda #$07		; yellow
  sta (flowerDrawPosL), y; draw pixel
  inc flowerDrawPosL
  lda #$09		; brown
  sta (flowerDrawPosL), y; draw pixel
  inc flowerDrawPosL
  lda #$07		; yellow
  sta (flowerDrawPosL), y; draw pixel
  inc flowerDrawPosL
  lda #$03
  sta (flowerDrawPosL), y
  ; third row above
  lda flowerPosL
  sec
  sbc #$60
  bcc substractHighBitUpdated
  sta flowerDrawPosL
  lda #$07		; yellow
  sta (flowerDrawPosL), y; draw pixel
  inc flowerDrawPosL
  lda #$03
  sta (flowerDrawPosL), y
  rts

substractHighBitUpdated:
  dec flowerDrawPosH	; decrease high bit by 1
  rts

; ---------------- COLLISION DETECTION WITH OBSTACLES ----------------
checkCollision:
  jsr checkFlowerCollision
  jsr checkCloudCollision
  rts

; - cloud -
checkCloudCollision:
  lda cloudPosH
  cmp #$01
  beq doneCheckingCloudCollision
  
  ldx chickenPosL
  stx lowOrderByte
  ldx chickenPosH
  stx highOrderByte
  ldx #$a0
  jsr getPixelPosAbove
  lda cloudPosL
  cmp lowOrderByte
  bne doneCheckingCloudCollision
  lda cloudPosH
  cmp highOrderByte
  bne doneCheckingCloudCollision

  jmp gameOver ; collision with cloud
doneCheckingCloudCollision:
  rts
; - flower -
checkFlowerCollision:
  lda flowerPosL
  cmp #$00
  beq doneCheckingFlowerCollision

  cmp chickenPosL
  bne doneCheckingFlowerCollision
  lda flowerPosH
  cmp chickenPosH
  bne doneCheckingFlowerCollision

  ; collision with flower
  jmp gameOver
doneCheckingFlowerCollision:
  rts
 
; ---------------- DRAW TEXT ON SCREEN FUNCTIONS ----------------
drawInfo:
  lda #$21
  sta lowOrderByte
  lda #$02
  sta highOrderByte
;PRESS S
; first line of text
  ; blue colour -> 06
  lda #$0B
  ldy #0
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw3Pixels
  iny
  iny
  jsr draw3Pixels
; second line of text
  ldx #$20
  stx addValue
  jsr getPixelPosBelow ; get the beginning of next line
  ldy #0 ; reset offset to 0
  lda #$0B ; colour grey
  jsr draw2PixelsWithGap
  jsr draw2PixelsWithGap
  jsr draw1PixelFront
  jsr draw1PixelFront
  jsr draw1PixelFront
  iny
  iny
  sta (lowOrderByte), y
; third line of text
  jsr getPixelPosBelow ; get beginning of next line
  ldy #0 ; reset offset to 0
  lda #$0B ; reset colour
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw2PixelsFront
  jsr draw3Pixels
  jsr draw3Pixels
  iny
  iny
  jsr draw3Pixels
; fourth line of text
  jsr getPixelPosBelow ; get beginning of next line
  ldy #0
  lda #$0B
  jsr draw1PixelFront
  jsr draw2PixelsFront
  jsr draw1PixelFront
  jsr draw1PixelBack
  jsr draw1PixelBack
  iny
  iny
  jsr draw1PixelBack
; fifth line of text
  jsr getPixelPosBelow ; get beginning of next line
  ldy #0
  lda #$0B
  jsr draw1PixelFront
  jsr draw2PixelsWithGap
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw3Pixels
  iny
  iny
  jsr draw3Pixels
;TO START
; first line of text
  ldx #$40
  stx addValue
  jsr getPixelPosBelow ; get beginning of next line
  ldy #0
  lda #$0B
  jsr draw3Pixels
  jsr draw3Pixels
  iny
  iny
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw3Pixels
; second line of text
  ldx #$20
  stx addValue
  jsr getPixelPosBelow
  ldy #0
  lda #$0B
  jsr draw1PixelMiddle
  jsr draw2PixelsWithGap
  iny
  iny
  jsr draw1PixelFront
  jsr draw1PixelMiddle
  jsr draw2PixelsWithGap
  jsr draw2PixelsWithGap
  jsr draw1PixelMiddle
; third line of text
  jsr getPixelPosBelow
  ldy #0
  lda #$0B
  jsr draw1PixelMiddle
  jsr draw2PixelsWithGap
  iny
  iny
  jsr draw3Pixels
  jsr draw1PixelMiddle
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw1PixelMiddle
; fourth line of text
  jsr getPixelPosBelow
  ldy #0
  lda #$0B
  jsr draw1PixelMiddle
  jsr draw2PixelsWithGap
  iny
  iny
  jsr draw1PixelBack
  jsr draw1PixelMiddle
  jsr draw2PixelsWithGap
  jsr draw2PixelsFront
  jsr draw1PixelMiddle
; fifth line of text
  jsr getPixelPosBelow
  ldy #0
  lda #$0B
  jsr draw1PixelMiddle
  jsr draw3Pixels
  iny
  iny
  jsr draw3Pixels
  jsr draw1PixelMiddle
  jsr draw2PixelsWithGap
  jsr draw2PixelsWithGap
  jsr draw1PixelMiddle
  rts

; this function draw the word PAUSED on the screen when the game is paused
drawPausedText:
  lda #$24
  sta lowOrderByte
  lda #$02
  sta highOrderByte
  lda colourText
  ldy #0
; first line of text
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw2PixelsWithGap
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw2PixelsFront
; second line of text
  ldx #$20
  stx addValue
  jsr getPixelPosBelow
  lda colourText
  ldy #0
  jsr draw2PixelsWithGap
  jsr draw2PixelsWithGap
  jsr draw2PixelsWithGap
  jsr draw1PixelFront
  jsr draw1PixelFront
  jsr draw2PixelsWithGap
; third line of text
  jsr getPixelPosBelow
  lda colourText
  ldy #0
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw2PixelsWithGap
  jsr draw3Pixels
  jsr draw2PixelsFront
  jsr draw2PixelsWithGap
; fourth line of text
  jsr getPixelPosBelow
  lda colourText
  ldy #0
  jsr draw1PixelFront
  jsr draw2PixelsWithGap
  jsr draw2PixelsWithGap
  jsr draw1PixelBack
  jsr draw1PixelFront
  jsr draw2PixelsWithGap
; fifth line of text
  jsr getPixelPosBelow
  lda colourText
  ldy #0
  jsr draw1PixelFront
  jsr draw2PixelsWithGap
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw3Pixels
  jsr draw2PixelsFront
  rts

; --- HELPER FUNCTIONS DRAWING TEXT ---
draw3Pixels:
  sta (lowOrderByte), y
  iny
  sta (lowOrderByte), y
  iny
  sta (lowOrderByte), y
  iny
  iny
  rts
draw2PixelsFront:
  sta (lowOrderByte), y
  iny
  sta (lowOrderByte), y
  iny
  iny
  iny
  rts
draw2PixelsWithGap:
  sta (lowOrderByte), y
  iny
  iny
  sta (lowOrderByte), y
  iny
  iny
  rts
draw1PixelFront:
  sta (lowOrderByte), y
  iny
  iny
  iny
  iny
  rts
draw1PixelMiddle:
  iny
  sta (lowOrderByte), y
  iny
  iny
  iny
  rts
draw1PixelBack:
  iny
  iny
  sta (lowOrderByte), y
  iny
  iny
  rts

; ---------------- READ KEY FUNCTIONS ----------------
readSKey:
  lda sysLastKey
  cmp #ASCII_s
  bne noSInput
  lda #$00
  sta sysLastKey
  jmp startGame
noSInput:
  lda #$00
  sta sysLastKey
  rts

readPKey:
  lda sysLastKey
  cmp #ASCII_p
  bne noPInput
  lda #$00
  sta sysLastKey
  jmp pauseGame
noPInput:
  rts

readWKey:
  lda sysLastKey
  cmp #ASCII_w
  beq upKey ; branch  if equal to upkey
  rts
  upKey:
  lda #running
  cmp chickenActionState ; if chickenActionState is equal to running
  ; can only jump when it's just running
  beq actionStateToJumping
  rts
actionStateToJumping:
  jsr changeActionStateToJumping
  rts

; ---------------- HELPER FUNCTIONS ----------------
getPixelPosAbove:
  ; load low order byte into register A
  lda lowOrderByte
  ; load high order byte into register X
  ldx highOrderByte
  sec ; set carry flag
  sbc substractValue ; substract with carry
  sta lowOrderByte
  bcc substractFromHighBit ; branch if carry flag is set off
  rts
substractFromHighBit:
  dex 
  stx highOrderByte
  rts

getPixelPosBelow:
  ; load low order byte into register A
  lda lowOrderByte
  ; load high order byte into register X
  ldx highOrderByte
  clc; clear carry flag
  adc addValue; + 32
  sta lowOrderByte
  bcs addToHighBit; if carry flag is set then overflow
  rts
addToHighBit:
  inx
  stx highOrderByte
  rts
  
drawLine:
  ldy #$00; offset on zero
	firstloop:
	  txa
	  sta (displayPosL),Y
	  pha
	  iny
	  cpy #$10
	  bne firstloop ;loop until Y is $10
	secondloop:
	  pla
	  sta (displayPosL),Y
	  iny
	  cpy #$20      ;loop until Y is $20
	  bne secondloop
  rts