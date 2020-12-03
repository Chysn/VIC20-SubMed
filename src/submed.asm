;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                   Sub Med
;                            (c)2020, Jason Justian
;                  
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This software is released under the Creative Commons
; Attribution-NonCommercial 4.0 International
; License. The license should be included with this file.
; If not, please see: 
;
; https://creativecommons.org/licenses/by-nc/4.0/legalcode.txt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BASIC LAUNCHER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is the tokenization of the following BASIC program, which
; runs this game
;     42 SYS4110
* = $1001
BASIC:      .byte $0b,$04,$2a,$00,$9e,$34,$31,$31
            .byte $30,$00,$00,$00,$00
            jmp Startup
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constants - Game Configuration
SCRCOM      = 107               ; Screen color
TXTCOL      = $01               ; Text color
UP          = $01               ; Directional constants
RIGHT       = $02               ; ,,
DOWN        = $03               ; ,,
LEFT        = $04               ; ,,
FIRE        = $05               ; Joystick fire button pressed
PL_SPEED    = $05               ; Player speed (delay per pixel, in jiffies)
SEALIFE_GEN = $30               ; Sealife generation frequency (jiffies)
O2_START    = $20               ; Starting oxygen
O2_RATE     = $80               ; Oxygen depletion rate (jiffies)

; Score constants
PICKUP      = 25                ; Score when a med pack is picked up
DELIVERY    = 100               ; Score when a med pack is delivered
O2_ADD      = $08               ; Oxygen added when surfacing

; Character constants
CO_PLAYER   = $07               ; Player color
CHAR_S      = $2e               ; Source bitmap character code
CHAR_D      = $2f               ; Destination bitmap character code
LANDCHAR    = $25               ; Land and sky character
TOPLAND     = $26               ; Top land character
CO_SKY      = $03               ; Sky color
MEDPACK     = $1b               ; Med Pack
SEABASE     = $00               ; Sea base
FISH        = $27               ; School of fish
CO_FISH     = $05               ; Fish color
O2_CHAR     = $3a               ; Oxygen icon

; wAxScore Constants
NO_EFFECT   = $0f
LEGATO_ON   = $3f
LEGATO_OFF  = $4f

; System Resources
CINV        = $0314             ; ISR vector
NMINV       = $0318             ; Release NMI vector
;NMINV       = $fffe             ; Development NMI non-vector
SCREEN      = $1e00             ; Screen character memory (unexpanded)
COLOR       = $9600             ; Screen color memory (unexpanded)
IRQ         = $eabf             ; System ISR   
BASRND      = $e094             ; Routine for BASIC's RND() function
RNDNUM      = $8d               ; Result storage location for RND()
VICCR5      = $9005             ; Character map register
VOICEH      = $900c             ; High sound register
VOICEM      = $900b             ; Mid sound register
VOICEL      = $900a             ; Low sound register
FX_VOICE    = $900c             ; Sound effects voice
NOISE       = $900d             ; Noise register
VOLUME      = $900e             ; Sound volume register/aux color
BACKGD      = $900f             ; Background color
VIA1DD      = $9113             ; Data direction register for joystick
VIA1PA      = $9111             ; Joystick port (up, down, left, fire)
VIA2DD      = $9122             ; Data direction register for joystick
VIA2PB      = $9120             ; Joystick port (for right)
CLSR        = $e55f             ; Clear screen/home
HOME        = $e581             ; Home text
TCOLOR      = $0286             ; Text color
PRTSTR      = $cb1e             ; Print from data (Y,A)
CASECT      = $0291             ; Disable Commodore case
PRTFIX      = $ddcd             ; Decimal display routine (A,X)
CHROUT      = $ffd2             ; Output one character
TIME_L      = $a2               ; Jiffy counter low
TIME_M      = $a1               ; Jiffy counter middle  

; Game Memory
CUR_NOTE    = $03               ; Current wAxScore note
CURSOR      = $f9               ; Cursor (2 bytes)
PLAYER      = $fb               ; Player location (2 bytes)
BASE_COL    = $fd               ; Base color pointer (2 bytes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Data
SCORE:      .byte $00,$00       ; Score
HISCORE:    .byte $00,$00       ; High Score
LEVEL:      .byte $00           ; Level
HAVEMED:    .byte $00           ; Have med pack flag
LAST_BASE   .byte $00,$00       ; Last base location
DIR:        .byte $00           ; Direction
TRAVEL:     .byte $02           ; Horizontal (RIGHT/LEFT) travel direction
SUBCHAR:    .byte $2e           ; Character of player
JOYREG:     .byte $00           ; Joystick register
SUBSPEED:   .byte $00           ; Sub speed (lower is faster)
SEALIFE_CD: .byte $00           ; Sealife movement countdown
OXYGEN:     .byte $00           ; Oxygen
SHOWSCORE:  .byte $00           ; Score Bar show flag
O2_CD:      .byte $00           ; Oxygen depletion countdown
PLAY_FL:    .byte $00           ; Play flag - bit 7 when game is going on

; Music Player Memory                 
TEMPO:      .byte $04           ; Tempo (jiffies per eighth note)
COUNTDOWN:  .byte $00           ; Tempo countdown
PLAY_FLAG:  .byte $00           ; Play flag if bit 7 is set
LEGATO:     .byte $00           ; Legato if bit 7 is set
ENABLE_MUS  .byte $01           ; Music player is enabled if 1

; Sound Effects Player Memory
FX_REG      .byte $00           ; Current effect frequency register
FX_LEN:     .byte $00           ; Effect length for current effect
FX_COUNT:   .byte $00           ; Effect countdown for current effect
FX_DIR:     .byte $00           ; Effect direction ($00 if left, $80 is right)
FX_SPEED:   .byte $00           ; Effect countdown reset value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Startup:    jsr SetupHW         ; Set up hardware           
            lda #$00            ; Initialize high score
            sta HISCORE         ; ,,
            sta HISCORE+1       ; ,,
           
; Welcome Screen
; Show intro, set level, then show manual page      
Welcome:    clc                 ; Disable the playing flag, in case we get
            ror PLAY_FL         ;   here from the NMI
            lda #110            ; Set background color
            sta BACKGD          ; ,,
            jsr wsStop          ; Stop wAxScore player
            lda #$00            ; Shut off all sounds
            sta VOICEL          ; ,,
            sta VOICEM          ; ,,
            sta VOICEH          ; ,,
            sta NOISE           ; ,,
            jsr CLSR            ; Clear screen
            lda #<Intro         ; Show Intro
            ldy #>Intro         ; ,,
            jsr PRTSTR          ; ,,
            lda #120
            sta CURSOR
            lda #$1e
            sta CURSOR+1
            lda #$1c
            ldy #$07
            jsr DrawChar
            ldx #RIGHT
            jsr MoveCursor
            lda #$1d
            ldy #$07
            jsr DrawChar
            ldx #DOWN
            jsr MoveCursor
            lda #$1e
            ldy #$07
            jsr DrawChar
            ldx #LEFT
            jsr MoveCursor
            lda #$1f
            ldy #$07
            jsr DrawChar
            jsr Wait4Fire
            lda #<Manual        ; Show the game manual
            ldy #>Manual        ; ,,
            jsr PRTSTR          ; ,,

; Start a new game            
Start:      jsr Wait4Fire
            jsr InitGame

; Main loop            
Main:       bit SHOWSCORE
            bpl control
            lda #$00
            sta SHOWSCORE
            jsr ScoreBar
control:    jsr Joystick        ; Read the joystick
            bne ch_fire         ; If no movement, draw the sub, reduce the
            jsr RSCursor        ;   speed
            lda SUBCHAR         ;   ,,
            ldy #CO_PLAYER      ;   ,,
            jsr DrawChar        ;   ,,
            lda #PL_SPEED       ;   ,,
            sta SUBSPEED        ;   ,,
            bne Main            ; Check the joystick again
ch_fire:    cmp #FIRE           ; Has fire been pressed?
            bne handle_dir      ; If not, handle a direction
            jsr wsToggle        ; Toggle music on fire
debounce:   jsr Joystick        ; Debounce fire button:
            cmp #FIRE           ;   Check joystick for fire button release
            beq debounce        ;   before returning to Main
            jmp Main            ;   ,,
handle_dir: jsr Move            ; Move the sub
            jsr CheckMed        ; Are we on the med pack?
            jmp Main
            
 ; Custom ISR for music player and day counting
ISR:        bit PLAY_FL         ; If the game is over, don't do anything
            bpl isr_r           ;   in this routine
            jsr FXService       ; Service sound effect
            jsr wsService       ; Service music
            dec SEALIFE_CD
            bne o2_deplete
            lda SEALIFE_GEN     ; Reset countdown timer
            sta SEALIFE_CD      ; ,,
            lda CURSOR          ; Save the cursor position while in the ISR
            pha                 ; ,,
            lda CURSOR+1        ; ,,
            pha                 ; ,,
            jsr Swim
            pla                 ; Restore the cursor position
            sta CURSOR+1        ; ,,
            pla                 ; ,,
            sta CURSOR          ; ,,
o2_deplete: dec O2_CD
            bne flash_base
            dec OXYGEN          ; Deplete oxygen
            sec                 ; ,,
            ror SHOWSCORE       ; ,,
            lda #O2_RATE        ; Reset the countdown
            sta O2_CD           ; ,,
flash_base: lda TIME_L
            ror
            bcc isr_r
            ldx #$00
            lda (BASE_COL,x)
            tay
            iny
            tya
            and #$07
            sta (BASE_COL,x)
isr_r:      jmp IRQ           
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GAME MECHANICS ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                        
CheckMed:   bit HAVEMED         ; If the med pack is already picked up,
            bmi check_r         ;   no need to check it
            lda CURSOR+1
            cmp #>SCREEN
            bne check_r
            lda CURSOR
            cmp #155
            bne check_r
            ror HAVEMED         ; Set med pack flag
            jsr ClrMedPack      ; Clear the med pack icon
            lda #$00            ; Play sound effect for med pack pickup
            jsr FXLaunch        ; ,,
            lda OXYGEN          ; Add some oxygen as a bonus
            clc                 ;   ,,
            adc #O2_ADD         ;   ,,
            sta OXYGEN          ;   ,,
            cmp #O2_START       ; Ensure that the oxygen doesn't exceed the
            bcc check_r         ;   starting level
            lda #O2_START       ;   ,,
            sta OXYGEN          ;   ,,
            lda #PICKUP         ; Add points for pickup and display
            jsr ScoreBar        ;   score bar update
            sec                 ; Set show score flag
            ror SHOWSCORE       ; ,,
check_r:    rts

; Set and Clear Med Pack
SetMedPack: lda #MEDPACK
            ldy #$0a
            jmp DrawMed            

ClrMedPack: lda #LANDCHAR
            ldy #CO_SKY
            jmp DrawMed                        

; Draw Med Pack Location
; Used by both SetMedPack and ClrMedPack
DrawMed:    tax                 ; Save A in X while we mess with A
            lda CURSOR          ; Save the current cursor
            pha                 ; ,,
            lda CURSOR+1        ; ,,
            pha                 ; ,,
            lda #133            ; Set the cursor to the med pack
            sta CURSOR          ; ,,
            lda #>SCREEN        ; ,,
            sta CURSOR+1        ; ,,
            txa                 ; Put the original A back
            jsr DrawChar        ; Draw the character (color was in Y)
            pla                 ; Restore the cursor back to
            sta CURSOR+1        ;   its original location
            pla                 ;   ,,
            sta CURSOR          ;   ,,
            rts
 
; Touch Base
; The base has been encountered, so determine what to do
TouchBase:  bit HAVEMED         ; Does the player have the med pack?
            bmi have_med        ; If not, play the "missing med pack"
            lda #$04            ;   sound  
            jmp FXLaunch        ;   ,,
have_med:   clc                 ; Clear the med pack flag
            ror HAVEMED         ; ,,
            jsr SetMedPack      ; Show the med pack at the lighthouse
            lda #DELIVERY       ; Add the delivery to the score
            clc                 ; And add remaining oxygen as a bonus
            adc OXYGEN          ; ,,
            jsr ScoreBar        ; ,,
            jsr LevelUp         ; Move the base and advance the level
            lda #$01            ; Launch sound effect for delivery
            jmp FXLaunch        ; ,,

            
; Level Up
; Move Sea Base to next level            
LevelUp:    sei
            inc LEVEL
            ldx LEVEL
            cpx #$01
            beq new_base
            lda LAST_BASE
            sta CURSOR
            lda LAST_BASE+1
            sta CURSOR+1
            lda #TOPLAND        ;   draw so that it doesn't get set
            ldy #08             ;   to the wrong color by the ISR
            jsr DrawChar        ;   ,,
            ldx LEVEL
new_base:   cpx #$08            ; Base position maxes out at level 8
            bcc get_pos         ; ,,
            ldx #$08            ; ,,
get_pos:    lda LevelPos,x      ; Get base postion from the table
            sta $02             ; Save for horizontal movement iterator
drop_base:  lda #154
            sta CURSOR
            lda #>SCREEN
            sta CURSOR+1
-loop:      ldx #RIGHT
            jsr MoveCursor
            dec $02
            bne loop
ch_floor:   jsr GetChar
            cmp #TOPLAND
            beq draw_base
            ldx #DOWN
            jsr MoveCursor
            jmp ch_floor
draw_base:  lda CURSOR
            sta LAST_BASE
            sta BASE_COL
            lda CURSOR+1
            sta LAST_BASE+1
            lda #$97
            sta BASE_COL+1
            lda #SEABASE
            ldy #$01
            jsr DrawChar
            cli
            rts
            
; Swim
; Move each fish to the left                   
Swim:       ldy #$00
-loop:      lda $1e00,y
            cmp #FISH
            beq MoveFishL
            cmp #FISH+1
            beq MoveFishL
high_f0:    lda $1f00,y
            cmp #FISH
            beq MoveFishH
            cmp #FISH+1
            beq MoveFishH
next_cell:  iny
            bne loop
            ; Fall through to Spawn
  
; Spawn
; Generate new fish
Spawn:      jsr BASRND          ; Get random number, for the placement of
            lda RNDNUM          ;   a fish on the right-hand side of the
            and #%00000111      ;   screen
            tay                 ;   ,,
            iny                 ;   ,,
            sty $02             
            lda #153
            sta CURSOR
            lda #>SCREEN
            sta CURSOR+1
-loop:      ldx #DOWN
            jsr MoveCursor
            dec $02
            bne loop
            lda #FISH
            ldy #CO_FISH
            jsr DrawChar
            rts

; Move Fish
; To a location to the left of its current location            
MoveFishL:  ldx #$1e
            .byte $3c           ; Skip word
MoveFishH:  ldx #$1f
            stx CURSOR+1
            sty CURSOR
            cmp #FISH
            beq half_move
            lda #$20            ; Replace fish with water
            ldy #$06            ; ,,
            jsr DrawChar        ; ,,
            ldx #LEFT           ; Move to the fish's new candidate position
            jsr MoveCursor      ; ,,
            jsr GetChar         ; What's there now?
            cmp #FISH           ; Is it a fish or sub character?
            bcs redraw          ; If so, redraw
            cmp #$20            ; A fish may only move to an unoccupied
            bne fish_exit       ;   position
            lda #FISH           ; The position to the left is unoccupied, so
            ldy #CO_FISH        ;   move the fish here
            jsr DrawChar        ;   ,,
fish_exit:  ldy CURSOR          ; Restore Y to its previous value, since it's
            iny                 ;   an iterator within the caller
            jmp next_cell 
redraw:     ldx #RIGHT
            jsr MoveCursor              
half_move:  lda #FISH+1
            ldy #CO_FISH
            jsr DrawChar
            ldy CURSOR
            jmp next_cell         
                                                                                           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Wait for Fire
Wait4Fire:  jsr Joystick        ; Wait for fire to be released
            cpx #FIRE           ; ,,
            beq Wait4Fire       ; ,,
wait_fire:  jsr Joystick        ; Wait for the fire button
            cpx #FIRE           ; ,,
            bne wait_fire       ; ,,
            rts
            
; Read the Joystick
; Return the direction in A
; 1=North, 2=East, 3=South, 4=West, 5=Fire, 0=None
Joystick:   lda VIA1PA          ; Read VIA1 port
            and #$3c            ; Keep track of bits 2,3,4,5
            sta JOYREG
            lda VIA2PB          ; Combine with read of bit 7
            and #$80            ;   from VIA2-B
            ora JOYREG
            eor #$bc            ; Flip each joystick bit in the
                                ;   combined read byte, so that
                                ;   on = 1
            sta JOYREG          ; Store temporary direction
            ldx #$05            ; Check five directions (5=fire)
-loop:      lda JoyTable,x      ; Is the joystick pointed in the
            bit JOYREG          ;   direction indexed by X?
            bne found_dir       ; If so, set that as the joystick direction
            dex                 ; Loop back until found, or 0
            bne loop            ; ,,
found_dir:  txa
            rts         

; Delay A Jiffies
Delay:      clc
            adc TIME_L
-loop:      cmp TIME_L
            bne loop
            rts  

; Score
; Add score in Accumulator (positive or negative), then show
; score bar
ScoreBar:   sei                 ; Stop interrupt during score bar display
            clc
            adc SCORE
            sta SCORE
            lda #$00
            adc SCORE+1
            sta SCORE+1
            lda HISCORE+1       ; Is the last score greater than
            cmp SCORE+1         ;   the high score?
            bcc new_hs          ;   ,,
            bne show_score      ;   ,,
            lda SCORE           ;   ,,
            cmp HISCORE         ;   ,,
            bcc show_score      ;   ,,
new_hs:     lda SCORE           ; A new high score has been
            sta HISCORE         ; achived; update high score
            lda SCORE+1         ; ,,
            sta HISCORE+1       ; ,,                
show_score: jsr ShowScore
            lda #O2_CHAR
            jsr CHROUT
            lda OXYGEN          ; Get the current oxygen level
            beq GameOver        ; End game if oxygen goes to 0
            sta $02             ; Oxygen counter
            cmp #$06            ; If the oxygen gets low, make the
            bcs o2_bar          ;   oxygen bar red
            lda #$9c            ;   ,,
            jsr CHROUT          ;   ,,
            lda #$03            ; Emit warning sound when O2 is low
            jsr FXLaunch        ; ,,
            lda $02             ; Restore the oxygen temp register
o2_bar:     cmp #$04
            bcc finish          ; If oxygen is under 4, then wrap it up
            lda #O2_CHAR+1
            jsr CHROUT
            lda $02            
            sec
            sbc #$04
            sta $02
            jmp o2_bar
finish:     lda #$3f
            sec
            sbc $02
            jsr CHROUT          ; Add the end of the oxygen meter
            cli                 ; Restart interrupt
            rts
            
GameOver:   jsr wsStop          ; Stop the music
            jsr ShowScore
            lda #<HiTx
            ldy #>HiTx
            jsr PRTSTR
            ldx HISCORE
            lda HISCORE+1
            jsr PRTFIX
            lda #<GameOverTx
            ldy #>GameOverTx
            jsr PRTSTR
            lda #$08
            sta VOLUME
            jsr RSCursor
            jsr GetChar
            ldy #$00
            jsr DrawChar
            lda #$00
            ldx #$00
            sta (BASE_COL,x)    ; Set base color to black
            lda #$02
            jsr FXLaunch
            lda #$40
            jsr Delay
            clc                 ; Clear the game playing flag
            ror PLAY_FL         ; ,,
            jmp Start
            
; Show Score
; For score bar and game over            
ShowScore:  lda #<ScoreTx
            ldy #>ScoreTx
            jsr PRTSTR
            ldx SCORE
            lda SCORE+1
            jsr PRTFIX
            lda #" "
            jmp CHROUT
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MOVEMENT ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
; Move player in direction specified in A (UP, DOWN, RIGHT, LEFT)
Move:       cmp #DOWN           ; If moving up or down, there's no need to check 
            beq not_turn        ;   for a directional change
            cmp #UP             ;   ,,
            beq not_turn        ;   ,,
            cmp TRAVEL          ; Has direction of travel (RIGHT, LEFT) changed?
            beq not_turn        ; If so, do a turning maneuver instead of
            jmp Turn            ;   moving
not_turn:   sta DIR             ; Set the direction
            jsr PickChar        ; Set Cursor to the right source character
            ldy #$07            ; Initialize the source and destination
-loop:      lda #$00            ;   characters in the character set
            sta BITMAP_D,y      ;   ,,
            lda (CURSOR),y      ;   ,,
            sta BITMAP_S,y      ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            jsr RSCursor
            lda #CHAR_S         ; Draw the bitmap source character at the
            ldy #CO_PLAYER      ;   player location
            jsr DrawChar        ;   ,,
            ldx DIR             ; Move the cursor
            jsr MoveCursor      ; ,,
            jsr GetChar         ; Get the character at the Cursor location
            cmp #SEABASE        ; If it's a base, handle it
            bne is_clear        ; ,,
            jmp TouchBase       ; ,,
is_clear:   cmp #$20            ; If there's a non-space, do not move
            beq do_move         ; ,,
            lda #PL_SPEED       ; If an obstacle has been struck (fish, land)
            sta SUBSPEED        ;   reduce speed
            rts
do_move:    lda #CHAR_D         ; Draw the bitmap destination character at the
            ldy #CO_PLAYER      ;   destination location with player color
            jsr DrawChar        ;   ,,
            ldx #$08            ; Move the character 8 pixels in the selected
-loop:      jsr MoveBitmap      ;   direction, with a short delay between each
            lda #$c0            ;   pixel. The engine sound is a pulse
            sta VOICEL          ;   followed by silence.
            lda #01             ;   ,,
            jsr Delay           ;   ,,
            lda #$00            ;   ,,
            sta VOICEL          ;   ,,
            lda SUBSPEED        ;   ,,
            jsr Delay           ;   ,,
            dex                 ;   ,,
            bne loop            ;   ,,
            lda #$20            ; Replace the character with a space
            ldx #$00            ;   ,,
            sta (PLAYER,x)      ;   ,,
            jsr UDPlayer        ; Update player location
            lda #$00            ; Set DIR to 0
            sta DIR             ; ,,
            dec SUBSPEED        ; Pick up speed with each movement until the
            lda SUBSPEED        ;   sub is turned, or until the joystick is
            cmp #$01            ;   released.
            bne move_r          ; But enforce a maximum speed
            lda #$02            ; ,,
            sta SUBSPEED        ; ,,
move_r:     rts

; Turn
; Perform a turning maneuver in the direction (RIGHT, LEFT) in Accumulator
Turn:       sta TRAVEL          ; Switch the direction of travel
            lda #PL_SPEED       ; Reset the sub speed to slowest
            sta SUBSPEED        ; ,,
            jsr RSCursor
next_turn:  lda SUBCHAR         ; Start turn with the current character
            ldy #CO_PLAYER      ; ,,
            jsr DrawChar        ; ,,
            lda #04             ; Delay so the turn can be seen
            jsr Delay           ; ,,
            lda TRAVEL          ; Check the new directon of travel
            cmp #LEFT           ; If the new direction is left, then decrement
            bne turn_right      ;   the sub character until it gets to $29
            dec SUBCHAR         ;   ,,
            lda #$29            ;   ,,
            cmp SUBCHAR         ;   ,,
            bne next_turn       ;   ,,
            rts                 ; Leftward turn is done
turn_right: inc SUBCHAR         ; If the new direction is right, then increment
            lda #$2d            ;   the sub character until it gets to $2d
            cmp SUBCHAR         ;   ,,
            bne next_turn       ;   ,,
turn_r:     rts                 ; Rightward turn is done

; Move Bitmap
MoveBitmap: lda DIR
            cmp #UP
            beq mv_north
            cmp #RIGHT
            beq mv_east
            cmp #DOWN
            beq mv_south
            ; Fall through to mv_west
mv_west:    ldy #$07            ; For each byte
-loop:      lda BITMAP_D,y      ; Shift the destination character left
            asl                 ; ,,
            sta BITMAP_D,y      ; ,,
            lda BITMAP_S,y      ; Shift the source character left
            asl                 ; ,,
            sta BITMAP_S,y      ; ,,
            lda #$00            ; If the high bit of the source byte was 1,
            adc BITMAP_D,y      ;   then add 1 to the destination byte
            sta BITMAP_D,y      ;   and save it
            dey
            bpl loop
            rts
mv_east:    ldy #$07            ; For each byte
-loop:      lda BITMAP_D,y      ; Shift the destination character right
            lsr                 ; ,,
            sta BITMAP_D,y      ; ,,
            lda BITMAP_S,y      ; Shift the source character right
            lsr                 ; ,,
            sta BITMAP_S,y      ; ,,
            bcc east_next       ; 
            lda BITMAP_D,y      ; If the low bit of the source byte was 1,
            ora #$80            ;   then set the high bit of the destination
            sta BITMAP_D,y      ;   and save it
east_next:  dey
            bpl loop
            rts
mv_north:   ldy #$00
-loop:      lda BITMAP_D+1,y    ; Copy each byte from the byte below it
            sta BITMAP_D,y      ; ,,
            iny
            cpy #$07
            bne loop
            lda BITMAP_S        ; Move the top byte of the source character to
            sta BITMAP_D+7      ;   the bottom of the destination character
            ldy #$00
-loop:      lda BITMAP_S+1,y
            sta BITMAP_S,y
            iny
            cpy #$07
            bne loop
            lda #$00            ; Clear the bottom byte of the source character
            sta BITMAP_S+7      ; ,,
            rts
mv_south:   ldy #$06            ; Copy each byte from the byte above it
-loop:      lda BITMAP_D,y      ; ,,
            sta BITMAP_D+1,y    ; ,,
            dey
            bpl loop
            lda BITMAP_S+7      ; Copy the bottom of the source to the top of
            sta BITMAP_D        ;   the destination
            ldy #$06
-loop:      lda BITMAP_S,y
            sta BITMAP_S+1,y
            dey
            bpl loop                    
            lda #$00            ; Clear the top byte of the source character
            sta BITMAP_S        ; ,,               
            rts                   

; Reset Cursor to Player
RSCursor:   lda PLAYER
            sta CURSOR
            lda PLAYER+1
            sta CURSOR+1
            rts

; Update Player to Cursor
UDPlayer:   lda CURSOR
            sta PLAYER
            lda CURSOR+1
            sta PLAYER+1
            rts
            
; Get Character at Cursor            
GetChar:    ldx #$00
            lda (CURSOR,x)
            rts            

; Draw Character at Cursor
; A is the character, Y is the color           
DrawChar:   ldx #$00
            sta (CURSOR,x)
            ; Fall through to SetColor

; Set Color at Cursor
; Use color code in Y
SetColor:   lda CURSOR+1
            pha
            clc
            adc #$78
            sta CURSOR+1
            ldx #$00
            tya
            sta (CURSOR,x)
            pla
            sta CURSOR+1
            rts

; Move Cursor
; In the direction specified by X
MoveCursor: lda DirTable,x
            pha
            and #%10000000      ; Extend the sign to 16 bits
            beq sign            ; ,,
            ora #$ff            ; ,,
sign:       tay                 ; ,,
            pla                 ; Get original direction value
            clc                 ; Add (or subtract) the signed direction
            adc CURSOR          ;   and update CURSOR
            sta CURSOR          ;   ,,
            tya                 ;   ,,
            adc CURSOR+1        ;   ,,
            sta CURSOR+1        ;   ,,
            rts            
  
; Pick Character
; Depending on which direction the player is moving, a different character
; will be loaded into the source bitmap character. Direction is in A.
PickChar:   lda TRAVEL
            cmp #LEFT
            beq pick_left
            lda #<PL_RIGHT
            sta CURSOR
            lda #>PL_RIGHT
            sta CURSOR+1
            rts
pick_left:  lda #<PL_LEFT
            sta CURSOR
            lda #>PL_LEFT
            sta CURSOR+1
            rts            
                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETUP ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Setup Hardware
SetupHW:    lda TIME_L          ; Seed random number generator
            sta RNDNUM          ; ,,
            lda #$ff            ; Set custom character location
            sta VICCR5          ; ,,
            lda #$00            ; Initialize sound registers
            sta VOICEM          ; ,,
            sta VOICEH          ; ,,
            sta VOICEL          ; ,,
            sta NOISE           ; ,,
            lda #$7f            ; Set DDR to read East
            sta VIA2DD          ; ,,
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,
            lda #TXTCOL         ; Set color of screen text, like
            sta TCOLOR          ;   intro, game over, score, etc.
            lda #<Welcome       ; Install the custom NMI (restart)
            sta NMINV           ; ,, 
            lda #>Welcome       ; ,,
            sta NMINV+1         ; ,,
            sei                 ; Install the custom ISR
            lda #<ISR           ; ,,
            sta CINV            ; ,,
            lda #>ISR           ; ,,
            sta CINV+1          ; ,,
            cli                 ; ,,
            rts
            
; Initialize Game
InitGame:   lda #SCRCOM         ; Set background color
            sta BACKGD          ; ,,
            jsr CLSR            ; Clear Screen
            lda #$88            ; Set volume and aux color
            sta VOLUME          ; ,,
            lda #$00            ; Initialize score and level
            sta SCORE           ; ,,
            sta SCORE+1         ; ,,
            sta LEVEL           ; ,,
            sta SHOWSCORE       ; Clear show score flag
            sta HAVEMED         ; Clear med pack flag
            lda #O2_START       ; Initialize oxygen
            sta OXYGEN          ; ,,
            lda #179            ; Initialize player position
            sta PLAYER          ; ,,
            lda #$1e            ; ,,
            sta PLAYER+1        ; ,,
            lda #RIGHT          ; Initialize direction
            sta TRAVEL          ; ,,
            lda #$2d            ; ,,
            sta SUBCHAR         ; ,,
            lda #132            ; Seven rows of sky
            sta $02             ; ,,
            lda #<SCREEN+22     ; Set cursor to second line
            sta CURSOR          ; ,,
            lda #>SCREEN        ; ,,
            sta CURSOR+1        ; ,,
next_sky:   lda #LANDCHAR       ; Draw some sky
            ldy #CO_SKY         ; ,,
            jsr DrawChar        ; ,,
            ldx #RIGHT          ; Advance to next location
            jsr MoveCursor
sky_adv:    dec $02             ; Until done
            bne next_sky        ; ,,
            lda CURSOR          ; Draw Lighthouse
            sbc #43             ; ,,
            sta CURSOR          ; ,,
            lda #$23            ; ,,
            ldy #$03            ; ,,
            jsr DrawChar        ; ,,
            ldx #DOWN           ; ,,
            jsr MoveCursor      ; ,,
            lda #$24            ; ,,
            ldy #$03            ; ,,
            jsr DrawChar        ; ,,
            lda #16             ; Draw land edge
            sta $02             ; ,,
next_land:  ldx #DOWN           ; ,,
            jsr MoveCursor      ; ,,
            lda #LANDCHAR       ; ,,
            ldy #$0f            ; ,,
            jsr DrawChar        ; ,,
            dec $02             ; ,,
            bne next_land       ; ,,
            lda #21             ; Draw ocean floor
            sta $02             ; ,,
next_floor: ldx #RIGHT          ; ,,
            jsr MoveCursor      ; ,,
            lda CURSOR          ; ,,
            pha                 ; ,,
            lda CURSOR+1        ; ,,
            pha                 ; ,,
            jsr BASRND          ; ,,
            lda RNDNUM          ; ,,
            and #%00000111      ; ,,
            sta $03             ; ,,
            inc $03             ; ,,
next_reef:  lda #LANDCHAR       ; ,,
            ldy #$0f            ; ,,
            jsr DrawChar        ; ,,
            ldx #UP             ; ,,
            jsr MoveCursor      ; ,,
            dec $03             ; ,,
            bpl next_reef       ; ,,
            ldx #DOWN           ; ,,
            jsr MoveCursor      ; ,,
            lda #TOPLAND        ; ,,
            ldy #$0f            ; ,,
            jsr DrawChar        ; ,,
            pla                 ; ,,
            sta CURSOR+1        ; ,,
            pla                 ; ,,
            sta CURSOR          ; ,,
            dec $02             ; ,,
            bne next_floor      ; ,,
            jsr SetMedPack      ; Turn on MedPack
            jsr LevelUp         ; Move Seabase to level 1
            lda #$00            ; Initialize the score bar
            jsr ScoreBar        ; ,,
            sec                 ; Show score bar immediately
            ror SHOWSCORE       ; ,,
            lda #SEALIFE_GEN    ; Set sealife generation countdown
            sta SEALIFE_CD      ; ,,
            lda #O2_RATE        ; Set oxygen depletion countdown
            sta O2_CD           ; ,,
            jsr wsReset         ; Reset the music score
            jsr wsPlay          ; Start the music
            sec                 ; Set the game playing flag
            ror PLAY_FL         ; ,,
            cli                 ; Start the interrupt
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WAXSCORE IRQ PLAYER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reset Score to Start
wsReset:    lda #<Theme
            sta CUR_NOTE
            lda #>Theme
            sta CUR_NOTE+1
            lda #$00
            sta COUNTDOWN
            sta LEGATO
            rts

; Begin Playing
wsPlay:     lda ENABLE_MUS
            beq play_r
            sec
            ror PLAY_FLAG
play_r:     rts
            
; Stop Playing
wsStop:     clc
            ror PLAY_FLAG
            sta VOICEM
            rts

; Toggle Play
wsToggle:   bit PLAY_FLAG
            bmi wsStop
            bpl wsPlay
            
; Service Routine           
wsService:  bit PLAY_FLAG
            bpl svc_r
            lda COUNTDOWN
            beq fetch_note
            cmp #$03            ; Handle legato playing by stopping
            bcs keep_on         ;   a non-legato note with a few
            bit LEGATO          ;   jiffies before the end of the
            bmi keep_on         ;   duration. Legato notes are kept on
            lda #$00
            sta VOICEM
keep_on:    dec COUNTDOWN
            lda VOLUME
            and #$0f
            cmp #$08
            beq svc_r
            inc VOLUME
            rts
fetch_note: ldx #$00
            stx COUNTDOWN       ; Initialize countdown
            lda (CUR_NOTE,x)
            beq eos             ; End of score
            tay                 ; Y holds the full note data
            and #$0f            ; Mask away the duration
            cmp #$0f            ; Is this a effect?
            beq wsEffect
            tax
            lda Oct0,x
            sta VOICEM
            lda DIR             ; If the ship is in motion, don't
            bne keep_vol        ;   mess with the volume
            tya                 ; If the duration is shorter than a
            cpy #$30            ;   dotted quarter note, don't mess
            bcc keep_vol        ;   with the volume
            lda VOLUME          ; Start at minimum volume; leave aux
            and #$f0            ;   color alone
            sta VOLUME          ;   ,,
keep_vol:   tya
            and #$f0            ; Mask away the note index
            lsr
            lsr
            lsr
            lsr
            ldy TEMPO
-loop:      clc
            adc COUNTDOWN
            sta COUNTDOWN
            dey
            bne loop
NextNote:   inc CUR_NOTE
            bne svc_r
            inc CUR_NOTE+1
svc_r:      rts
eos:        jsr wsReset
            rts       

; Apply Effect
; The effect command is in Y            
wsEffect:   cpy #NO_EFFECT      ; Handle Placeholder
            beq effect_r        ; ,,
ch_legato:  cpy #LEGATO_ON      ; Handle legato
            bne ch_leg_off      ; ,,
            lda #$80            ; ,,
            sta LEGATO          ; ,,
            jmp NextNote
ch_leg_off: cpy #LEGATO_OFF     ; Handle legato off
            bne effect_r        ; ,,
            lsr LEGATO          ; ,,
effect_r:   jmp NextNote                       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; EFFECTS SERVICE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
; Play Next Sound Effect
; Rotates the 8-bit sound effect register and
; plays the pitch      
FXService:  lda FX_LEN          ; Has the sound been launched?
            beq fx_end          ; If unlaunched, kill voice and return
            dec FX_LEN          ; Decrement both length
            dec FX_COUNT        ;   and countdown
            bne fx_r            ; If countdown has elapsed,
            lda FX_SPEED        ;   reset it with the current effect speed
            sta FX_COUNT        ;   ,,
            bit FX_DIR          ; Rotate the register, based on the direction
            bmi fx_right        ;   specified by the direction flag
fx_left:    lda #$00
            asl FX_REG          ; Rotate the register left if flag = $00
            adc FX_REG          ; If carry was set, set bit 0
            jmp fx_update       ; Update and play the new frequency
fx_right:   lsr FX_REG          ; Rotate the register right if flag = $80
            lda FX_REG          ; ,,
            bcc fx_play         ; If carry was set, set bit 7
            lda #%10000000      ; ,,
            ora FX_REG          ; ,,
fx_update:  sta FX_REG          ; ,,
fx_play:    ora #$80            ; Gate the high voice
fx_end:     sta FX_VOICE        ; ,,
fx_r:       rts      
        
; Launch Sound Effect
; Preparations
;     A - The sound effect index
FXLaunch:   sei                 ; Don't play anything while setting up
            asl                 ; Each effect has two bytes in the table
            tax
            lda FXTable,x       ; Get the register byte
            sta FX_DIR          ; Set the direction (only bit 7 will be used)
            and #$7f            ; Mask away the direction bit to get the 7-bit
            sta FX_REG          ;   frequency
            lda FXTable+1,x     ; Get the length byte
            tax                 ;   and preserve it
            and #$f0            ; Length is in bits 4-7 of the length byte
            sta FX_LEN          ;  ,,
            txa
            and #$0f            ; Speed (jiffies per rotation) is in the low
            sta FX_SPEED        ;   nybble of the length byte
            sta FX_COUNT        ;   ,,
            cli                 ; Go! 
            rts
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Intro:      .asc $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,"     S U B  M E D"
            .asc $0d,$0d,$0d,"  JASON JUSTIAN 2020",$0d,$0d,$0d
            .asc "      PRESS FIRE",$00

; Manual Text            
Manual:     .asc $93,$05,"AVAST",$0d,$0d
            .asc "OUR UNDERSEA BASE IS",$0d,$0d
            .asc "IN DANGER",$0d,$0d,$0d
            .asc $9e,$29,$05," NAVIGATE YOUR SUB",$0d,$0d
            .asc $1c,$21,$05," PICK UP A MED PACK",$0d,$0d
            .asc "  FROM THE LIGHTHOUSE",$0d,$0d
            .asc "@ DELIVER TO THE BASE",$0d,$0d
            .asc $1e,$28,$05," AVOID SEA LIFE ",$0d,$0d
            .asc $3a," WATCH YOUR OXYGEN ",$0d,$0d,$0d
            .asc "            AGENT ANZU",$00

; Score Bar
ScoreTx:    .asc $13,$05," SCORE ",$00
HiTx:       .asc "HIGH ",$00
GameOverTx: .asc $13,$11,$11,$11,$11,$11,$11,$11,$11
            .asc $1d,$1d,$1d,$1d,$1d,$1d
            .asc $05," GAME OVER ",$00
            
; Direction Tables                       
DirTable:   .byte 0,$ea,$01,$16,$ff
JoyTable:   .byte 0,$04,$80,$08,$10,$20            ; Corresponding direction bit
         
; Level Advancement Table           
LevelPos:   .byte $01,$01,$02,$04,$06,$09,$0d,$10,$14           
   
; Degree to Note Value
; Determined with electronic tuner
Oct0:       .byte 0,194,197,201,204,207,209,212,214,217,219,221,223,225
   
; Music in wAxScore format 
Theme:      .byte $2f,$2f,$26,$16,$18,$1a,$18,$16
            .byte $1a,$28,$2d,$2d,$2d,$28,$18,$1a
            .byte $1b,$1a,$18,$1b,$2a,$26,$23,$21 
            .byte $26,$16,$18,$1a,$18,$16,$1a,$28
            .byte $2d,$2d,$28,$2b,$2b,$1a,$1b,$1a
            .byte $3f,$18,$16,$11,$18,$11,$1a,$11,$2d,$4f

Theme2:     .byte $2f,$2f,$26,$16,$18,$1a,$18,$16
            .byte $1a,$28,$2d,$2d,$2d,$28,$18,$1a
            .byte $1b,$1a,$18,$1b,$2a,$26,$23,$21 
            .byte $26,$16,$18,$1a,$18,$16,$1a,$28
            .byte $2d,$2d,$28,$2b,$2b,$1a,$1b,$1a
            .byte $18,$66

Theme3:     .byte $3f,$1b,$1a,$4f,$1b,$16,$13,$16,$1b,$16 
            .byte $13,$16,$18,$16,$14,$13,$24,$16 
            .byte $18,$2d,$21,$1d,$1c,$1b,$18,$66 
            .byte $1b,$1a,$1b,$16,$13,$16,$1b,$1a 
            .byte $18,$16,$24,$26,$28,$16,$18,$1a 
            .byte $1b,$18,$1b,$1d,$1b,$1a,$1d,$6b

Theme4:     .byte $3f,$1b,$1a,$1b,$16,$13,$16,$1b,$16 
            .byte $13,$16,$18,$16,$14,$13,$24,$16 
            .byte $18,$2d,$21,$1d,$1c,$1b,$18,$66 
            .byte $1b,$1a,$1b,$16,$13,$16,$1b,$1a 
            .byte $18,$16,$24,$26,$28,$16,$18,$1a 
            .byte $1b,$18,$1b,$1d,$1b,$1a,$1d,$8b,$4f

            .byte $00
            
; Sound effects for the sound effects player
; Each effect has four parameters (DFFFFFFF LLLLSSSS)
;   (1) Bit 7 (D) of the first byte is the direction
;       * Unset=shift left, or a rising sound 
;       * Set=shift right, or a falling sound
;   (2) Bits 0-6 (F) of the first byte is the frequency register
;   (3) High nybble of the second byte (L) is the length in jiffies x 16
;       * Between approx. 1/4 sec and 4 sec in length
;   (4) Low nybble of second byte (S) is speed in jiffies
FXTable:    .byte $8a,$13       ; 0- Med Pack Pickup
            .byte $07,$34       ; 1- Med Pack Delivery
            .byte $f0,$26       ; 2- Game Over
            .byte $55,$24       ; 3- Low O2 Warning
            .byte $87,$11       ; 4- Missing Med Pack
           
; Padding to 3583 bytes
Padding:    .asc "2020 JASON JUSTIAN",$0d
            .asc "JJUSTIAN@GMAIL.COM",$0d
            .asc "BEIGEMAZE.COM/VICLAB",$0d
            .asc "RELEASED UNDER CREATIVE COMMONS",$0d
            .asc "ATTRIBUTION-NONCOMMERCIAL 4.0",$0d
            .asc "INTERNATIONAL PUBLIC LICENSE",$0d
            .asc "--------------------------------------------",$00
            .asc "ALL WORK AND NO PLAY MAKES JACK A DULL BOY",$00
            .asc "ALL WORK AND NO PLAY MAKES JACK A DULL BOY",$00
            .asc "ALL WORK AND NO PLAY MAKES JACK A DULL BOY",$00
            .asc "ALL WORK AND NO PLAY MAKES JACK A DULL BOY",$00
            .asc "ALL WORK AND NO PLAY MAKES JACK A DULL BOY",$00
            .asc "ALL WORK AND NO PLAY MAKES JACK A DULL BOY",$00
            .asc "ALL WORK AND NO PLAY MAKES JACK A DULL BOY",$00
            .asc "ALL WORK AND NO PLAY MAKES JACK A DULL BOY",$00
            .asc "ALL WORK AND NO PLAY MAKES JACK A DULL BOY",$00
            .asc "ALL WORK AND NO PLAY MAKES JACK A DULL BOY",$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CUSTOM CHARACTER SET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The character set must start at $1C00. If you change anything
; anywhere, you must account for this. The easiest way is to use
; padding bytes immediately before this character data.
;
; The easiest way to tell if you've done this right is to
; make sure that the object code is exactly 3583 bytes. This is
; a reliable method as long as you don't add anything AFTER this
; character data.
;                       
CharSet:    .byte $00,$00,$3c,$42,$4a,$89,$a9,$bd ; Sea Base
            .byte $00,$38,$44,$4c,$54,$64,$44,$44 ; A
            .byte $00,$78,$44,$44,$48,$54,$64,$78 ; B
            .byte $00,$38,$44,$40,$40,$44,$4c,$38 ; C
            .byte $00,$70,$48,$44,$44,$44,$48,$70 ; D
            .byte $00,$7c,$40,$40,$70,$40,$40,$7c ; E
            .byte $00,$7c,$40,$40,$70,$40,$40,$40 ; F
            .byte $00,$38,$44,$40,$44,$5c,$44,$38 ; G
            .byte $00,$44,$44,$44,$4c,$74,$44,$44 ; H
            .byte $00,$40,$40,$40,$40,$40,$40,$40 ; I
            .byte $00,$04,$04,$04,$04,$04,$44,$38 ; J
            .byte $00,$44,$44,$44,$48,$54,$64,$44 ; K
            .byte $00,$40,$40,$40,$40,$40,$40,$7c ; L
            .byte $00,$36,$49,$49,$49,$49,$49,$49 ; M
            .byte $00,$44,$64,$54,$4c,$44,$44,$44 ; N
            .byte $00,$38,$44,$44,$44,$44,$44,$38 ; O
            .byte $00,$78,$44,$44,$58,$60,$40,$40 ; P
            .byte $00,$38,$44,$44,$54,$4c,$44,$3a ; Q
            .byte $00,$78,$44,$44,$58,$70,$48,$44 ; R
            .byte $00,$38,$44,$60,$38,$0c,$44,$38 ; S
            .byte $00,$7c,$10,$10,$10,$10,$10,$10 ; T
            .byte $00,$44,$44,$44,$44,$44,$44,$38 ; U
            .byte $00,$44,$44,$44,$44,$24,$14,$0c ; V
            .byte $00,$49,$49,$49,$49,$49,$49,$36 ; W
            .byte $00,$44,$44,$44,$38,$44,$44,$44 ; X
            .byte $00,$44,$44,$44,$44,$38,$10,$10 ; Y
            .byte $00,$7c,$04,$08,$10,$20,$40,$7c ; Z
            .byte $55,$65,$65,$a9,$a9,$65,$65,$55 ; Med pack
            .byte $00,$03,$02,$02,$03,$03,$83,$c3 ; Title Sub I
            .byte $00,$00,$00,$00,$c0,$c0,$c0,$c0 ; Title Sub II
            .byte $fe,$ff,$ef,$c7,$ef,$fe,$fc,$f8 ; Title Sub III
            .byte $ff,$ff,$fd,$78,$3d,$3f,$63,$61 ; Title Sub IV
            .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Space
            .byte $00,$18,$18,$7e,$7e,$18,$18,$00 ; Med Pack (single color)
            .byte $3c,$42,$5a,$5a,$42,$3c,$c3,$81 ; <unused>
            .byte $e7,$bd,$81,$e7,$e7,$db,$db,$c3 ; Lighthouse Upper
            .byte $c3,$bd,$bd,$81,$81,$bd,$a5,$a5 ; Lighthouse Lower
            .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Sea floor (multi)
            .byte $3c,$3c,$ff,$ff,$ff,$ff,$ff,$ff ; Sea floor top (multi)
            .byte $00,$00,$0d,$1e,$0d,$00,$00,$00 ; Fish 1
            .byte $00,$00,$68,$f0,$68,$00,$00,$00 ; Fish 2
PL_LEFT:    .byte $18,$19,$7f,$d7,$fe,$78,$00,$00 ; Player left
            .byte $18,$1a,$3e,$6e,$7c,$38,$00,$00 ; Player turning left
            .byte $18,$18,$3c,$3c,$7e,$3c,$00,$00 ; Player facing
            .byte $18,$58,$7c,$76,$3e,$1c,$00,$00 ; Player turning right
PL_RIGHT:   .byte $18,$98,$fe,$eb,$7f,$1e,$00,$00 ; Player right
BITMAP_S:   .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Bitmap Source
BITMAP_D:   .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Bitmap Destination
            .byte $00,$38,$44,$44,$44,$44,$44,$38 ; 0
            .byte $00,$04,$0c,$04,$04,$04,$04,$04 ; 1
            .byte $00,$38,$44,$04,$08,$10,$20,$7c ; 2
            .byte $00,$38,$44,$04,$38,$04,$44,$38 ; 3
            .byte $00,$04,$0c,$14,$24,$7c,$04,$04 ; 4
            .byte $00,$7c,$40,$60,$18,$04,$04,$78 ; 5
            .byte $00,$38,$44,$60,$58,$44,$44,$38 ; 6
            .byte $00,$7c,$04,$04,$04,$04,$04,$04 ; 7
            .byte $00,$38,$44,$38,$44,$44,$44,$38 ; 8
            .byte $00,$38,$44,$44,$3c,$04,$44,$38 ; 9            
            .byte $06,$f2,$96,$94,$96,$90,$f0,$00 ; O2 Indicator
            .byte $00,$00,$00,$ff,$ff,$00,$00,$00 ; Oxygen level 4
            .byte $00,$00,$00,$fc,$fc,$00,$00,$00 ; Oxygen level 3
            .byte $00,$00,$00,$f0,$f0,$00,$00,$00 ; Oxygen level 2
            .byte $00,$00,$00,$c0,$c0,$00,$00,$00 ; Oxygen level 1
            .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Oxygen level 0            