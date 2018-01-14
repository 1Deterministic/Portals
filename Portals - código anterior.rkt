#lang racket
(require 2htdp/universe 2htdp/image)
(require racket/runtime-path)

(struct player (imagem posx posy) #:mutable)
(struct portal (posx-in posy-in posx-out posy-out) #:mutable)
(struct contato-cenario (posx-i posx-f posy))

(define portal-final-posx 20)
(define portal-final-posy 116)

(define PORTAL-IN-IMAGE (bitmap/file "Portal-In.png"))
(define PORTAL-OUT-IMAGE (bitmap/file "Portal-Out.png"))
(define JOGADOR (player (bitmap/file "Player-Image.png") 32 476))
(define CENARIO (bitmap/file "Level-Image.png"))

(define CONTATO-0 (contato-cenario 0 800 555))
(define CONTATO-1 (contato-cenario 658 800 430))
(define CONTATO-2 (contato-cenario 311 480 362))
(define CONTATO-3 (contato-cenario 470 635 258))
(define CONTATO-4 (contato-cenario 220 378 203))
(define CONTATO-5 (contato-cenario 0 165 150))
(define PORTAL (portal 200 200 200 200))
(define status "game")


(define CONTATOS (cons CONTATO-0 (cons CONTATO-1 (cons CONTATO-2 (cons CONTATO-3 (cons CONTATO-4 (cons CONTATO-5 empty)))))))

(define (COLISAO-CENARIO cont)
  (if (empty? cont) #f (or (COLIDE JOGADOR (first cont))(COLISAO-CENARIO (rest cont)))))

(define (COLIDE jog cont)
  (if (and (= (+ (player-posy jog) 27) (contato-cenario-posy cont)) (and (>= (+ (player-posx jog) 18) (contato-cenario-posx-i cont)) (<= (- (player-posx jog) 18) (contato-cenario-posx-f cont)))) #t #f))

(define (gravity)
  (if (COLISAO-CENARIO CONTATOS) (set-player-posy! JOGADOR (player-posy JOGADOR)) (set-player-posy! JOGADOR (+ (player-posy JOGADOR) 1))))

(define (GRAVIDADE-E-PORTAIS a)
  (gravity)
  (PORTAIS))

(define (TECLAS w key)   
  (cond [(and (key=? key "right") (equal? status "game")) (DIREITA)]         
        [(and (key=? key "left") (equal? status "game")) (ESQUERDA)]
        [(key=? key " ") (PORTAL-F)]
        [(and (key=? key "right") (equal? status "portal-in")) (DIREITA-PORTAL-IN)]
        [(and (key=? key "left") (equal? status "portal-in")) (ESQUERDA-PORTAL-IN)]
        [(and (key=? key "up") (equal? status "portal-in")) (CIMA-PORTAL-IN)]
        [(and (key=? key "down") (equal? status "portal-in")) (BAIXO-PORTAL-IN)]
        [(and (key=? key "right") (equal? status "portal-out")) (DIREITA-PORTAL-OUT)]
        [(and (key=? key "left") (equal? status "portal-out")) (ESQUERDA-PORTAL-OUT)]
        [(and (key=? key "up") (equal? status "portal-out")) (CIMA-PORTAL-OUT)]
        [(and (key=? key "down") (equal? status "portal-out")) (BAIXO-PORTAL-OUT)]
        ))
(define (DIREITA)
  (cond [(< (+ (player-posx JOGADOR) 6) 800) (set-player-posx! JOGADOR (+ (player-posx JOGADOR) 6))]))
(define (ESQUERDA)
  (cond [(> (- (player-posx JOGADOR) 6) 0) (set-player-posx! JOGADOR (- (player-posx JOGADOR) 6))]))
(define (PORTAL-F)
  ;TESTAR A VALIDADE DOS PORTAIS AQUI
  (cond [(equal? status "game") (set! status "portal-in")]
        [(equal? status "portal-in") (set! status "portal-out")]
        [(equal? status "portal-out") (set! status "game")]))


;CORRIGIR PORTAIS FORA DA TELA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



(define (DIREITA-PORTAL-IN)
  ;CORRIGIR QUANDO A COORDENADA ULTRAPASSA O MÁXIMO POSSÍVEL PROVAVELMENTE TESTES DESNECESSÁRIOS ABAIXO ---------- IN
  (if (and (> (- (portal-posx-in PORTAL) 38) 0) (< (+ (portal-posx-in PORTAL) 38) 800)) (set-portal-posx-in! PORTAL (+ (portal-posx-in PORTAL) 6)) (set-portal-posx-in! PORTAL (portal-posx-in PORTAL))))
(define (ESQUERDA-PORTAL-IN)
  ;CORRIGIR QUANDO A COORDENADA ULTRAPASSA O MÁXIMO POSSÍVEL
  (if (and (> (- (portal-posx-in PORTAL) 38) 0) (< (+ (portal-posx-in PORTAL) 38) 800)) (set-portal-posx-in! PORTAL (- (portal-posx-in PORTAL) 6)) (set-portal-posx-in! PORTAL (portal-posx-in PORTAL))))
(define (BAIXO-PORTAL-IN)
  ;CORRIGIR QUANDO A COORDENADA ULTRAPASSA O MÁXIMO POSSÍVEL
  (if (and (> (- (portal-posy-in PORTAL) 38) 0) (< (+ (portal-posy-in PORTAL) 38) 600)) (set-portal-posy-in! PORTAL (+ (portal-posy-in PORTAL) 6)) (set-portal-posy-in! PORTAL (portal-posy-in PORTAL))))
(define (CIMA-PORTAL-IN)
  ;CORRIGIR QUANDO A COORDENADA ULTRAPASSA O MÁXIMO POSSÍVEL
  (if (and (> (- (portal-posy-in PORTAL) 38) 0) (< (+ (portal-posy-in PORTAL) 38) 600)) (set-portal-posy-in! PORTAL (- (portal-posy-in PORTAL) 6)) (set-portal-posy-in! PORTAL (portal-posy-in PORTAL))))
(define (DIREITA-PORTAL-OUT)
  ;CORRIGIR QUANDO A COORDENADA ULTRAPASSA O MÁXIMO POSSÍVEL PROVAVELMENTE TESTES DESNECESSÁRIOS ABAIXO --------- OUT
  (if (and (> (- (portal-posx-out PORTAL) 38) 0) (< (+ (portal-posx-out PORTAL) 38) 800)) (set-portal-posx-out! PORTAL (+ (portal-posx-out PORTAL) 6)) (set-portal-posx-out! PORTAL (portal-posx-out PORTAL))))
(define (ESQUERDA-PORTAL-OUT)
  ;CORRIGIR QUANDO A COORDENADA ULTRAPASSA O MÁXIMO POSSÍVEL
  (if (and (> (- (portal-posx-out PORTAL) 38) 0) (< (+ (portal-posx-out PORTAL) 38) 800)) (set-portal-posx-out! PORTAL (- (portal-posx-out PORTAL) 6)) (set-portal-posx-out! PORTAL (portal-posx-out PORTAL))))
(define (BAIXO-PORTAL-OUT)
  ;CORRIGIR QUANDO A COORDENADA ULTRAPASSA O MÁXIMO POSSÍVEL
  (if (and (> (- (portal-posy-out PORTAL) 38) 0) (< (+ (portal-posy-out PORTAL) 38) 600)) (set-portal-posy-out! PORTAL (+ (portal-posy-out PORTAL) 6)) (set-portal-posy-out! PORTAL (portal-posy-out PORTAL))))
(define (CIMA-PORTAL-OUT)
  ;CORRIGIR QUANDO A COORDENADA ULTRAPASSA O MÁXIMO POSSÍVEL
  (if (and (> (- (portal-posy-out PORTAL) 38) 0) (< (+ (portal-posy-out PORTAL) 38) 600)) (set-portal-posy-out! PORTAL (- (portal-posy-out PORTAL) 6)) (set-portal-posy-out! PORTAL (portal-posy-out PORTAL))))

(define (SCENE)
  (place-image PORTAL-IN-IMAGE (portal-posx-in PORTAL) (portal-posy-in PORTAL) (SCENE-2)))

(define (SCENE-2)
  (place-image PORTAL-OUT-IMAGE (portal-posx-out PORTAL) (portal-posy-out PORTAL) CENARIO))

(define (TELA w)
  (place-image (player-imagem JOGADOR) (player-posx JOGADOR) (player-posy JOGADOR) (SCENE)))

#|IMPORTANTE PRA KCT
(define (SCENE)
  (place-image PORTAL-IN-IMAGE (portal-posx-in PORTAL) (portal-posy-in PORTAL) CENARIO))

(define (TELA w)
  (place-image (player-imagem JOGADOR) (player-posx JOGADOR) (player-posy JOGADOR) (SCENE)))
funcoes com place-image retornam uma scene, para colocar multiplas imagens é só usar uma função com place-image no lugar da scene da funcao
|#

(define (PORTAIS)
  (if (and (equal? status "game") (PROXIMO-PORTAL-IN)) (ATRAVESSA-PORTAL) (NADA)))

(define (PROXIMO-PORTAL-IN)
  (if (< (sqrt (+ (expt (- (player-posx JOGADOR) (portal-posx-in PORTAL)) 2) (expt (- (player-posy JOGADOR) (portal-posy-in PORTAL)) 2))) 20) #t #f))

(define (ATRAVESSA-PORTAL)
  (set-player-posx! JOGADOR (portal-posx-out PORTAL))
  (set-player-posy! JOGADOR (portal-posy-out PORTAL)))

(define (FIM? w)
  (if (< (sqrt (+ (expt (- (player-posx JOGADOR) portal-final-posx) 2) (expt (- (player-posy JOGADOR) portal-final-posy) 2))) 10) #t #f))

(define (NADA)
  (+ 0 0))

(define (TELA-FIM w)
  (text (format "Fim do Jogo!") 20 "black"))

(big-bang 0
          (on-tick GRAVIDADE-E-PORTAIS)
          (on-key TECLAS)
          (to-draw TELA)
          (stop-when FIM? TELA-FIM))
