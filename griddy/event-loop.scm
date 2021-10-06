;;; Chickadee Game Toolkit
;;; Copyright © 2018, 2021 David Thompson <davet@gnu.org>
;;; Copyright © 2020 Peter Elliott <pelliott@ualberta.ca>
;;;
;;; Chickadee is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; Chickadee is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Simple SDL + OpenGL game loop implementation.
;;
;;; Code:

(define-module (griddy event-loop)
  #:use-module (chickadee config)
  #:use-module (chickadee game-loop)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee graphics color)
  #:use-module (chickadee graphics engine)
  #:use-module (chickadee graphics gl)
  #:use-module (chickadee graphics viewport)
  #:use-module (chickadee utils)
  #:use-module (gl)
  #:use-module (gl enums)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:use-module (sdl2)
  #:use-module (sdl2 events)
  #:use-module ((sdl2 video) #:prefix sdl2:)
  #:use-module (srfi srfi-9)
  #:export (current-window
            window?
            window-width
            window-height
            window-x
            window-y
            elapsed-time
            run-game)
  #:re-export (abort-game))

(define %time-freq (exact->inexact (sdl-performance-frequency)))

(define (elapsed-time)
  "Return the current value of the system timer in seconds."
  (/ (sdl-performance-counter) %time-freq))

(define-record-type <window>
  (wrap-window sdl-window gl-context)
  window?
  (sdl-window unwrap-window)
  (gl-context window-gl-context))

(define* (make-window #:key (title "Chickadee") fullscreen? resizable?
                      (width 640) (height 480) (multisample? #t))
  ;; Hint that we want OpenGL 3.2 Core profile.  Doesn't mean we'll
  ;; get it, though!
  (sdl2:set-gl-attribute! 'context-major-version 3)
  (sdl2:set-gl-attribute! 'context-major-version 2)
  (sdl2:set-gl-attribute! 'context-profile-mask 1) ; core profile
  (sdl2:set-gl-attribute! 'stencil-size 8) ; 8-bit stencil buffer
  (if multisample?
      (begin
        (sdl2:set-gl-attribute! 'multisample-buffers 1)
        (sdl2:set-gl-attribute! 'multisample-samples 4))
      (begin
        (sdl2:set-gl-attribute! 'multisample-buffers 0)
        (sdl2:set-gl-attribute! 'multisample-samples 0)))
  (let* ((window (sdl2:make-window #:opengl? #t
                                   #:title title
                                   #:size (list width height)
                                   #:fullscreen? fullscreen?
                                   #:resizable? resizable?))
         (gl-context (sdl2:make-gl-context window)))
    (wrap-window window gl-context)))

(define current-window (make-parameter #f))

(define* (run-game #:key
                   (window-title "Chickadee!")
                   (window-width 640)
                   (window-height 480)
                   window-fullscreen?
                   window-resizable?
                   (update-hz 60)
                   (load (const #t))
                   (update (const #t))
                   (draw (const #t))
                   (quit abort-game)
                   error)
  (sdl-init)
  ;; We assume here that if window creation fails it is because
  ;; multisampling is not supported and that we need to try again with
  ;; multisampling disabled.  This is obviously hacky but it solves a
  ;; real world user issue and I'm not sure how to test for driver
  ;; features before opening the window.  SDL's display mode
  ;; information doesn't seem to be enough.  Help wanted!
  (let* ((window (or (false-if-exception
                      (make-window #:title window-title
                                   #:width window-width
                                   #:height window-height
                                   #:fullscreen? window-fullscreen?
                                   #:resizable? window-resizable?))
                     (make-window #:title window-title
                                  #:width window-width
                                  #:height window-height
                                  #:fullscreen? window-fullscreen?
                                  #:resizable? window-resizable?
                                  #:multisample? #f)))
         (gfx (make-graphics-engine (window-gl-context window)))
         (default-viewport (make-atomic-box
                            (make-viewport 0 0 window-width window-height)))
         (default-projection (make-atomic-box (orthographic-projection 0 window-width
                                                       window-height 0
                                                       0 1))))
    (define (invert-y y)
      ;; SDL's origin is the top-left, but our origin is the bottom
      ;; left so we need to invert Y coordinates that SDL gives us.
      (- window-height y))
    (define (input-sdl)
      (define (process-event event)
        (if (quit-event? event)
            (quit)))
      ;; Process all pending events.
      (let loop ((event (poll-event)))
        (when event
          (process-event event)
          (loop (poll-event)))))
    (define (update-sdl dt)
      (input-sdl)
      (update dt)
      ;; Free any GPU resources that have been GC'd.
      (graphics-engine-reap! gfx))
    (define (render-sdl-opengl alpha)
      (with-graphics-state! ((g:viewport (atomic-box-ref default-viewport)))
                            (clear-viewport)
        (with-projection (atomic-box-ref default-projection)
          (draw alpha)))
      (sdl2:swap-gl-window (unwrap-window window)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (parameterize ((current-window window)
                       (current-graphics-engine gfx))
          ;; Attempt to activate vsync, if possible.  Some systems do
          ;; not support setting the OpenGL swap interval.
          (catch #t
            (lambda ()
              (sdl2:set-gl-swap-interval! 'vsync))
            (lambda args
              (display "warning: could not enable vsync\n"
                       (current-error-port))))
          ;; Turn off multisampling by default.
          (gl-disable (version-1-3 multisample))
          (load)
          (run-game* #:update update-sdl
                     #:render render-sdl-opengl
                     #:error error
                     #:time elapsed-time
                     #:update-hz update-hz)))
      (lambda ()
        (sdl2:delete-gl-context! (window-gl-context window))
        (sdl2:close-window! (unwrap-window window))))))
