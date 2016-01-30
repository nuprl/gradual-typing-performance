#lang typed/racket

(provide Mode Image-Color Y-Place X-Place Angle Side-Count
         Pen-Style Pen-Cap Pen-Join)

(define-type Mode
  (U 'solid "solid" 'outline "outline" Byte))

(define-type Image-Color
  (U String Symbol color))

(define-type Y-Place
  (U "top" 'top "bottom" 'bottom "middle" 'middle "center" 'center
     "baseline" 'baseline "pinhole" 'pinhole))

(define-type X-Place
  (U "left" 'left "right" 'right "middle" 'middle "center" 'center
     "baseline" 'baseline "pinhole" 'pinhole))

(define-type Angle Nonnegative-Real)

(define-type Side-Count Positive-Integer)

(define-type Pen-Style
  (U "solid" 'solid "dot" 'dot "long-dash" 'long-dash
     "short-dash" 'short-dash "dot-dash" 'dot-dash))

(define-type Pen-Cap
  (U "round" 'round "projecting" 'projecting "butt" 'butt))

(define-type Pen-Join
  (U "round" 'round "bevel" 'bevel "miter" 'miter))

(require/typed/provide
 lang/posn
 #;
 [#:struct posn ([x : Real] [y : Real])
           #:extra-constructor-name make-posn])

(require/typed/provide
 2htdp/image
 [#:opaque Image image?]
 [#:struct color ([red : Byte] [green : Byte] [blue : Byte] [alpha : Byte])
           #:extra-constructor-name make-color]
 [#:opaque pen pen?]
 ;; Can't use this type unless mrlib/image-core provides the struct:pen
 ;; binding. For now, just leave it opaque
 #;
 [#:struct pen ([color : Image-Color] [width : Nonnegative-Real]
                [style : Pen-Style] [cap : Pen-Cap] [join : Pen-Join])
           #:extra-constructor-name make-pen]
 ;; 2.3.1
 [circle (Nonnegative-Real Mode (U pen Image-Color) -> Image)]
 [ellipse (Nonnegative-Real Natural Mode (U pen Image-Color) -> Image)]
 [line (Real Real (U pen Image-Color) -> Image)]
 [add-line (Image Real Real Real Real (U pen Image-Color) -> Image)]
 [add-curve (Image Real Real Angle Real Real Angle Real
             (U pen Image-Color)-> Image)]
 [text (String Positive-Integer Image-Color -> Image)]
 [text/font (String Positive-Integer Image-Color (Option String)
             (U 'default 'decorative 'roman 'script 'swiss 'modern
                'symbol 'system)
             (U 'normal 'italic 'slant)
             (U 'normal 'bold 'light)
             Any -> Image)]
 [empty-image Image]
 ;; 2.3.2
 [triangle (Nonnegative-Real Mode (U pen Image-Color) -> Image)]
 [right-triangle (Nonnegative-Real Natural Mode (U pen Image-Color) -> Image)]
 [isosceles-triangle (Nonnegative-Real Angle Mode (U pen Image-Color) -> Image)]
 [triangle/sss (Nonnegative-Real Natural Natural Mode (U pen Image-Color) -> Image)]
 [triangle/ass (Angle Nonnegative-Real Natural Mode (U pen Image-Color) -> Image)]
 [triangle/sas (Nonnegative-Real Angle Natural Mode (U pen Image-Color) -> Image)]
 [triangle/ssa (Nonnegative-Real Natural Angle Mode (U pen Image-Color) -> Image)]
 [triangle/aas (Angle Nonnegative-Real Natural Mode (U pen Image-Color) -> Image)]
 [triangle/asa (Angle Nonnegative-Real Angle Mode (U pen Image-Color) -> Image)]
 [triangle/saa (Nonnegative-Real Angle Angle Mode (U pen Image-Color) -> Image)]
 [square (Nonnegative-Real Mode (U pen Image-Color) -> Image)]
 [rectangle (Nonnegative-Real Nonnegative-Real Mode (U pen Image-Color) -> Image)]
 [rhombus (Nonnegative-Real Angle Mode (U pen Image-Color) -> Image)]
 [star (Nonnegative-Real Mode (U pen Image-Color) -> Image)]
 [star-polygon (Nonnegative-Real Side-Count Side-Count Mode (U pen Image-Color) -> Image)]
 [radial-star
  (Positive-Integer Nonnegative-Real Nonnegative-Real Mode (U pen Image-Color) -> Image)]
 [regular-polygon (Nonnegative-Real Side-Count Mode (U pen Image-Color) -> Image)]
 #;
 [polygon ((Listof posn) Mode (U pen Image-Color) -> Image)]
 ;; 2.3.3
 [overlay (Image Image Image * -> Image)]
 [overlay/align (X-Place Y-Place Image Image Image * -> Image)]
 [overlay/offset (Image Real Real Image -> Image)]
 [overlay/align/offset (X-Place Y-Place Image Real Real Image -> Image)]
 [overlay/xy (Image Real Real Image -> Image)]
 [underlay (Image Image Image * -> Image)]
 [underlay/align (X-Place Y-Place Image Image Image * -> Image)]
 [underlay/offset (Image Real Real Image -> Image)]
 [underlay/align/offset (X-Place Y-Place Image Real Real Image -> Image)]
 [underlay/xy (Image Real Real Image -> Image)]
 [beside (Image Image Image * -> Image)]
 [beside/align (Y-Place Image Image Image * -> Image)]
 [above (Image Image Image * -> Image)]
 [above/align (X-Place Image Image Image * -> Image)]
 ;; 2.3.4
 [empty-scene (case-> (Nonnegative-Real Nonnegative-Real -> Image)
                      (Nonnegative-Real Nonnegative-Real Image-Color -> Image))]
 [place-image (Image Real Real Image -> Image)]
 [place-image/align (Image Real Real X-Place Y-Place Image -> Image)]
 [scene+line (Image Real Real Real Real (U pen Image-Color) -> Image)]

 [scene+curve (Image Real Real Angle Real Real Real Angle Real
                     (U pen Image-Color) -> Image)]
 ;; 2.3.5
 [rotate (Angle Image -> Image)]
 [scale (Positive-Real Image -> Image)]
 [scale/xy (Positive-Real Positive-Real Image -> Image)]
 [flip-horizontal (Image -> Image)]
 [flip-vertical (Image -> Image)]
 [crop (Real Real Nonnegative-Real Nonnegative-Real Image -> Image)]
 [frame (Image -> Image)]
 ;; 2.3.6
 [bitmap/url (String -> Image)]
 [bitmap/file (Path-String -> Image)]
 [image->color-list (Image -> (Listof color))]
 [color-list->bitmap
  ((Listof Image-Color) Nonnegative-Real Nonnegative-Real -> Image)]
 [freeze
  (case-> (Image -> Image)
          (Nonnegative-Real Nonnegative-Real Image -> Image)
          (Real Real Nonnegative-Real Nonnegative-Real Image -> Image))]
 ;; 2.3.7
 [image-width (Image -> Natural)]
 [image-height (Image -> Natural)]
 [image-baseline (Image -> Natural)]
 ;; 2.3.10
 [center-pinhole (Image -> Image)]
 [put-pinhole (Integer Integer Image -> Image)]
 [pinhole-x (Image -> (Option Integer))]
 [pinhole-y (Image -> (Option Integer))]
 [clear-pinhole (Image -> Image)]
 [overlay/pinhole (Image Image Image * -> Image)]
 [underlay/pinhole (Image Image Image * -> Image)]
 ;; 2.3.11
 [save-image
  (case-> (Image Path-String -> Boolean)
          (Image Path-String Nonnegative-Real -> Boolean)
          (Image Path-String Nonnegative-Real Nonnegative-Real -> Boolean))]
 [save-svg-image
  (case-> (Image Path-String -> Boolean)
          (Image Path-String Nonnegative-Real -> Boolean)
          (Image Path-String Nonnegative-Real Nonnegative-Real -> Boolean))])

