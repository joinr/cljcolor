;;An auxillary namespace for color implementations.
;;Designed to support charting and general
;;color palettes, including brewers and
;;other famous stuff...
;;designed to provide/leverage portable
;;color specifications in an approachable
;;way.
(ns cljcolor.core
  (:require [thi.ng.color.core :as color]
            [thi.ng.color.gradients :as gradients]
            [thi.ng.color.presets :as presets]
            [thi.ng.color.presets.brewer :as colorbrewer]
            [thi.ng.color.presets.categories :as categories]))

;;Imported from spork.graphics2d
;;Eclipse Public License
;;https://github.com/joinr/spork/src/spork/graphics2d.clj

(defprotocol IColor
  (get-rgb   [c] "Returns an int encoding of the RGB values.")
  (get-r     [c] "Returns an int encoding Red, between 0-255.")
  (get-g     [c] "Returns an int encoding Green, between 0-255.")
  (get-b     [c] "Returns an int encoding Blue, between 0-255.")
  (get-a     [c] "Returns an int encoding of alpha, between 0-255."))

(defprotocol IColorVector
  (color-vec [c]))

;;color gradients elided
(defrecord color-rgba #?(:clj  [^int r ^int g ^int b ^int a]
                         :cljs [^number r ^number g ^number b ^number a])
  IColor
  (get-rgb [c] (+ (bit-shift-left r 16)
                  (bit-shift-left g 8)
                  b))
  (get-r [c] r)
  (get-g [c] g)
  (get-b [c] b)
  (get-a [c] a))

(defn color-int [x] (int (* x 255)))
;;vectors are simple color containers.
;;Longs are also valid encodings.
(extend-protocol IColor
  clojure.lang.PersistentVector
  (get-rgb [c] (+ (bit-shift-left (nth c 0) 16)
                  (bit-shift-left (nth c 1) 8)
                  (nth c 2)))
  (get-r [c] (nth c 0))
  (get-g [c] (nth c 1))
  (get-b [c] (nth c 2))
  (get-a [c] (nth c 3 255))
  java.lang.Long
  (get-rgb [c] c)
  (get-r [c] (color-int (color/red (color/int->rgba c))))
  (get-g [c] (color-int (color/green (color/int->rgba c))))
  (get-b [c] (color-int (color/blue (color/int->rgba c))))
  (get-a [c] (color-int (color/alpha (color/int->rgba c)))))

(extend-protocol IColorVector
    clojure.lang.PersistentVector
    (color-vec [v] v)
    java.lang.Long
    (color-vec [c]
      (let [iv (color/int->rgba c)]
        (mapv color-int iv))))

(defn random-color []
  [(rand-int 256)
   (rand-int 256)
   (rand-int 256)
   255])

(defn- get-byte [n idx] (bit-and (bit-shift-right n (* idx 8)) 255))
(defn  get-rgba     [c] (+ (get-rgb c) (bit-shift-left (get-a c) 24)))
(defn  color->alpha [c] (float (/ (get-a c) 255)))
(defn  color->rgb   [c]   (juxt [get-r get-g get-b]))
(defn  color->rgba  [c]  (juxt [get-r get-g get-b get-a]))
(defn  color->rgb-alpha [c]  (juxt [get-r get-g get-b color->alpha]))
(defn  rgba->color-rgba [rgba]
  (let [byte-at (partial get-byte rgba)] 
    (->color-rgba (byte-at 2)
            (byte-at 1)
            (byte-at 0)
            (byte-at 3))))

(defn rgb-equal
  "Determines if two IColors are in fact equal across red, green, and blue."
  [c1 c2] (= (get-rgb c1) (get-rgb c2)))

(defn rgba-equal
  "Determines if two IColors are in fact equal across all four channels."
  [c1 c2]
  (and (= (get-a c1) (get-a c2))
       (rgb-equal c1 c2)))

;;a set of colors from
;;http://www.rapidtables.com/web/color/RGB_Color.htm
(def color-specs
  (into {}
        (concat 
         [[:clear [255 255 255 0]]
          [:maroon [128 0 0]]
          [:dark-red [139 0 0]]
          [:brown        [165 42 42]]
          [:firebrick    [178 34 34]]
          [:crimson      [220 20 60]]
          [:red          [255 0 0]]
          [:tomato       [255 99 71]]
          [:coral        [255 127 80]]
          [:light-red    [255 127 80]] ;added
          [:indian-red   [205 92 92]]
          [:light-coral  [240 128 128]]
          [:dark-salmon  [233 150 122]]
          [:salmon       [250 128 114]]
          [:light-salmon [255 160 122]]
          [:orange-red [255 69 0]]
          [:dark-orange [255 140 0]]
          [:orange [255 165 0]]
          [:gold [255 215 0]]
          [:dark-golden-rod [184 134 11]]
          [:golden-rod [218 165 32]]
          [:pale-golden-rod [238 232 170]]
          [:dark-khaki [189 183 107]]
          [:khaki [240 230 140]]
          [:olive [128 128 0]]
          [:yellow [255 255 0]]
          [:yellow-green [154 205 50]]
          [:dark-olive-green [85 107 47]]
          [:olive-drab [107 142 35]]
          [:lawn-green [124 252 0]]
          [:chart-reuse [127 255 0]]
          [:green-yellow [173 255 47]]
          [:dark-green [0 100 0]]
          [:green [0 128 0]]
          [:forest-green [34 139 34]]
          [:lime [0 255 0]]
          [:lime-green [50 205 50]]
          [:light-green [144 238 144]]
          [:pale-green [152 251 152]]
          [:dark-sea-green [143 188 143]]
          [:medium-spring-green [0 250 154]]
          [:spring-green [0 255 127]]
          [:sea-green [46 139 87]]
          [:medium-aqua-marine [102 205 170]]
          [:medium-sea-green [60 179 113]]
          [:light-sea-green [32 178 170]]
          [:dark-slate-gray [47 79 79]]
          [:teal [0 128 128]]
          [:dark-cyan [0 139 139]]
          [:aqua [0 255 255]]
          [:cyan [0 255 255]]
          [:light-cyan [224 255 255]]
          [:dark-turquoise [0 206 209]]
          [:turquoise [64 224 208]]
          [:medium-turquoise [72 209 204]]
          [:pale-turquoise [175 238 238]]
          [:aqua-marine [127 255 212]]
          [:powder-blue [176 224 230]]
          [:cadet-blue [95 158 160]]
          [:steel-blue [70 130 180]]
          [:corn-flower-blue [100 149 237]]
          [:deep-sky-blue [0 191 255]]
          [:dodger-blue [30 144 255]]
          [:light-blue [173 216 230]]
          [:sky-blue [135 206 235]]
          [:light-sky-blue [135 206 250]]
          [:midnight-blue [25 25 112]]
          [:navy [0 0 128]]
          [:dark-blue [0 0 139]]
          [:medium-blue [0 0 205]]
          [:blue [0 0 255]]
          [:royal-blue [65 105 225]]
          [:blue-violet [138 43 226]]
          [:indigo [75 0 130]]
          [:dark-slate-blue [72 61 139]]
          [:slate-blue [106 90 205]]
          [:medium-slate-blue [123 104 238]]
          [:medium-purple [147 112 219]]
          [:dark-magenta [139 0 139]]
          [:dark-violet [148 0 211]]
          [:dark-orchid [153 50 204]]
          [:medium-orchid [186 85 211]]
          [:purple [128 0 128]]
          [:thistle [216 191 216]]
          [:plum [221 160 221]]
          [:violet [238 130 238]]
          [:magenta [255 0 255]]
          [:fuchsia [255 0 255]]
          [:orchid [218 112 214]]
          [:medium-violet-red [199 21 133]]
          [:pale-violet-red [219 112 147]]
          [:deep-pink [255 20 147]]
          [:hot-pink [255 105 180]]
          [:light-pink [255 182 193]]
          [:pink [255 192 203]]
          [:antique-white [250 235 215]]
          [:beige [245 245 220]]
          [:bisque [255 228 196]]
          [:blanched-almond [255 235 205]]
          [:wheat [245 222 179]]
          [:corn-silk [255 248 220]]
          [:lemon-chiffon [255 250 205]]
          [:light-golden-rod-yellow [250 250 210]]
          [:light-yellow [255 255 224]]
          [:saddle-brown [139 69 19]]
          [:sienna [160 82 45]]
          [:chocolate [210 105 30]]
          [:peru [205 133 63]]
          [:sandy-brown [244 164 96]]
          [:burly-wood [222 184 135]]
          [:tan [210 180 140]]
          [:rosy-brown [188 143 143]]
          [:moccasin [255 228 181]]
          [:navajo-white [255 222 173]]
          [:peach-puff [255 218 185]]
          [:misty-rose [255 228 225]]
          [:lavender-blush [255 240 245]]
          [:linen [250 240 230]]
          [:old-lace [253 245 230]]
          [:papaya-whip [255 239 213]]
          [:sea-shell [255 245 238]]
          [:mint-cream [245 255 250]]
          [:slate-gray [112 128 144]]
          [:light-slate-gray [119 136 153]]
          [:light-steel-blue [176 196 222]]
          [:lavender [230 230 250]]
          [:floral-white [255 250 240]]
          [:alice-blue [240 248 255]]
          [:ghost-white [248 248 255]]
          [:honeydew [240 255 240]]
          [:ivory [255 255 240]]
          [:azure [240 255 255]]
          [:snow [255 250 250]]
          [:black [0 0 0]]
          [:dim-gray [105 105 105]] 
          [:dim-grey [105 105 105]]
          [:gray [128 128 128]]
          [:grey [128 128 128]]
          [:dark-gray [169 169 169]]
          [:dark-grey [169 169 169]]
          [:silver [192 192 192]]
          [:light-gray [211 211 211]]
          [:light-grey [211 211 211]]
          [:gainsboro [220 220 220]]
          [:white-smoke [245 245 245]]
          [:white [255 255 255]]
          ;;added due to convenience
          [:amber [178 140 0]]]
         (for [[k n] presets/colors] ;;merge thi.ng color presets
           [k (assoc (color-vec n) 3 255)]))))

(defn rgb-floor [xs]  [(int  (Math/floor (*  (nth xs 0) 255.0)))
                       (int  (Math/floor (*  (nth xs 1) 255.0)))
                       (int  (Math/floor (*  (nth xs 2) 255.0)))])

(defn hsv->rgb [h s v]
  (let [h_i (long (Math/floor (* h 6)))
        f   (- (* h 6) h_i)
        p   (* v (- 1.0 s))
        q   (* v (- 1.0 (* f s)))
        t   (* v (- 1.0 (*  (- 1.0 f) s)))]
    (rgb-floor  (case h_i
                  0 [v t p]
                  1 [q v p]
                  2 [p v t]
                  3 [p q v]
                  4 [t p v]
                  5 [v p q]))))

(defn rgb->hsv [r g b]
  (let [r (/ r 255.0)
        g (/ g 255.0)
        b (/ b 255.0)
        cmax (max r g b)
        cmin (min r g b)
        delta  (- cmax cmin)
        d  (if (zero? delta) 1.0 delta)
        v cmax
        ;_ (println [r g b cmax cmin delta v])
        s (if (zero? delta) 0 (/ delta cmax))]
    [(/  (cond (= r cmax)  (mod (/ (- g b) d) 6)
                  (= g cmax)  (+ (/ (- b r) d) 2)
                  :else       (+ (/ (- r g) d) 4))
         6)
     s
     v]))
    

;;Thanks to Martin Ankerl at
;; http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/: 

;;these are primitive compared to what thi.ng provides...
(def ^:constant +golden-ratio+ 0.618033988749895)

(let [hue         (double-array [(rand)])]
  (defn random-golden-hue []
    (let [h (double (mod (+ (aget hue 0) +golden-ratio+) 1))]
      (do (aset hue 0 h)
          h))))

(defn saturations
  ([step r g b]
   (let [[h s v] (rgb->hsv r g b)]
     (map (fn [sat]
            (hsv->rgb h sat v)) (take-while #(<= % 1.0) (iterate (fn [x] (+ x step)) 0.0)))))
  ([step color] (saturations step (get-r color) (get-g color) (get-b color))))

(defn mono-color-palette
  ([step color] (saturations step   (get-r color) (get-g color) (get-b color)))
  ([step]       (mono-color-palette step (random-color))))

(defn random-color-palette 
  ([s v] (map (fn [h] (hsv->rgb h s v)) (repeatedly random-golden-hue)))
  ([] (random-color-palette 0.5 0.95)))

;;default color palette
(def ^:dynamic *palette* color-specs)

;;Leveraging thi.ng.color for its EXCELLENT presets and
;;host extra functionality (probably migrate to it entirely
;;entirely!)

(def brewer-schemes
  "Defines the valid range-sizes covered
   by each brewer scheme. For use with
   selecting a brewer scheme."
  {:accent [3 8],
   :blue-green [3 9],
   :blue-purple [3 9],
   :blues [3 9],
   :brown-blue-green [3 11],
   :dark2 [3 8],
   :green-blue [3 9],
   :greens [3 9],
   :greys [3 9],
   :orange-red [3 9],
   :oranges [3 9],
   :paired [3 12],
   :pastel1 [3 9],
   :pastel2 [3 8],
   :pink-yellow-green [3 11],
   :purple-blue [3 9],
   :purple-blue-green [3 9],
   :purple-green [3 11],
   :purple-orange [3 11],
   :purple-red [3 9],
   :purples [3 9],
   :red-blue [3 11],
   :red-grey [3 11],
   :red-purple [3 9],
   :red-yellow-blue [3 11],
   :red-yellow-green [3 11],
   :reds [3 9],
   :set1 [3 9],
   :set2 [3 8],
   :set3 [3 12],
   :spectral [3 11],
   :yellow-green [3 9],
   :yellow-green-blue [3 9],
   :yellow-orange-brown [3 9],
   :yellow-orange-red [3 9]})

(def brewer-pals
  (into {}
        (for [[k [l r]] brewer-schemes
              n         (range l (inc r))]
          [(keyword (str (name k) "-" n))
           (fn [] (mapv #(assoc (color-vec %) 3 255) (colorbrewer/brewer-scheme-int k n)))])))
          

(def d3-pals
  (let [opaque (fn [g] (assoc (color-vec g) 3 255))
        c10  (mapv opaque categories/cat10)
        c20  (mapv opaque categories/cat20)
        c20b (mapv opaque categories/cat20b)
        c20c (mapv opaque categories/cat20c)]
    {:cat10 c10
     :cat20 c20
     :cat20b c20b
     :cat20c c20c
     :cat-10 c10
     :cat-20 c20
     :cat-20b c20b
     :cat-20c c20c}))

;;color ranges...
(defn get-color [k]
  (or (get *palette* k)
      (throw (ex-info (str [:unknown-color! k]) {:color k}))))

(defn get-palette
  ([k]
   (if-let [f (or (get brewer-pals k)
                  (get brewer-pals (keyword k)))]
     (f)
     (or (get d3-pals k)
         (get d3-pals (keyword k))
         (throw (ex-info (str [:unkown-pallette! k]) {:color k})))))
  ([k n]
   (get-palette (keyword (str (name k) "-" n)))))
   
