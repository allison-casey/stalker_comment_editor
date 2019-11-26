(import [hy.contrib.walk [walk]]
        [.consts [*sound-types*
                  *sound-types-lookup*]]
        re)


(defn int->sound-type
  [sound-int]
  (.get *sound-types* sound-int "default"))

(defn sound-type->int
  [sound-type]
  (unless (in sound-type *sound-types-lookup*)
    (raise (ValueError "sound type not in valid sound-types")))
  (get *sound-types-lookup* sound-type))

(defn render-identification
  [identification]
  (.join "\n" ["Identification Header"
               "---------------------"
               #* (lfor (, k v) (.items identification)
                     (+ (name k) ": " (str v)))]))

(defn render-comment
  [comment]
  (setv (get comment :sound-type)
        (int->sound-type (:sound-type (:body comment))))
  (.join "\n" ["Comment Block"
               "---------------------"
               #* (lfor (, k v) (.items (:body comment))
                        (+ (name k) ": " (str v)))]))

(defn render-ogg
  [filename ident comment]
  (.join "\n" [(+ "File: " filename "\n")
               (render-identification ident)
               ""
               (render-comment comment)]))

(defn update-namedtuple
  [col tpl]
  (setv tpl-old (._asdict tpl)
        tpl-new {#** tpl-old #** col })
  (.__class__ tpl #** tpl-new))

(defn split-seq
  [idx seq]
  (, (cut seq 0 idx)
     (cut seq idx)))

(defn insert-list
  [index seq1 seq2]
  (+ (cut seq1 0 index) seq2 (cut seq1 index)))

(defn replace-range
  [start end value seq]
  (+ (cut seq 0 start)
     value
     (cut seq end)))

(defn get-pages
  [byte-seq]
  (re.findall b"(OggS.*?(?=OggS|$))" byte-seq re.DOTALL))

(defn get-sections
  [byte-seq]
  (re.findall b"(vorbis.*?(?=vorbis|$))" byte-seq re.DOTALL))

(defn find-in-pages
  [value pages]
  (try (next (gfor (, i page) (enumerate pages) :if (>= (.find page value) 0) i))
       (except [StopIteration])))

(defn merge
  [coll1 coll2]
  {#** coll1 #** coll2})

(defn spy
  [x]
  (print "LOG:" x)
  x)

(defn hydict
  [coll]
  )

(import [hy.contrib.walk [walk]])

(defn -convert-keys
  [tpl]
  (setv (, k v) tpl)
  (if (isinstance v dict)
      [(keyword k) (dfor (, k1 v1)
                         (.items v)
                         :setv (, k2 v2) (-convert-keys (, k1 v1))
                         [k2 v2]) ]
      [(keyword k) v]))

(defn convert-keys
  [coll]
  (->> coll
       .items
       (walk -convert-keys identity)
       dict))

(defn py->hy
  [x]
  (cond
    [(none? x) None]
    [(list? x) (lfor item x (py->hy item))]
    [(isinstance x dict) (convert-keys x)]
    [:else x]))
