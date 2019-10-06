(import [.consts [*sound-types*
                  *sound-types-lookup*]]
        re)

(defn int->sound-type [sound-int]
  (.get *sound-types* sound-int "default"))

(defn sound-type->int [sound-type]
  (unless (in sound-type *sound-types-lookup*)
    (raise (ValueError "sound type not in valid sound-types")))
  (get *sound-types-lookup* sound-type))

(defn render-identification [identification]
  (.join "\n" ["Identification Header"
               "---------------------"
               #* (lfor (, k v) (.items (._asdict identification))
                     (+ k ": " (str v)))]))

(defn render-comment [comment]
  (setv comment-dict (._asdict comment)
        (get comment-dict "sound_type")
        (int->sound-type (get comment-dict "sound_type")))
  (.join "\n" ["Comment Block"
               "---------------------"
               #* (lfor (, k v) (.items comment-dict)
                        (+ k ": " (str v)))]))

(defn render-ogg [filename ident comment]
  (.join "\n" [(+ "File: " filename "\n")
               (render-identification ident)
               ""
               (render-comment comment)]))

(defn update-namedtuple [col tpl]
  (setv tpl-old (._asdict tpl)
        tpl-new {#** tpl-old #** col })
  (.__class__ tpl #** tpl-new))

(defn split-seq [idx seq]
  (, (cut seq 0 idx)
     (cut seq idx)))

(defn insert-list [index seq1 seq2]
  (+ (cut seq1 0 index) seq2 (cut seq1 index)))

(defn replace-range [start end value seq]
  (+ (cut seq 0 start)
     value
     (cut seq end)))

(defn get-pages [byte-seq]
  (re.findall b"(OggS.*?(?=OggS|$))" byte-seq re.DOTALL))

(defn get-sections [byte-seq]
  (re.findall b"(vorbis.*?(?=vorbis|$))" byte-seq re.DOTALL))

(defn find-in-pages [value pages]
  (try (next (gfor (, i page) (enumerate pages) :if (>= (.find page value) 0) i))
       (except [StopIteration])))
