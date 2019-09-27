(import [itertools [islice
                    takewhile
                    dropwhile]]
        [pprint [pprint]]
        [collections [namedtuple]]
        [pathlib [Path]]
        struct
        crcmod
        crcmod.predefined
        re
        binascii
        click
        yaml)


(setv Identification
      (namedtuple "Identification"
                  ["vorbis_version"
                   "audio_channels"
                   "audio_sample_rate"
                   "bitrate_maximum"
                   "bitrate_nominal"
                   "bitrate_minimum"
                   "blocksize_0_1"]))

(setv CommentHeader
      (namedtuple "CommentHeader"
                  ["vender_length"
                   "vendor"
                   "num_comments"]))

(setv Comment
      (namedtuple "Comment"
                  ["quality"
                   "min_distance"
                   "maximum_distance"
                   "base_volume"
                   "sound_type"
                   "max_ai_distance"]))


(setv *default-comment* (Comment 3 2 100 1 0 50))
(setv *ident-header-flag* (struct.pack "B6s" 1 b"vorbis"))
(setv *comment-header-flag* (struct.pack "B6s" 3 b"vorbis"))
(setv *comment-format* "I3fIf")
(setv *sound-types* {134217856 "World ambient"
                     134217984 "Object exploding"
                     134218240 "Object colliding"
                     134218752 "Object breaking"
                     268437504 "Anomaly idle"
                     536875008 "NPC eating"
                     536879104 "NPC attacking"
                     536887296 "NPC talking"
                     536903680 "NPC step"
                     536936448 "NPC injuring"
                     537001984 "NPC dying"
                     1077936128 "Item using"
                     1082130432 "Item taking"
                     1090519040 "Item hiding"
                     1107296256 "Item dropping"
                     1140850688 "Item picking up"
                     2147745792 "weapon recharging"
                     2148007936 "Weapon bullet hit"
                     2148532224 "Weapon empty clicking"
                     2149580800 "Weapon shooting"})

(defn int->sound-type [sound-int]
  (.get *sound-types* sound-int "default"))

(defn render-identification [identification]
  (.join "\n" ["Identification Header"
               "---------------------"
               #* (lfor (, k v) (.items (._asdict identification))
                     (+ k ": " (str v)))]))

(defn render-comment [comment]
  (setv comment-dict (._asdict comment))
  (setv (get comment-dict "sound_type") (int->sound-type (get comment-dict "sound_type")))
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


(defn parse-identity [byte-seq]
  (setv index (.find byte-seq *ident-header-flag*))
  (setv data (cut byte-seq index))
  (setv data (cut data (len *ident-header-flag*)))
  (, byte-seq
     (Identification #* (struct.unpack "<IBI3iB" (cut data 0 22)))))


(defn parse-comment [byte-seq]
  (setv index (.find byte-seq *comment-header-flag*))
  (setv data (cut byte-seq index))
  (setv data (cut data (len *comment-header-flag*)))
  (setv (, header-length ) (struct.unpack "I" (cut data 0 4)))
  (setv header (CommentHeader #*
                              (struct.unpack f"<I{header-length}sI"
                                             (cut data 0 (+ 8 header-length)))))
  (setv data (cut data (+ 8 header-length)))
  (setv (, comment-length) (struct.unpack "I" (cut data 0 4)))

  (unless (= comment-length 24)
    (raise (ValueError "Ogg contains no stalker vorbis comment")))
  (setv data (cut data 4))
  (, byte-seq
     header
     (Comment #* (struct.unpack "I3fIf" (cut data 0 24)))))

(defn ensure-comment [byte-seq]
  (setv index (.find byte-seq *comment-header-flag*))
  (setv data (cut byte-seq index))
  (setv data (cut data (len *ident-header-flag*)))
  (setv (, header-length ) (struct.unpack "I" (cut data 0 4)))
  (setv header (CommentHeader #*
                              (struct.unpack f"<I{header-length}sI"
                                             (cut data 0 (+ 8 header-length)))))
  (setv data (cut data (+ 8 header-length)))
  (setv (, comment-length) (struct.unpack "I" (cut data 0 4)))
  (unless (= comment-length 24)
    (setv default-comment-bytes (struct.pack *comment-format* #* *default-comment*))
    (setv index-of-split (+ index
                            (len *ident-header-flag*)
                            4
                            header-length))
    (setv byte-seq (replace-range index-of-split
                                  (+ index-of-split 4)
                                  (struct.pack "I" (+ header.num_comments 1))
                                  byte-seq))
    (setv byte-seq (insert-list (+ index-of-split 4)
                                byte-seq
                                (+ (struct.pack "I" 24) default-comment-bytes ))))
  byte-seq)

(defn get-pages [byte-seq]
  (re.findall b"(OggS.*?(?=OggS|$))" byte-seq re.DOTALL))

(defn find-in-pages [value pages]
  (try (next (gfor (, i page) (enumerate pages) :if (>= (.find page value) 0) i))
       (except [StopIteration])))

(defn update-checksum [byte-seq]
  (setv pages (get-pages byte-seq)
        comment-page (find-in-pages *comment-header-flag*  pages)
        data (get pages comment-page)
        crc-fun (crcmod.mkCrcFun 0x104c11db7 :initCrc 0 :xorOut 0 :rev False)
        crc-zero (struct.pack "I" 0)
        crc-old (cut data 22 (+ 22 4))
        data (replace-range 22 (+ 22 4) crc-zero data)
        crc-new (struct.pack "I" (crc-fun data))
        data (replace-range 22 (+ 22 4) crc-new data)
        (get pages comment-page) data)
  (print (struct.unpack "I" crc-old ) (struct.unpack "I" crc-new))
  (.join b"" pages))


(defn parse-ogg [file]
  (with [f (open file "rb")]
    (setv data (.read f)))
  (setv data (ensure-comment data))
  (setv (, data ident) (parse-identity data))
  (setv (, data comment-header comment) (parse-comment data))
  (setv data (update-checksum data))
  (, data ident comment))

(with-decorator
  (click.command)
  (click.argument "manifest" :type (click.File "r"))
  (defn cli [manifest]
    (setv manifest (yaml.load manifest :Loader yaml.FullLoader))
    (setv cwd (Path.cwd))
    (for [entry manifest]
      (setv files (cwd.glob (get entry "glob")))
      (for [file files]
        (try
          (setv (, data ident header) (parse-ogg file))
          (print (render-ogg (. file stem) ident header))
          (print (+ "\n" (* "=" 25) "\n"))
          ;; (with [out (open "uncommented-inserted.ogg" "wb")]
          ;;   (out.write data))
          (except [ValueError]))))))

(defmain [&rest args]
  (cli))
