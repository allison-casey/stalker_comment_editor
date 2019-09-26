(import [itertools [islice
                    takewhile
                    dropwhile]]
        [pprint [pprint]]
        [collections [namedtuple]]
        [pathlib [Path]]
        struct
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
  (Identification #* (struct.unpack "<IBI3iB" (cut data 0 22))))


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
  (Comment #* (struct.unpack "I3fIf" (cut data 0 24))))

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

(defn parse-ogg [file]
  (with [f (open file "rb")]
    (setv data (.read f)))
  (setv data (ensure-comment data))
  (setv ident (parse-identity data))
  (setv comment (parse-comment data))
  (, ident comment))

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
          (setv (, ident header) (parse-ogg file))
          (print "File: " (. file stem))
          (print "Identification Header:" ident)
          (print)
          (print "Comment Block:" header)
          (print (* "-" 50))
          (except [ValueError]))))))

(defmain [&rest args]
  (cli))


(setv some-list [1 2 3 4 5 6 7])
(print (insert-list 3 some-list ["a" "b"]))

;; (split-seq 4 (lfor digit (cut (bin 184) 2) (int digit)))

;; (setv (, blocksize_0 blocksize_1) (map (fn [x] (int x 2) ) (split-seq 4 (cut (bin 184) 2))))
;; (print (** 2 blocksize_0)  (** 2 blocksize_1))


;; ;; check for valid headers
;; (setv (, header data) (read-string data 4))
;; (unless (= header b"OggS") (raise (ValueError "Bad Headers")))

;; (print (cut data 0 200))
;; ;; move to vorbis tag
;; (setv data (bytes (dropwhile (fn [x] (!= x (ord b"v" ))) data)))
;; (setv data (cut data 6))

;; (setv id-header (Identification #* (struct.unpack "<IBI3iBB" (cut data 0 23))))

;; (setv data (bytes (dropwhile (fn [x] (!= x (ord b"v"))) data)))
;; (setv data (cut data 6))

;; (setv (, vender-length vender num-comments) (struct.unpack "<I29sI" (cut data 0 37)))
;; (setv data (cut data 37))
;; (setv comments [])
;; (for [i (range num-comments)]
;;   (setv (, length ) (struct.unpack "I" (cut data 0 4)))
;;   (setv data (cut data 4))

;;   (setv (, comment ) (struct.unpack (.format "{}s" length) (cut data 0 length)))
;;   (setv data (cut data length))

;;   (print comment)
;;   (.append comments comment))
;; (Comment #* (struct.unpack "I3fIf" (get comments 0)))
;; (struct.unpack "<I24s4" (cut data 37 (+ 37 4 24 16)))

;; (setv (, vorbis-tag data) (read-sting data 6))
;; (unless (= vorbis-tag b"vorbis") (raise (ValueError "No vorbis tag")))



;; (struct.unpack "<ii" (cut
;;                        b"\x03\x00\x00\x00\x00\x00\x00@\x00\x00\xc8B\x00\x00\x80?\x00\x00\x00\x00\x00\x00HB" 0 8))

;; (struct.pack "f" 1)
;; (struct.pack "f" 2)
;; (struct.pack "f" 100)
;; (struct.pack "f" 50)


;; (islice data 4))
;; ;; Look for ogg header
;; (= (list (islice bytes 4) b"OggS"))
;; ;; find vorbis marker
;; (takewhile (fn [x] (!= x b"v")) bytes)
;; ;; (= (list (islice bytes 5) b"vorbis")))
;; (setv some-iter (iter [1 2 3 b"v" 5 6]))

;; (list (takewhile (fn [x] (!= x b"v"))
;;                some-iter))

;; (setv stalker-header b"\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00@\x00\x00\xc8B\x00\x00\x80?\x00\x00\x00\x00\x00\x00HB\x0b\x00")

;; (struct.unpack "i" (cut stalker-header 0 4))
;; (struct.unpack "f" b"\x00\x00\x00@")
