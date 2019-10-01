(import [.records [Identification CommentHeader Comment]]
        [.utils [
                 ;; int->sound-type
                 ;; sound-type->int
                 ;; render-identification
                 ;; render-comment
                 ;; render-ogg
                 update-namedtuple
                 ;; split-seq
                 insert-list
                 replace-range
                 get-pages
                 find-in-pages]]
        [.consts [*default-comment*
                  *ident-header-flag*
                  *comment-header-flag*
                  *comment-format*]]
        struct
        crcmod)

(defn parse-identity [byte-seq]
  (setv index (.find byte-seq *ident-header-flag*)
        data (cut byte-seq index)
        data (cut data (len *ident-header-flag*))
        ident (Identification #* (struct.unpack "<IBI3iB" (cut data 0 22))))
  (, byte-seq ident))


(defn parse-comment [new-comments byte-seq]
  (if (in "sound_type" new-comments)
      (setv (get new-comments "sound_type")
            (sound-type->int (get new-comments "sound_type"))))

  (setv index (.find byte-seq *comment-header-flag*)
        data (cut byte-seq index)
        data (cut data (len *comment-header-flag*))
        (, header-length ) (struct.unpack "I" (cut data 0 4))
        header (CommentHeader #*
                               (struct.unpack f"<I{header-length}sI"
                                              (cut data 0 (+ 8 header-length))))
        data (cut data (+ 8 header-length)))

  (as-> (cut data 0 4) com-length
        (struct.unpack "I" com-length)
        (get com-length 0)
        (unless (= com-length 24)
          (raise (ValueError "Ogg contains no stalker vorbis comment"))))

  (setv new-comment-bytes (as-> new-comments com
                                (update-namedtuple com *default-comment*)
                                (struct.pack *comment-format* #* com))
        data (as-> data d
                   (cut d 4)
                   (replace-range 0 24 new-comment-bytes d))
        comment (Comment #* (struct.unpack "I3fIf" (cut data 0 24)))

        data (+ (cut byte-seq 0 (+ index
                                   (len *comment-header-flag*)
                                   header-length
                                   4
                                   4
                                   4))
                      data))
  (, data header comment))


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

    (setv start-length (len byte-seq))
    (setv byte-seq (insert-list (+ index-of-split 4)
                                byte-seq
                                (+ (struct.pack "I" 24) default-comment-bytes )))
    )
  byte-seq)



(defn update-checksum [byte-seq]
  (setv pages (get-pages byte-seq)
        comment-page (find-in-pages *comment-header-flag*  pages)
        data (get pages comment-page)
        crc-fun (crcmod.mkCrcFun 0x104c11db7 :initCrc 0 :xorOut 0 :rev False)
        crc-zero (struct.pack "<I" 0)
        crc-old (cut data 22 (+ 22 4))
        data (replace-range 22 (+ 22 4) crc-zero data)
        crc-new (struct.pack "I" (crc-fun data))
        data (replace-range 22 (+ 22 4) crc-new data)
        (get pages comment-page) data)
  (.join b"" pages))


(defn parse-ogg [new-comments file]
  (with [f (open file "rb")]
    (setv data (.read f)))
  (setv data (ensure-comment data))
  (setv (, data ident) (parse-identity data))
  (setv (, data comment-header comment) (parse-comment new-comments data))
  (setv data (update-checksum data))
  (, data ident comment))
