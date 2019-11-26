(import [stalker_comment_editor.records.identification :as ident]
        [stalker_comment_editor.records.comment :as com]
        [stalker_comment_editor.records.comment_header :as comheader]
        [hy.contrib.hy-repr [hy-repr hy-repr-register]]
        [.utils [sound-type->int
                 update-namedtuple
                 insert-list
                 merge
                 replace-range
                 get-pages
                 get-sections
                 spy
                 find-in-pages]]
        [.consts [; *default-comment*
                  *ident-header-flag*
                  *comment-header-flag*]]
        struct
        crcmod)

;; (defn parse-identity [byte-seq]
;;   (as-> byte-seq $
;;         (ident.find $)
;;         ;; (.find $ *ident-header-flag*)     ; find the index of the identity header
;;         ;; (cut byte-seq $)                  ; get seq starting at the identity header
;;         (cut $ (len *ident-header-flag*)) ; read over the identity header
;;         (cut $ 0 22)                      ; get the identification body
;;         ;; (struct.unpack "<IBI3iB" $)
;;         (ident.unpack $)
;;         (ident.->Identification #* $)
;;         (, byte-seq $))
;;   ;; (setv index (.find byte-seq *ident-header-flag*)
;;   ;;       data (cut byte-seq index)
;;   ;;       data (cut data (len *ident-header-flag*))
;;   ;;       ident (Identification #* (struct.unpack "<IBI3iB" (cut data 0 22))))
;;   ;; (, byte-seq ident)
;;   )

;; (defn parse-comment
;;   [new-comments byte-seq]
;;   (if (in "sound_type" new-comments)
;;       (setv (get new-comments "sound_type")
;;             (sound-type->int (get new-comments "sound_type"))))

;;   (setv index (.find byte-seq *comment-header-flag*)
;;         data (cut byte-seq index)
;;         data (cut data (len *comment-header-flag*))
;;         (, header-length ) (struct.unpack "I" (cut data 0 4))
;;         header (comheader.->CommentHeader #*
;;                                (struct.unpack f"<I{header-length}sI"
;;                                               (cut data 0 (+ 8 header-length))))
;;         data (cut data (+ 8 header-length)))


;;   (as-> (cut data 0 4) com-length
;;         (struct.unpack "I" com-length)
;;         (get com-length 0)
;;         (unless (= com-length 24)
;;           (raise (ValueError "Ogg contains no stalker vorbis comment"))))


;;   (setv new-comment-bytes (->> new-comments
;;                                (merge com.DEFAULT)
;;                                com.pack
;;                                ;; (update-namedtuple $ *default-comment*)
;;                                ;; (struct.pack *comment-format* #** $)
;;                                )
;;         data (as-> data d
;;                    (cut d 4)
;;                    (replace-range 0 24 new-comment-bytes d))
;;         comment (com.->Comment -1 #* (struct.unpack "I3fIf" (cut data 0 24)))
;;         data (+ (cut byte-seq 0 (+ index
;;                                    (len *comment-header-flag*)
;;                                    header-length
;;                                    4
;;                                    4
;;                                    4))
;;                       data))
;;   (, data header comment))


;; (defn ensure-comment [byte-seq]
;;   (setv index (.find byte-seq *comment-header-flag*))
;;   (setv data (cut byte-seq index))
;;   (setv data (cut data (len *ident-header-flag*)))
;;   (setv (, header-length ) (struct.unpack "I" (cut data 0 4)))
;;   (setv header (comheader.->CommentHeader #*
;;                               (struct.unpack f"<I{header-length}sI"
;;                                              (cut data 0 (+ 8 header-length)))))
;;   (setv data (cut data (+ 8 header-length)))
;;   (setv (, comment-length) (struct.unpack "I" (cut data 0 4)))
;;   (unless (= comment-length 24)
;;     (setv default-comment-bytes (struct.pack *comment-format* #* *default-comment*))
;;     (setv index-of-split (+ index
;;                             (len *ident-header-flag*)
;;                             4
;;                             header-length))
;;     (setv byte-seq (replace-range index-of-split
;;                                   (+ index-of-split 4)
;;                                   (struct.pack "I" (+ header.num_comments 1))
;;                                   byte-seq))

;;     (setv start-length (len byte-seq))
;;     (setv byte-seq (insert-list (+ index-of-split 4)
;;                                 byte-seq
;;                                 (+ (struct.pack "I" 24) default-comment-bytes )))
;;     )
;;   byte-seq)



(defn update-checksum [byte-seq]
  (setv pages (get-pages byte-seq)
        comment-page (find-in-pages *comment-header-flag*  pages)
        data (get pages comment-page)
        packet-length (len (get (get-sections data) 0)))
   (setv
    crc-fun (crcmod.mkCrcFun 0x104c11db7 :initCrc 0 :xorOut 0 :rev False)
    crc-zero (struct.pack "<I" 0)
    crc-old (cut data 22 (+ 22 4))
    data (replace-range 22 (+ 22 4) crc-zero data)
    data (replace-range 27 28 (struct.pack "B" packet-length) data))

  (setv
    crc-new (struct.pack "I" (crc-fun data))
    data (replace-range 22 (+ 22 4) crc-new data)
    (get pages comment-page) data)

  (.join b"" pages))

(defn printhy [&rest args] (print #* (map hy-repr args)))

(defn parse-ogg
  [next-comments file]
  (with [f (open file "rb")]
    (setv data (.read f)))
  ;; (setv data (ensure-comment data))
  ;; (setv (, data ident) (parse-identity data))
  (setv (, data identification) (ident.parse data)
        (, data comment) (com.parse data :ensure-comment True)
        data (com.update-comment data comment next-comments)
        data (update-checksum data))
  ;; (setv (, data comment-header comment) (parse-comment new-comments data))
  (, data identification comment))


