(import [stalker_comment_editor.parsers.identification :as ident]
        [stalker_comment_editor.parsers.comment :as com]
        [hy.contrib.hy-repr [hy-repr hy-repr-register]]
        [.utils [replace-range
                 get-pages
                 get-sections
                 find-in-pages]]
        [.consts [*comment-header-flag*]]
        struct
        crcmod)

(defn update-checksum
  [byte-seq]
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


(defn parse-ogg
  [next-comments file]
  (with [f (open file "rb")]
    (setv data (.read f)))

  (setv (, data identification) (ident.parse data)
        (, data comment) (com.parse data :ensure-comment True)
        data (com.update-comment data comment next-comments)
        data (update-checksum data))

  (, data identification comment))


