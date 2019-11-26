(import struct
        [stalker_comment_editor.utils [replace-range insert-list merge]]
        [pprint [pprint]])


(defn ->Comment
  [body-index
   quality
   min-distance
   maximum-distance
   base-volume
   sound-type
   max-ai-distance
   &optional
   header-index
   vendor-length
   vendor
   num-comments]
  {:header {:index header-index
            :vendor-length vendor-length
            :vendor vendor
            :num-comments  num-comments}
   :body {:index body-index
          :quality quality
          :min-distance min-distance
          :maximum-distance maximum-distance
          :base-volume base-volume
          :sound-type sound-type
          :max-ai-distance max-ai-distance}})

(setv HEADER-FLAG (struct.pack "B6s" 3 b"vorbis")
      DEFAULT (->Comment -1 3 2 100 1 0 50))

(defn pack
  [comment]
  (setv attributes
        ((juxt
           ':quality
           ':min-distance
           ':maximum-distance
           ':base-volume
           ':sound-type
           ':max-ai-distance)
          comment))
  (struct.pack "I3fIf" #* attributes))

(defn find
  [seq]
  (.find seq HEADER-FLAG))

(defn contains-comment?
  [seq index]
  (as-> seq $
        (cut $ index)
        (cut $ 0 4)
        (struct.unpack "I" $)
        (first $)
        (= $ 24)))

(defn parse-header
  [seq index length]
  (as-> seq $
        (cut $ index)
        (cut $ 0 (+ 4 length 4)) ;; header block is 8 bytes long + header
        (struct.unpack f"<I{length}sI" $)))

(defn parse-comment
  [seq index]
  (as-> seq $
        (cut $ index)
        (cut $ 4) ;; drop comment length indicator
        (cut $ 0 24) ;; stalker comments are 24 bytes long
        (struct.unpack "I3fIf" $)))

(defn inject-default-comment
  [seq header-index header-length body-index]
  ;; Increment the number of comments by one
  (setv num-comments-index (+ header-index 4 header-length)
        num-comments (as-> seq $
                           (cut $ num-comments-index (+ num-comments-index 4))
                           (struct.unpack "I" $)
                           (first $))
        next-seq (replace-range
                   num-comments-index
                   (+ num-comments-index 4)
                   (struct.pack "I" (inc num-comments))
                   seq))
  ;; Inject the default comments into the comment block
  (setv default-bytes (pack DEFAULT)
        next-seq (insert-list body-index
                              next-seq
                              (+ (struct.pack "I" 24) default-bytes)))

  next-seq)

(defn update-comment
  [seq comment next-comments]
  (setv index (-> comment :body :index))
  (as-> comment $
        (:body $)
        (merge $ next-comments)
        (pack $)
        (insert-list (+ index 4) seq $)))

(defn parse
  [seq &optional [ensure-comment True]]
  (setv index (find seq))
  (when (> index 0)
    (setv header-length
          (as-> seq $
                (cut $ index)
                (cut $ (len HEADER-FLAG))
                (cut $ 0 4)
                (struct.unpack "I" $)
                (first $))
          header-index (+ index (len HEADER-FLAG))
          comment-index (+ header-index 4 header-length 4))

    (if-not (contains-comment? seq comment-index)
            (if ensure-comment
                (setv seq (injex-default-comment seq comment-index))
                (raise (ValueError "ERROR: Ogg contains no stalker vorbis comment"))))

    (, seq (->Comment
             comment-index
             #* (parse-comment seq comment-index)
             header-index
             #* (parse-header seq header-index header-length)))))
