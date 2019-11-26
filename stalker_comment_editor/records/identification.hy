(import struct)

(setv HEADER-FLAG (struct.pack "B6s" 1 b"vorbis"))

(defn ->Identification
  [vorbis-version
   audio-channel
   audio-sample-rate
   bitrate-maximum
   bitrate-nominal
   bitrate-minimum
   blocksize-0-1]
  {:vorbis-version vorbis-version
   :audio-channel audio-channel
   :audio-sample-rate audio-sample-rate
   :bitrate-maximum bitrate-maximum
   :bitrate-nominal bitrate-nominal
   :bitrate-minimum bitrate-minimum
   :blocksize-0-1 blocksize-0-1})


(defn find
  [seq]
  (setv index (.find seq HEADER-FLAG))
  (when (> index 0)
    (cut seq index)))


(defn unpack
  [seq]
  (when (= (len seq) 22)
    (struct.unpack "<IBI3iB" seq)))


(defn parse
  [seq]
  (as-> seq $
        (find $)
        (cut $ (len HEADER-FLAG))
        (cut $ 0 22)
        (unpack $)
        (->Identification #* $)
        (, seq $)))
