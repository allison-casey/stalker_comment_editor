(import [collections [namedtuple]])

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
