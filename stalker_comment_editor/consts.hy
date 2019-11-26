(import ; [stalker_comment_editor.records.comment [->Comment]]
        struct)

(setv ; *default-comment* (->Comment -1 3 2 100 1 0 50)
      *ident-header-flag* (struct.pack "B6s" 1 b"vorbis")
      *comment-header-flag* (struct.pack "B6s" 3 b"vorbis")
      *comment-format* "I3fIf"
      *sound-types* {134217856 "World ambient"
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
                     2149580800 "Weapon shooting"}
      *sound-types-lookup* (dfor (, k v) (.items *sound-types*) [v k]))
