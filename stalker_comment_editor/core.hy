;;;; stalker_comment_editor -- Cli tool for bulk editing of stalker ogg vorbis comments.
(import [.utils [render-ogg]]
        [.parsers [parse-ogg]]
        [pathlib [Path]]
        [jsonschema [validate]]
        [jsonschema.exceptions [ValidationError]]
        [pkg_resources [resource_stream]]
        click
        yaml)


(with-decorator
  (click.command)
  (click.argument "manifest" :type (click.File "r"))
  (click.option "-p" "--print" "_print" :is_flag True)
  (defn cli [manifest _print]
    "Cli tool for bulk editing of S.T.A.L.K.E.R. ogg vorbis commets."
    (setv f (resource_stream --name-- "schema.yaml")
          schema (yaml.load f :Loader yaml.FullLoader)
          manifest (yaml.load manifest :Loader yaml.FullLoader)
          cwd (Path.cwd))
    (try (validate :instance manifest :schema schema)
         (except [e ValidationError]
           (click.echo e)
           ( return 1 )))
    (for [entry manifest]
      (setv in-path (as-> "in-path" p (get entry p) (cwd.joinpath p))
            out-path (as-> "out-path" p (get entry p) (cwd.joinpath p))
            new-comments (if (in "comment" entry)
                             (get entry "comment")
                             {}))

      (if (= in-path out-path)
          (click.confirm (.join " " ["in-path and out-path are the same"
                                     "and will result in overwritting source files."
                                     "Would you like to continue"])
                         :abort True
                         :default False))

      (in-path.mkdir :parents True :exist_ok True)
      (out-path.mkdir :parents True :exist_ok True)
      (setv files (in-path.glob (get entry "glob")))

      (for [file files]
        (unless (= file.suffix ".ogg") continue)
        (print (file.relative_to in-path))
        (try
          (setv (, data ident header) (parse-ogg new-comments file)
                file-path (out-path.joinpath (file.relative_to in-path)))
          (.mkdir (. file-path parent) :parents True :exist_ok True)
          (with [out (open file-path "wb")]
            (out.write data))
          (if _print
              (do (click.echo (render-ogg (. file stem) ident header))
                  (click.echo (+ "\n" (* "=" 25) "\n"))))
          (except [e ValueError]
            (click.echo e)))))))

(defmain [&rest args]
  (cli))



