{:user {:plugins      [[cider/cider-nrepl "0.15.0-SNAPSHOT"]
                       [refactor-nrepl "2.3.0-SNAPSHOT"]
                       [lein-exec "0.3.6"]]
        :dependencies [[alembic "0.3.2"]
                       [org.clojure/tools.nrepl "0.2.12"]]}}
        :aliases {"hue" ["with-profile" "user,hue" "repl"]}}

  :hue {:repl-options {:init-ns me.raynes.clhue.repl
                        :init (setup)}
        :dependencies [[me.raynes/clhue "0.1.2"]]}
}
