;; One-time bootstrap: converts ../thecli/manifest.json into tools.edn,
;; the ToolConfig format convocli actually reads at runtime (see
;; convocli.allium). Not part of the running app - a dev utility for
;; producing/refreshing a starting point, which the user then edits by
;; hand (descriptions, bespoke mapper functions, etc).
;;
;; Usage: bb scripts/generate_tools.clj [manifest-path] [out-path]

(require '[cheshire.core :as json])

(def type->schema-type
  {"string"  "string"
   "int"     "number"
   "boolean" "boolean"
   "json"    "object"
   "keyword" "string"})

(defn param->property [{:keys [flag type values note ref]}]
  (let [base-desc (or note (str flag " parameter"))
        desc (if ref (str base-desc " (ref: " ref ")") base-desc)
        prop {:type (get type->schema-type type "string")
              :description desc}]
    (if values (assoc prop :enum values) prop)))

(defn generic-mapper-source [cli-prefix]
  (str
   "(fn [args-json]\n"
   "  (let [args (json-parse args-json)]\n"
   "    (str \"" cli-prefix "\"\n"
   "         (apply str\n"
   "                (map (fn [[k v]]\n"
   "                       (str \" --\" (name k) \" \\\"\"\n"
   "                            (if (string? v) v (json-encode v))\n"
   "                            \"\\\"\"))\n"
   "                     args)))))"))

(defn command->tool-config [group-name cmd-name cmd-def]
  (let [params (:params cmd-def)
        props (into {} (map (fn [p] [(:flag p) (param->property p)]) params))
        required (vec (map :flag (filter :required params)))
        cli-prefix (str (name group-name) " " (name cmd-name))]
    {:name (str (name group-name) "_" (name cmd-name))
     :description (str "Runs `" cli-prefix "` on the Event Modeling CLI")
     :parameters-schema {:type "object"
                         :properties props
                         :required required}
     :mapper-source (generic-mapper-source cli-prefix)}))

(defn manifest->tool-configs [manifest]
  (vec (mapcat (fn [[group-name commands]]
                 (map (fn [[cmd-name cmd-def]]
                        (command->tool-config group-name cmd-name cmd-def))
                      commands))
               (:groups manifest))))

(defn -main [& [manifest-path out-path]]
  (let [manifest-path (or manifest-path "../thecli/manifest.json")
        out-path (or out-path "tools.edn")
        manifest (json/parse-string (slurp manifest-path) true)
        tool-configs (manifest->tool-configs manifest)]
    (spit out-path (with-out-str (clojure.pprint/pprint tool-configs)))
    (println (str "Wrote " (count tool-configs) " tool configs to " out-path))))

(apply -main *command-line-args*)
