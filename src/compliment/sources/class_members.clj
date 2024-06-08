(ns compliment.sources.class-members
  "Completion for both static and non-static class members."
  (:require [clojure.string :refer [join]]
            [compliment.sources :refer [defsource]]
            [compliment.sources.local-bindings :refer [bindings-from-context]]
            [compliment.utils :as utils :refer [fuzzy-matches-no-skip?
                                                resolve-class
                                                clojure-1-12+?]])
  (:import [java.lang.reflect Field Member Method Modifier Constructor Executable]))

(defn static?
  "Tests if class member is static."
  [^Member member]
  (Modifier/isStatic (.getModifiers member)))

(defn constructor?
  "Tests if member is a constructor."
  [^Member member]
  (instance? Constructor member))

;; ## Regular (non-static) members

(def members-cache
  "Stores cache of all non-static members for every namespace."
  (atom {}))

(defn- demunge-deftype-field-name
  "If the member is a deftype field, change .x_y to .x-y for compatibility. See
  https://github.com/alexander-yakushev/compliment/issues/33."
  [^Member m, ^Class c, ^String name]
  (if (and (instance? Field m)
           (.isAssignableFrom clojure.lang.IType c))
    (.replaceAll name "_" "-")
    name))

(defn populate-members-cache
  "Populate members cache of class members for `ns` from the given list of
  classes. `imported-classes-cnt` is a number that indicates the current number
  of imported classes in this namespace."
  [ns classes imported-classes-cnt]
  (let [members
        (for [^Class class classes
              ^Member member (concat (.getMethods class) (.getFields class))
              :when (not (static? member))]
          (let [dc (.getDeclaringClass member)
                name (.getName member)
                demunged-name (demunge-deftype-field-name member dc name)]
            [demunged-name
             (if (= dc class)
               member
               (if (instance? Method member)
                 (.getMethod dc name (.getParameterTypes ^Method member))
                 (.getField dc name)))]))

        cache
        (reduce (fn [cache [full-name m]]
                  (assoc! cache full-name (conj (cache full-name []) m)))
                (transient {}) members)]
    (swap! members-cache assoc ns {:classes (set classes)
                                   :imported-classes-cnt imported-classes-cnt
                                   :members (persistent! cache)})))

(defn update-cache
  "Updates members cache for a given namespace if necessary."
  ([ns] (update-cache ns nil))
  ([ns context-class]
   (let [imported-classes (reduce-kv (fn [acc _ mapping]
                                       (if (class? mapping)
                                         (conj acc mapping)
                                         acc))
                                     [] (ns-map ns))
         imported-classes-cnt (count imported-classes)
         cache (@members-cache ns)]
     (when (or (nil? cache)
               (not= (:imported-classes-cnt cache) imported-classes-cnt)
               (and context-class
                    (not (contains? (:classes cache) context-class))))
       (let [classes (cond-> (into (set imported-classes) (:classes cache))
                       context-class (conj context-class))]
         (populate-members-cache ns classes imported-classes-cnt))))))

(defn get-all-members
  "Returns all non-static members for a given namespace."
  [ns context-class]
  (update-cache ns context-class)
  (get-in @members-cache [ns :members]))

(defn class-member-symbol?
  "Tests if `x` looks like a non-static class member,
  e.g. \".getMonth\" or (with `clj-1-12+` option set) \"java.util.Date/.getMonth\".

  When true, yields `[klass-name method-name]`."
  ([x] (class-member-symbol? x nil))
  ([^String x {:keys [clj-1-12+]}]
   (when-let [[_ klass-name member-name]
              (re-matches #"(?x)
                            (?=.)  # at least one character
                            (?:    # optional 'a.b/'
                              ([^\/\:\.][^\:]*) # capture 'a.b'
                              (?<!\.)  # deny 'a.b./'
                              \/)?
                            (?:    # optional '.method'
                              \.([^.]*))?  # capture 'method'"
                          x)]
     (when (or clj-1-12+ (nil? klass-name))
       [klass-name member-name]))))

(defn camel-case-matches?
  "Tests if prefix matches the member name following camel case rules.
  Thus, prefix `getDeF` matches member `getDeclaredFields`."
  [prefix member-name]
  (fuzzy-matches-no-skip? prefix member-name (fn [ch] (Character/isUpperCase ^char ch))))

^{:lite nil}
(defn try-get-object-class
  "Tries to get the type of the object from the context, which the member will be
  applied to. Object should be a symbol resolving to a Var or have a type tag."
  [ns context]
  (when (= (:idx (first context)) 0)
    (let [form (second (:form (first context)))]
      (if-let [tag (or
                    ;; Form might have an immediate tag...
                    (:tag (meta form))
                    ;; ...or a tag somewhere in local scope. Note how getting
                    ;; an element from a set can return itself but with meta.
                    (:tag (meta (get (set (bindings-from-context context ns)) form))))]
        ;; We have a tag - try to resolve the class from it.
        (resolve-class ns tag)
        ;; Otherwise, try to resolve symbol to a Var,
        ;; or literal to class.
        (or (utils/var->class ns form)
            (utils/invocation-form->class ns form)
            (utils/literal->class form))))))

(defn members-candidates
  "Returns a list of Java non-static fields and methods candidates."
  [prefix ns context]
  (when-let [[klass-name prefix] (class-member-symbol? prefix {:clj-1-12+ (clojure-1-12+?)})]
    (let [prefix      (or prefix "")
          qualified?  (seq klass-name)
          inparts?    (re-find #"[A-Z]" prefix)
          klass       ^{:lite nil} (if qualified?
                                     (resolve-class ns (symbol klass-name))
                                     (try-get-object-class ns context))
          all-members (when-not (and qualified? (not klass))
                        (get-all-members ns klass))]
      (for [[member-name members] all-members
            :when (and (if inparts?
                         (camel-case-matches? prefix member-name)
                         (.startsWith ^String member-name prefix))
                       ^{:lite true}
                       (or (not klass)
                           (some (fn [m] (.isAssignableFrom (.getDeclaringClass ^Member m) klass))
                                 members)))]
        {:candidate (cond->> (str "." member-name)
                      (and qualified? klass) (str klass-name "/"))
         :type (if (instance? Method (first members))
                 :method :field)}))))

;; ### Member documentation

^{:lite nil}
(defn type-to-pretty-string
  "Takes a type (either a class or a primitive) and returns it's
  human-readable name."
  [^Class t]
  (if (or (.isLocalClass t)
          (.isMemberClass t))
    (.getName t)
    (.getSimpleName t)))

^{:lite nil}
(defn doc-method-parameters
  "Takes a list of method parameters and stringifies it."
  [parameters]
  (->> parameters
       (map type-to-pretty-string)
       (interpose " ")
       join
       (format "(%s)")))

(defn- class&members->doc-dispatch [[^Class _cl members]]
  (class (first members)))

(defmulti ^:private class&members->doc #'class&members->doc-dispatch)

(defmethod class&members->doc java.lang.reflect.Method
  [[^Class cl members]]
  (let [^Member f-mem (first members)]
    (str (.getName cl) "." (.getName f-mem)
         (join
          (map (fn [^Method member]
                 (str "\n  " (doc-method-parameters (.getParameterTypes member))
                      " -> " (type-to-pretty-string (.getReturnType ^Method member))
                      " (" (Modifier/toString (.getModifiers member)) ")"))
               members))
         "\n")))

(defmethod class&members->doc java.lang.reflect.Constructor
  [[^Class cl members]]
  (str (.getName cl) ".new"
       (join
        (map (fn [^Constructor member]
               (str "\n  " (doc-method-parameters (.getParameterTypes member))))
             members))
       "\n"))

(defmethod class&members->doc java.lang.reflect.Field
  [[^Class cl members]]
  (let [^Member f-mem (first members)]
    (str (.getName cl) "." (.getName f-mem)
         (str " = " (try (.get ^Field f-mem nil)
                         (catch Exception _e "?"))
              " (" (type-to-pretty-string (.getType ^Field f-mem)) ")\n"
              (Modifier/toString (.getModifiers f-mem)))
         "\n")))

^{:lite nil}
(defn create-members-doc
  "Takes a list of members (presumably with the same name) and turns
  them into a docstring."
  [members]
  (let [sort-members (fn [[^Class cl members]]
                       [cl (sort-by #(.getParameterCount ^Executable %)
                                    (distinct members))])]
    (->> members
         (group-by (fn [^Member m] (.getDeclaringClass m)))
         (map (comp class&members->doc sort-members))
         (interpose "\n")
         join)))

(declare non-static-members)
(defn- qualified-member-doc [ns klass-name member-name]
  (some-> (symbol klass-name)
          (->> (resolve-class ns))
          (non-static-members)
          (get member-name)
          create-members-doc))

(defn- non-qualified-member-doc [ns member-name]
  (update-cache ns)
  (when-let [members (get-in @members-cache [ns :members member-name])]
    (create-members-doc members)))

^{:lite nil}
(defn members-doc
  "Documentation function for non-static members."
  [member-str ns]
  (when-let [[klass-name member-name]
             (class-member-symbol? member-str {:clj-1-12+ (clojure-1-12+?)})]
    (if (seq klass-name)
      (qualified-member-doc ns klass-name member-name)
      (non-qualified-member-doc ns member-name))))

^{:lite nil}
(defn classname-doc [^Class class]
  (let [members (group-by static? (concat (.getMethods class)
                                          (.getFields class)))
        [static non-static] (for [flag [true false]]
                              (->> (for [^Member m (members flag)]
                                     (.getName m))
                                   distinct
                                   (interpose ", ")
                                   join))]
    (str (.getName class) "\n\n"
         " Non-static members:\n  " non-static "\n\n"
         " Static members:\n  " static "\n")))

^{:lite '(defsource :compliment.lite/members :candidates #'members-candidates)}
(defsource ::members
  :candidates #'members-candidates
  :doc #'members-doc)

;; ## Static members

(defn static-member-symbol?
  "Tests if `x` looks like a static member symbol, returns parsed parts."
  [^String x]
  (re-matches #"([^\/\:\.][^\:]*)\/(.*)" x))

(def ^:private class-members-cache
  "Members by class"
  (atom {}))

(defn- populate-class-members-cache
  "Populates qualified methods cache for a given class."
  [^Class class]
  (let [member->cache-key #(get {:constructor "new"} %2 (.getName ^Member %1))
        methods-by-type   (group-by (comp {true :static-method false :method} static?)
                                    (.getMethods class))
        members-by-type   (merge methods-by-type
                                 {:static-field (.getFields class)
                                  :constructor  (.getConstructors class)})]
    (swap! class-members-cache assoc class
           (reduce (fn [cache [type members]]
                     (reduce
                      (fn [acc ^Member m]
                        (update acc (member->cache-key m type) (fnil conj (with-meta [] {:type type})) m))
                      cache members))
                   {}
                   members-by-type))))

(defn static-members
  "Returns all static members for a given class."
  [^Class class]
  (let [class-members  (or (@class-members-cache class)
                           (get (populate-class-members-cache class) class))
        matching-types (cond-> #{:static-field :static-method}
                         (clojure-1-12+?) (conj :constructor))]
    (->> class-members
         (filter (comp matching-types :type meta val))
         (into {}))))

(defn non-static-members
  "Returns all non-static members for a given class."
  [^Class class]
  (let [class-members (or (@class-members-cache class)
                          (get (populate-class-members-cache class) class))]
    (->> class-members
         (filter (comp #{:method} :type meta val))
         (into {}))))

(defn static-members-candidates
  "Returns a list of static member candidates."
  [^String prefix, ns _context]
  (when-let [[_ cl-name member-prefix] (static-member-symbol? prefix)]
    (when-let [cl (resolve-class ns (symbol cl-name))]
      (let [inparts? (re-find #"[A-Z]" member-prefix)]
        (for [[^String member-name [member]] (static-members cl)
              :when (if inparts?
                      (camel-case-matches? member-prefix member-name)
                      (.startsWith member-name member-prefix))]
          {:candidate (str cl-name "/" member-name)
           :type (cond
                   (constructor? member) :constructor
                   (instance? Method member) :static-method
                   :else :static-field)})))))

^{:lite nil}
(defn resolve-static-member
  "Given a string representation of a static member returns Member object."
  [^String member-str ns]
  (let [[cl-name member-name] (.split member-str "/")]
    (when-let [cl (resolve-class ns (symbol cl-name))]
      (get (static-members cl) member-name))))

^{:lite nil}
(defn static-member-doc
  "Given a member name and class returns its docstring."
  [member-str ns]
  (when (static-member-symbol? member-str)
    (let [member (resolve-static-member member-str ns)]
      (when member
        (create-members-doc member)))))

^{:lite (defsource :compliment.lite/static-members :candidates #'static-members-candidates)}
(defsource ::static-members
  :candidates #'static-members-candidates
  :doc #'static-member-doc)
