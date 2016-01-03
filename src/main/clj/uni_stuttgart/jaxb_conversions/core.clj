(ns uni-stuttgart.jaxb-conversions.core
  (:require [taoensso.timbre :as t]
            [clojure.reflect :as r]
            [clojure.string :as s]
            [camel-snake-kebab.core :as cs])
  (:import [javax.xml.bind JAXBContext]
           [java.lang.reflect ParameterizedType Field]
           [java.io StringReader]))

(t/refer-timbre)


(defn- xml-str->jaxb-obj
  "Converts XML string into a JAX-B object using the JAX-B context"
  [m]
  {:pre [(:jaxb-context m)
         (:xml-str m)]}
  ;; (debug "XML string will be converted into jaxb object")
  (-> m
      :jaxb-context
      (.createUnmarshaller)
      (.unmarshal (StringReader. (:xml-str m)))))

(defn- add-generic-type-of-instance-method
  [m]
  {:pre [(:current-obj m)
         (:current-property-getter-name m)]
   :post [(:current-property-generic-type %)]}
  ;; (debug "Current object is" (:current-obj m))
  ;; (debug "Current map is" m)
  (assoc m :current-property-generic-type
         (or (and (map? (:current-map m)) ((:type-keyword m) (:current-map m)))
             (-> m
                 :current-obj
                 type
                 (.getMethod (:current-property-getter-name m) (into-array java.lang.Class []))
                 .getGenericReturnType
                 (.getActualTypeArguments)
                 (get 0)))))


(defn- add-jaxb-object
  "Add jaxb object"
  [m]
  ;; (debug "Adding jaxb object")
  (assoc m :jaxb-obj (xml-str->jaxb-obj m)))

(defn- add-empty-resulting-map
  "Adds an empty result map"
  [m]
  ;; (debug "Adding empty resulting map")
  (assoc m :resulting-map {}))

(defn- jaxb-obj-keyword
  [m]
  "Gets the keyword for a given jaxb element object under key :jaxb-obj"
  {:pre [(:jaxb-obj m)]}
  ;; (debug "Getting keyword for jaxb element" )
  (-> m
      :jaxb-obj
      bean
      :name
      .getLocalPart
      keyword))

(defn- add-jaxb-obj-keyword
  [m]
  "Adds the keyword for a given jaxb object under key :jaxb-obj"
  ;; (debug "Adding keyword for jaxb element")
  (assoc m :jaxb-obj-keyword (jaxb-obj-keyword m)))

(defn- add-object-bean-map
  [m]
  ;; (debug "Adding object bean map")
  (->> m
       :current-obj
       bean
       (assoc m :current-map)))


(defn- remove-class-from-current-map
  [m]
  (update-in m [:current-map] #(dissoc % :class)))

(defn- remove-nils-from-current-map
  [m]
  (update-in m [:current-map] #(filterv second %)))

(defn- remove-empty-values-from-current-map
  [m]
  (update-in m [:current-map] #(remove (fn [items] (and (vector? (second items)) (not (seq (second items))))) %)))

(defn- remove-blank-values-from-current-map
  [m]
  (update-in m [:current-map] #(remove (fn [items] (and (string? (second items)) (s/blank? (second items)))) %)))

(defn- clean-up-object-bean-map
  [m]
  {:pre [(:current-map m)]}
  ;; (debug "Cleaning bean map" (:current-map m))
  (-> m
      remove-class-from-current-map
      remove-nils-from-current-map
      remove-blank-values-from-current-map
      remove-empty-values-from-current-map
      (update-in [:current-map] #(into {} %))))


(defn- add-declared-annotations
  [m]
  (->> m
       :target-field-class
       .getDeclaredAnnotations
       (assoc m :annotations)))

(defn- get-xml-type-annotations
  [m]
  {:pre [(:annotations m)]}
  (->> m
       :annotations
       (filterv #(= javax.xml.bind.annotation.XmlType (.annotationType %)))))

(defn- get-xml-enum-annotations
  [m]
  {:pre [(:annotations m)]}
  (->> m
       :annotations
       (filterv #(= javax.xml.bind.annotation.XmlEnum (.annotationType %)))))

(defn- get-xml-value-annoations
  [m]
  {:pre [(:annotations m)]}
  (->> m
       :annotations
       (filterv #(= javax.xml.bind.annotation.XmlValue (.annotationType %)))))


(defn- complex-type?
  "Search for the annoation"
  [m]
  (and (class? (:target-field-class m))
       (-> m
           add-declared-annotations
           get-xml-type-annotations
           first)))

(defn- simple-type?
  "Search for the annoation"
  [m]
  (and (class? (:target-field-class m))
       (or (-> m
               add-declared-annotations
               get-xml-enum-annotations
               first)
           (-> m
               add-declared-annotations
               get-xml-value-annoations
               first))))


(defn- get-declared-field
  [m]
  {:pre [(:current-type m) (:field-name m)]}
  ;; (debug "Current obj" (:current-type m) (:field-name m))
  (try (.getDeclaredField (:current-type m) (:field-name m))
       (catch java.lang.NoSuchFieldException e
         (do
           ;; (warn "Method is not there checking superclasses" e)
           (if (.getSuperclass (:current-type m))
             (get-declared-field (assoc m :current-type (.getSuperclass (:current-type m)))))))))


(defn- add-annotations-of-property-key
  [m]
  {:pre [(:current-property-key m)]}
  (let [declared-field (->> m
                            :current-property-key
                            cs/->camelCaseString
                            (assoc m :current-type (type (:current-obj m)) :field-name)
                            get-declared-field)]
    (assoc m :annotations (and declared-field
                               (-> declared-field
                                   .getDeclaredAnnotations)))))

(defn- xml-id?
  [m]
  (and (:current-property-key m)
       (:current-obj m)
       (->> m
            add-annotations-of-property-key
            :annotations
            (filterv #(= javax.xml.bind.annotation.XmlSchemaType (.annotationType %)))
            (filterv #(= "ID" (.name %)))
            first)))

(defn- xml-id-ref?
  [m]
  (and (:current-property-key m)
       (:current-obj m)
       (->> m
            add-annotations-of-property-key
            :annotations
            (filterv #(= javax.xml.bind.annotation.XmlSchemaType (.annotationType %)))
            (filterv #(= "IDREF" (.name %)))
            first)))

;; dev.clojure.org/jira/browse/CLJ-1841
(defn- resolve-id-ref-str
  [m]
  ; (debug "Current obj as bean" ((second (first (into [] (bean (:current-obj m)))))))
  ((->> m
              :current-obj
              bean
              (filterv #(-> m
                            (assoc :current-property-key (first %))
                            xml-id?))
              first
              second)))

(defn- other-attributes?
  [m]
  (or
   (= :otherAttributes (:current-property-key m))
   (= :other-attributes (:current-property-key m))))

(defn- add-field-type
  [m]
  {:pre [(:target-field-class m)]
   :post [(:target-field-type %)]}
  ; (debug "Adding field type for" m)
  (cond
    (xml-id-ref? m)
    (assoc m :target-field-type :xml-id-ref)

    (simple-type? m)
    (assoc m :target-field-type :simple-type)

    (complex-type? m)
    (assoc m :target-field-type :complex-type)

    (other-attributes? m)
    (assoc m :target-field-type :other-attributes)

    (= (:current-property-key m)  :any)
    (assoc m :target-field-type :any)

    (xml-id? m)
    (assoc m :target-field-type :xml-id)

    (not (class? (:target-field-class m)))
    (assoc m :target-field-type :else)

    (.isAssignableFrom javax.xml.datatype.XMLGregorianCalendar (:target-field-class m))
    (assoc m :target-field-type :time)

    (.isAssignableFrom java.util.Map (:target-field-class m))
    (assoc m :target-field-type :map)

    (.isAssignableFrom java.util.List (:target-field-class m))
    (assoc m :target-field-type :list)

    (.isAssignableFrom javax.xml.bind.JAXBElement (:target-field-class m))
    (assoc m :target-field-type :jaxb-element)

    (.isAssignableFrom javax.xml.namespace.QName (:target-field-class m))
    (assoc m :target-field-type :qname)

    (or (.isAssignableFrom java.lang.Integer (:target-field-class m))
        (.isAssignableFrom (type int) (type (:target-field-class m))))
    (assoc m :target-field-type :int)

    :else
    (assoc m :target-field-type :else)))

(declare jaxb-type->map-recursively)
(declare jaxb-obj->map)

(defn- resolve-field
  [m]
  {:pre [(:current-property-key m)
         (:target-field-type m)
         (:current-property-val m)]}
  ;; (debug "Resolving field" (:current-property-key m) (:current-property-val m))
  (cond
    ;; another type that needs to be resolved
    (= (:target-field-type m) :complex-type)
    (do
      ;; (debug "It's a complex type" (:current-property-key m) (:current-property-val m))
      [(:current-property-key m)
       (-> m
           (assoc :current-obj (:current-property-val m))
           jaxb-type->map-recursively)])
    ;; simple type
    (= (:target-field-type m) :simple-type)
    (do
      ;; (debug "It's a simple type" (:current-property-key m) (:current-property-val m))
      [(:current-property-key m)
       (.value (:current-property-val m))])
    ;; in case it's a list return it as a list
    (= (:target-field-type m) :list)
    (do
      ;; (debug "It's a list field" (:current-property-key m) (:current-property-val m))
      [(:current-property-key m)
       (mapv
       #(-> m
            (assoc :current-obj %)
            jaxb-type->map-recursively)
       (:current-property-val m))])

    (= (:target-field-type m) :map)
    (do
      ;; (debug "It's a map field" (:current-property-key m) (:current-property-val m))
      [(:current-property-key m)
       (into {} (:current-property-val m))])

    (= (:target-field-type m) :qname)
    (do
      ;; (debug "It's a qname" (:current-property-key m) (:current-property-val m))
      [(:current-property-key m)
       (.toString (:current-property-val m))])

    (= (:target-field-type m) :jaxb-element)
    (do
      ;; (debug "It's a map field" (:current-property-key m) (:current-property-val m))
      [(:current-property-key m)
       (-> m
           (assoc :jaxb-obj (:current-property-val m))
           jaxb-obj->map)])

    (= (:target-field-type m) :xml-id-ref)
    (do
      ;; (debug "It's a map field" (:current-property-key m) (:current-property-val m))
      [(:current-property-key m)
       (-> m
           (assoc :current-obj (:current-property-val m))
           resolve-id-ref-str)])

    (= (:target-field-type m) :any)
    (do
      ;; (debug "It's any field" (:current-property-key m) (:current-property-val m) (vector? (:current-property-val m)) (type (:current-property-val m)))
      [(:current-property-key m)
       (cond
         (instance? java.util.List (:current-property-val m))
         (mapv
          #(-> m
               (assoc :current-obj %)
               jaxb-type->map-recursively)
          (:current-property-val m)))])

    (= (:target-field-type m) :other-attributes)
    (do
      [(:current-property-key m)
       (->> m
            :current-property-val
            (into {})
            (mapv #(vector (str (first %)) (second %)))
            (into {}))])

    :else
    [(:current-property-key m) (:current-property-val m)]))


(defn- remove-empty-vectors
  [v]
  (filterv
   seq
   v))

(defn- add-first-matching-key-val
  [m]
  {:pre [(:type-mappings m) (:current-obj-class-str m)]}
  (->> m
       :type-mappings
       (filterv #(= (second %) (:current-obj-class-str m)))
       first
       (assoc m :first-match)))

(defn- get-first-match-key-or-str
  [m]
  (if (:first-match m)
    (first (:first-match m)) ;; first match is a key val pair return only key
    nil))

(defn- resolve-str-into-entity-keyword
  [m]
  {:pre [(:current-obj-class-str m)]}
  (if (:type-mappings m)
    (or (-> m
            add-first-matching-key-val
            get-first-match-key-or-str)
        (:current-obj-class-str m))
    (:current-obj-class-str m)))


(defn- add-current-obj-type-string
  [m]
  {:pre [(:current-obj m)]
   :post [(:current-obj-class-str %)]}
  (assoc m :current-obj-class-str (-> m
      :current-obj
      type
      str
      (clojure.string/split #" ")
      second)))

(defn- add-type-class
  [m]
  {:pre [(:current-obj m)]
   :post [(get-in % [:current-map (:type-keyword m)])]}
  (assoc-in m [:current-map (:type-keyword m)] (->> m
                                                    add-current-obj-type-string
                                                    resolve-str-into-entity-keyword)))

(defn- recreate-fields-with-resolution
  [m]
  ;; (debug "Resolving fields of a map recursively" (:current-map m))
  (->> m
       add-type-class
       :current-map
       (mapv
        #(do
           ;; (debug "Current property" (first %))
           (-> m
             (assoc :current-property-key (if-not (= (first %) (:type-keyword m))
                                            (cs/->kebab-case-keyword (first %))
                                            (first %)))
             (assoc :current-property-val (second %))
             (assoc :target-field-class (type (second %)))
             add-field-type
             resolve-field)))
       remove-empty-vectors
       (into {})))

(defn- jaxb-type->map-recursively
  [m]
  ;; (debug "jaxb-type->map-recursively")
  (-> m
      add-object-bean-map
      clean-up-object-bean-map
      recreate-fields-with-resolution))

(defn jaxb-type-obj->map-recursively
  "Convert given jaxb type object into a map. Jaxb type object refers
  to any class that is generated from a schema. During this process
  each object is converted into a map using bean clojure.core/bean and
  afterwards each object value in the map is converted
  recursively. During the conversion for each object an
  additional ::type-class object-class pair is added. The :type-class
  keyword can be customized by specifiying :type-keyword in the
  additional map. Moreover, type-keywords can be used to add keywords
  instead of namespaced class strings. The structure of type-mappings
  map is {:class-type-keyword \"namespace.classname}\". During
  transformation current object class name is searched in the values
  of type-mappings and first matching keyword is used as type
  identifer of ::type-class"
  [jaxb-type-obj & m]
  (when-not (:type-keyword (first m))
    (warn ":type-keyword was not specified in" m ::type-class "will be used as type keyword..."))
  (-> {:current-obj jaxb-type-obj
       :type-keyword (or (:type-keyword (first m)) ::type-class)
       :type-mappings (:type-mappings (first m))}
      add-object-bean-map
      clean-up-object-bean-map
      recreate-fields-with-resolution))

(defn- add-jaxb-element-value
  [m]
  ;; (debug "add-jaxb-element-value")
  (->> m
       :jaxb-obj
       .getValue
       (assoc m :current-obj)))


(defn- jaxb-obj->map
  [m]
  ;; (debug "jaxb-obj->map")
  (->> m
       add-jaxb-obj-keyword
       add-jaxb-element-value
       jaxb-type->map-recursively
       (assoc-in {} [(jaxb-obj-keyword m)])))

(defn- add-map-of-jaxb-object
  [m]
  ;; (debug "add-map-of-jaxb-object")
  (assoc m :resulting-map (jaxb-obj->map m)))

(defn- xml->map
  "Accepts an xml string and its jaxb class and converts it into maps"
  [xml-str jaxb-class & jaxb-classes]
  (:pre [(string? xml-str)
         (class? jaxb-class)])
  ;; (debug jaxb-classes)
  (-> {:jaxb-context (JAXBContext/newInstance (apply into-array [jaxb-class] jaxb-classes))
       :xml-str xml-str}
      add-jaxb-object
      add-map-of-jaxb-object
      :resulting-map))

(defn xml-element-obj->map
  "Convert given javax.xml.bind.JAXBElement<T> object into a map. Jaxb
  type object refers to any class that is generated from a
  schema. During this process each object is converted into a map
  using bean clojure.core/bean and afterwards each object value in the
  map is converted recursively. During the conversion for each object
  an additional ::type-class object-class pair is
  added. The :type-class keyword can be customized by
  specifiying :type-keyword in the additional map. Moreover,
  type-keywords can be used to add keywords instead of namespaced
  class strings. The structure of type-mappings map is
  {:class-type-keyword \"namespace.classname}\". During transformation
  current object class name is searched in the values of type-mappings
  and first matching keyword is used as type identifer
  of ::type-class"
  [xml-obj & m]
  (when-not (:type-keyword (first m))
    (warn ":type-keyword was not specified." ::type-class "will be used as type keyword..."))
  (-> {:jaxb-obj xml-obj
       :type-keyword (or (:type-keyword (first m)) ::type-class)
       :type-mappings (:type-mappings (first m))}
      add-map-of-jaxb-object
      :resulting-map))



(declare add-members-recursively)

(defn- merge-new-members
  [m new-members]
  ;; (debug "New members are" new-members)
  (update-in m [:members] #(into new-members %))) ;; new members should replace old ones

(defn- call-again-if-base-not-nil
  [m]
  ;; (debug "Calling again for current reflection map:" (:current-reflect-map m))
  ;; (debug "Calling again for the bases:" (:bases (:current-reflect-map m)))
  (if (:bases (:current-reflect-map m))
    (->> m
         :current-reflect-map
         :bases
         (mapv #(-> m
                    (assoc :current-reflect-map (r/reflect (eval %)))
                    add-members-recursively))
         (reduce #(->> %2
                       :members
                       (merge-new-members %1))))
    m))



(defn- add-members-recursively
  [m]
  {:pre [(:current-reflect-map m)]}
  (->> m
       :current-reflect-map
       :members
       (merge-new-members m)
       call-again-if-base-not-nil))

(defn- add-property-member-map
  [m]
  {:pre [(:current-member-name m)
         (:current-obj m)]}
  ;; (debug "Adding property map for the following m" m)
  (->> (assoc m :members #{})
       :current-obj
       class
       r/reflect
       (assoc m :current-reflect-map)
       add-members-recursively
       :members
       (filter #(= (str (:name %))
                   (:current-member-name m)))
       first
       (assoc m :current-property-member-map)))

(defn- add-propperty-getter-member-map
  [m]
  {:pre [(:current-property-getter-name m)]}
  (->> (assoc m :current-member-name (:current-property-getter-name m))
       add-property-member-map
       :current-property-member-map
       (assoc m :current-property-getter-map)))

(defn- add-propperty-setter-member-map
  [m]
  {:pre [(:current-property-setter-name m)]}
  ;; (debug "Adding property setter" m)
  (->> (assoc m :current-member-name (:current-property-setter-name m))
       add-property-member-map
       :current-property-member-map
       (assoc m :current-property-setter-map)))


(defn- add-current-property-getter-name
  [m]
  (->> m
       :current-property-key
       name
       cs/->PascalCase
       (str "get")
       (assoc m :current-property-getter-name)))

(defn- add-current-property-setter-name
  [m]
  (->> m
      :current-property-key
      name
      cs/->PascalCase
      (str "set")
      (assoc m :current-property-setter-name)))

(defn- add-current-property-members
  [m]
  (->> m
       :current-property-member-map
       :parameter-types
       (assoc m :current-property-parameters)))


(defn- call-method*
  [obj m & args]
  (clojure.lang.Reflector/invokeInstanceMethod obj (str m) (into-array args)))

(defn- add-obj-using-generic-type
  [m]
  {:pre [(:current-property-generic-type m)]}
  ;; (debug "Adding object using generic class" (:current-property-generic-type m))
  (assoc m :current-obj (clojure.lang.Reflector/invokeConstructor (if (class? (:current-property-generic-type m))
                                                                    (:current-property-generic-type m)
                                                                    (resolve (symbol (:current-property-generic-type m))))
                                                                  (to-array []))))



(defn- resolve-type-class
  [m]
  {:pre [(:target-map-keyword m) (keyword? (:target-map-keyword m))]}
  (if (get-in m [(:target-map-keyword m) (:type-keyword m)])
    (when-not (class? (get-in m [(:target-map-keyword m) (:type-keyword m)]))
      (update-in m [(:target-map-keyword m) (:type-keyword m)]
                 #(if (:type-mappings m) ;; add type class if available from mappings if not from strign conversion
                    (get (:type-mappings m) %)
                    %)))
    m))

(defn- resolve-type-class-of-current-map
  [m]
  (-> m
      (assoc :target-map-keyword :current-map)
      resolve-type-class
      (dissoc :target-map-keyword)))


(defn- add-into-id-atom!
  [m]
  {:pre [(:current-property-val m) (:current-obj m) (:id-obj-map m)]}
  ;; (debug "Adding into id atom" [(:current-property-val m) (:current-obj m) (:id-obj-map m)])
  (swap! (:id-obj-map m) assoc (keyword (:current-property-val m)) {:current-obj (:current-obj m)}))


(defn- add-into-id-ref-atom!
  [m]
  {:pre [(:current-property-val m) (:current-obj m) (:current-property-setter-map m) (:idref-obj-map m)]}
  (swap! (:idref-obj-map m) update-in
         [(keyword (:current-property-val m))]
         #(if-not %
            [{:current-obj (:current-obj m)
              :current-property-setter-map (:current-property-setter-map m)}]
            (conj % {:current-obj (:current-obj m)
                     :current-property-setter-map (:current-property-setter-map m)}))))


(defn- update-refs-in-id-ref-atom
  "Updates all references pointing this id with the current
  object. Warning namespaces are not supported!!"
  [m]
  {:pre [(:current-property-val m) (:id-obj-map m) (:idref-obj-map m)]}
  ;; (debug "Updating refs " @(:id-obj-map m))
  ;; (debug "Updating refs " (:current-property-val m))
  ;; (debug "Updating refs " @(:idref-obj-map m))
  (->> m
       :current-property-val
       keyword
       (get @(:idref-obj-map m))
       (mapv #(call-method* (:current-obj %)
                            (:name (:current-property-setter-map %))
                            (:current-obj m)))))


(defn- add-id-and-set-references
  [m]

  (add-into-id-atom! m)
  (update-refs-in-id-ref-atom m))

(defn- update-refs-in-id-ref-atom-if-possible
  [m]
  {:pre [(:idref-obj-map m) @(:id-obj-map m) (:current-property-val m)]}
  (if (get @(:id-obj-map m) (keyword (:current-property-val m)))
    (->> m
         :current-property-val
         keyword
         (get @(:id-obj-map m))
         :current-obj
         (assoc m :current-obj)
         update-refs-in-id-ref-atom)))

(defn- add-id-ref-and-set-references
  [m]
  (add-into-id-ref-atom! m)
  (update-refs-in-id-ref-atom-if-possible m))


(declare map-properties->obj-properties)



(defn- add-property-value-into-obj
  [m]
  {:pre [(:target-field-type m)
         (:current-field-val m)
         (:current-obj m)]}
  ;; (debug "Following type will be converted into java obj" (:current-property-key m))
  ;; (debug "Following type will be converted into java obj" m)
  (cond
    ;; another type that needs to be resolved
    (= (:target-field-type m) :complex-type)
    (do
      ;; (debug "It's a recursive field" m (:current-property-val m) (:current-field-val m))
      ;; (debug "type" (type (:target-field-class m)))
      ;; it's a complex type
      (call-method* (:current-obj m)
                    (:name (:current-property-setter-map m))
                    (-> m
                        (assoc :current-obj (:current-field-val m))
                        (assoc :current-map (:current-property-val m))
                        map-properties->obj-properties)))
    ;; it's a simple type
    (= (:target-field-type m) :simple-type)
    (do
      ;; (debug "It's a simple type" m (:current-property-val m) (:current-field-val m))
      ;; it's a simple type
      (call-method* (:current-obj m)
                    (:name (:current-property-setter-map m))
                    (clojure.lang.Reflector/invokeStaticMethod (:target-field-class m) "fromValue" (into-array [(:current-property-val m)]))))

    ;; in case it's a list return it as a list
    (= (:target-field-type m) :list)
    (do
      ;; (debug "It's a list field" (:current-property-val m) (:current-field-val m))
      ;; (debug "It's a list field" (r/reflect (:current-property-val m)))
      (call-method* (:current-field-val m) "addAll"
                    (java.util.ArrayList. (mapv
                                           #(-> m
                                                (assoc :current-map %)
                                                resolve-type-class-of-current-map
                                                add-generic-type-of-instance-method
                                                add-obj-using-generic-type
                                                map-properties->obj-properties)
                                           (:current-property-val m)))))

    (= (:target-field-type m) :map)
    (do
      ;; (debug "It's a map field" (:current-property-val m) (:current-field-val m))
      (call-method* (:current-field-val m) "putAll" (java.util.HashMap. (:current-property-val m))))

    (= (:target-field-type m) :qname)
    (do
      ;; (debug "It's a qname field" (:current-property-val m) (type (:current-property-val m)) (:current-field-val m))

      (call-method* (:current-obj m)
                    (:name (:current-property-setter-map m))
                    (javax.xml.namespace.QName/valueOf (:current-property-val m))))

    (= (:target-field-type m) :jaxb-element)
    (do
      ;; (debug "It's a jaxb field" (:current-property-key m) (:current-property-val m))
      )

    (= (:target-field-type m) :int)
    (do
      ;; (debug "It's a qname field" (:current-property-val m) (type (:current-property-val m)) (:current-field-val m))

      (call-method* (:current-obj m)
                    (:name (:current-property-setter-map m))
                    (int (:current-property-val m))))

    (= (:target-field-type m) :time)
    (do
      ;; (debug "It's a qname field" (:current-property-val m) (type (:current-property-val m)) (:current-field-val m))

      (call-method* (:current-obj m)
                    (:name (:current-property-setter-map m))
                    (:current-field-val m)))


    (= (:target-field-type m) :any)
    (do
      ;; (debug "It's any field" (:current-property-val m) (type (:current-property-val m)) (:current-field-val m))
      (cond
        (instance? java.util.List (:current-field-val m)) ;; any is not singular rather max is *
        (call-method* (:current-field-val m) "addAll"
                    (java.util.ArrayList. (mapv
                                           #(-> m
                                                (assoc :current-map %)
                                                resolve-type-class-of-current-map
                                                add-generic-type-of-instance-method
                                                add-obj-using-generic-type
                                                map-properties->obj-properties)
                                           (:current-property-val m))))))

    (= (:target-field-type m) :xml-id)
    (do
      ;; (debug "It's an XML ID" (:current-property-val m) (type (:current-property-val m)) (:current-field-val m))
      (call-method* (:current-obj m)
                    (:name (:current-property-setter-map m))
                    (:current-property-val m))
      (add-id-and-set-references m))

    (= (:target-field-type m) :xml-id-ref)
    (do
      ;; (debug "It's an ID REF" (:current-property-val m) (type (:current-property-val m)) (:current-field-val m))

      (add-id-ref-and-set-references m))
    (= (:target-field-type m) :other-attributes)
    (do
      ;; (debug "Other attributes" m)
      (call-method* (:current-field-val m)
                    "putAll"
                    (->> m
                         :current-property-val
                         (mapv #(vector (javax.xml.namespace.QName/valueOf (first %))
                                        (second %)))
                         (into {}))))

    :else
    (do
      ;; (debug "Otherwise'" (:current-property-val m) (:current-field-val m))
      (call-method* (:current-obj m)
                    (:name (:current-property-setter-map m))
                    (:current-property-val m)))))

(defn- convert-if-int
  [v]
  (if (= (type v) (type int))
    java.lang.Integer
    v))

(defn- add-target-field-class
  [m]
  {:pre [(or ((:type-keyword m) (:current-property-val m))
             (:current-property-getter-map m))]
   :post [(:target-field-class %)]}
  (->> (or (and (map? (:current-property-val m))
                ((:type-keyword m) (:current-property-val m))
                (symbol ((:type-keyword m) (:current-property-val m))))
           (-> m :current-property-getter-map :return-type))
       eval
       (convert-if-int)
       (assoc m :target-field-class)))


(defn- add-field-obj
  [m]
  {:pre [(:current-obj m)
         (:target-field-class m)]}
  ;; (debug "Adding field object" m)
  ;; (debug "Type of class field " (:target-field-class m) (type (:target-field-class m)))
  (when (:current-property-getter-map m)
    (assoc m :current-field-val
           (cond
             (call-method* (:current-obj m)
                             (:name (:current-property-getter-map m)))
             (call-method* (:current-obj m)
                           (:name (:current-property-getter-map m)))
             ;;
             (= javax.xml.namespace.QName (:target-field-class m))
             (javax.xml.namespace.QName/valueOf (:current-property-val m))

             (= javax.xml.datatype.XMLGregorianCalendar (:target-field-class m))
             (cond
               (string? (:current-property-val m))
               (.newXMLGregorianCalendar (javax.xml.datatype.DatatypeFactory/newInstance))
               (instance? javax.xml.datatype.XMLGregorianCalendar (:current-property-val m))
               (:current-property-val m))
             ;;
             :else
             (clojure.lang.Reflector/invokeConstructor (:target-field-class m) (to-array []))))))



(defn- resolve-type-class-of-property-val
  [m]
  ;; (debug ((:type-keyword m) (:current-property-val m)) (:current-property-getter-map m))
  (-> m
      (assoc :target-map-keyword :current-property-val)
      resolve-type-class
      (dissoc :target-map-keyword)))


(defn- add-compatible-properties
  [m]
  {:pre [(:current-property-key m) (:current-property-val m)]}
  ;; (debug "Adding maps for the property" m)
  (when-not (= (:current-property-key m) (:type-keyword m))
    (-> m
        add-current-property-setter-name
        add-current-property-getter-name
        add-propperty-getter-member-map
        add-propperty-setter-member-map
        resolve-type-class-of-property-val
        add-target-field-class
        add-field-obj
        add-field-type
        remove-nils-from-current-map
        remove-empty-values-from-current-map
        add-property-value-into-obj)))



(defn- map-properties->obj-properties
  [m]
  (->> m
       remove-nils-from-current-map
       remove-empty-values-from-current-map
       :current-map
       (mapv
        #(when-not (= (first %) (:type-keyword m))
           (-> m
               resolve-type-class-of-current-map
               (assoc :current-property-key (cs/->camelCaseKeyword (first %)))
               (assoc :current-property-val (if (keyword? (second %))
                                              (str (second %))
                                              (second %)))
               add-compatible-properties))))
  (:current-obj m))

(defn map->jaxb-type-obj
  "Adds map fields into corresponding jaxb object. During this
  process, each key in the map is converted into a PascalCase and
  using this string getters and setters from the object is
  taken. Therefore, key names must match the object members when they
  are converted to PascalCase from kebap-case. Type map is a clojure
  map. Type object is an instance of a JAX-B type. During
  transformation each map can contain a property for creating new
  instances of these types. The key of this type is specified
  in :type-keyword of the addional map. If not specified, the default
  is ::type-class in this map. An additional :type-mappings map can be
  passed. The type-mappings will be used to map the given keywords
  in (:type-keyword m) into class types, i.e., jaxb class names. Class
  names are specified with namespaces and are strings."
  [type-map jaxb-type-obj & m]
  ;; (debug "map->jaxb-type-obj" type-map jaxb-type-obj m)
  (-> {:current-map type-map
       :current-obj jaxb-type-obj
       :type-keyword (or (:type-keyword (first m)) ::type-class)
       :type-mappings (:type-mappings (first m))
       :id-obj-map (atom {})
       :idref-obj-map (atom {})}
      map-properties->obj-properties))
