(ns tma-generate-pages.tma-programmer-designer-contract
  (:require
   [tma-generate-pages.secretary :as secretary]
   [me.raynes.fs :as fs]
   [clojure.pprint :as pp]
   [clojure.string :as st]
   [dire.core :refer [with-handler supervise]]
   [net.cgrand.enlive-html :as enlive]))




;; 2013-12-13 - in admin.views we establish a system whereby any HTML node that has a CSS class
;; of "tma-programmer-designer-contract" will also have an ID that is a function that should be
;; called. All of those functions live here in this namespace.


(defn transform-keyword-into-string [some-keyword]
  {:pre [(= (type some-keyword) clojure.lang.Keyword)]
   :post [(= (type %) java.lang.String)]}
  (let [some-keyword (st/replace (str some-keyword) #":" "")
        some-keyword (st/replace (str some-keyword) #"-" " ")]
      some-keyword))

  
(defn tma-all-user-info [template request]
  {:pre [
         (vector? template)
         (map? request)
         ]
   :post [(vector? %)]}
  (enlive/transform template [:p.tma-paginated-list]
                    (enlive/clone-for [item (secretary/get-user-info-for-search)]
                                      [:p.tma-paginated-list :a]
                                      (enlive/content (:username item))
                                      [:p.tma-paginated-list :a]
                                      (enlive/set-attr :href (str "/admin/edit/users/" (:item-name item)))
                                      [:p.tma-paginated-list :span.user-email]
                                      (enlive/content (:email item))
                                      [:p.tma-paginated-list :span.user-public-name]
                                      (enlive/content (str (:first-name item) " " (:last-name item))))))

(defn tma-bottom-links-list [template request]
  {:pre [
         (vector? template)
         (map? request)
         ]
   :post [(vector? %)]}
  (enlive/transform template [:#list-items :li]
                    (enlive/clone-for [one-type-as-string (secretary/get-schema-types)]
                                      [:li :a]
                                      (enlive/content one-type-as-string)
                                      [:li :a]
                                      (enlive/set-attr :href (str "/admin/list/" one-type-as-string)))))

(defn tma-bottom-links-edit [template request]
  {:pre [
         (vector? template)
         (map? request)
         ]
   :post [(vector? %)]}  
  (enlive/transform template [:#add-new :li]  
                    (enlive/clone-for [one-type-as-string (secretary/get-schema-types)]
                                      [:li :a]
                                      (enlive/content (str "New " one-type-as-string))
                                      [:li :a]
                                      (enlive/set-attr :href (str "/admin/edit/" one-type-as-string)))))

(defn tma-link-to-show-page [template request]
  {:pre [
         (vector? template)
         (map? request)
         ]
   :post [(vector? %)]}  
  (enlive/transform template [:#tma-link-to-show-page :a]  
                    (enlive/set-attr :href (str "/admin/show/" (get-in request [:params :item-name])))))

(defn tma-link-to-edit-page [template request]
  {:pre [
         (vector? template)
         (map? request)
         ]
   :post [(vector? %)]}
  (let [item (first (secretary/fetch "get-current-item" request))]
    (enlive/transform template [:#tma-link-to-edit-page :a]  
                      (enlive/set-attr :href (str "/admin/edit/" (:item-type item) "/" (:item-name item))))))

(defn tma-show-form-for-editing [template request]
  {:pre [
         (vector? template)
         (map? request)
         ]
   :post [(vector? %)]}  
  (enlive/transform template [:#tma-show-form-for-editing] (enlive/html-content (secretary/get-form request))))

(defn tma-children-item-links [template request]
  {:pre [
         (vector? template)
         (map? request)
         ]
   :post [(vector? %)]}  
  (enlive/transform template [:#tma-children-item-links :.tma-paginated-list]  
                    (enlive/clone-for [item (secretary/fetch "get-children-items" request)]
                                      [:.tma-paginated-list :a]
                                      (enlive/html-content (create-string-with-this-items-important-fields item))
                                      [:.tma-paginated-list :a]
                                      (enlive/set-attr :href (str "/admin/show/" (:item-name item))))))

(defn tma-parent-item-links [template request]
  {:pre [
         (vector? template)
         (map? request)
         ]
   :post [(vector? %)]}  
  (enlive/transform template [:#tma-parent-item-links :.tma-paginated-list]
                    (enlive/clone-for [item (secretary/fetch "get-parent-items" request)]
                                      [:.tma-paginated-list :a]
                                      (enlive/html-content (create-string-with-this-items-important-fields item))
                                      [:.tma-paginated-list :a]
                                      (enlive/set-attr :href (str "/admin/show/" (:item-name item))))))

(defn tma-show-all-possible-fields-for-this-item  [template request]
  {:pre [
         (vector? template)
         (map? request)
         ]
   :post [(vector? %)]}
  (let [item (first (secretary/fetch "get-current-item" request))]
    (pp/pprint item)
    (enlive/transform template [:#tma-show-all-possible-fields-for-this-item :.all-possible-fields]  
                      (enlive/clone-for [field-as-keyword (keys item)]
                                        [:h3]
                                        (enlive/content (secretary/transform-keyword-into-string field-as-keyword))
                                        [:p]
                                        (enlive/content (str (field-as-keyword item)))))))

(defn tma-paginate-items [template request]
  {:pre [
         (vector? template)
         (map? request)
         ]
   :post [(vector? %)]}
  (let [results-to-paginate (secretary/fetch "paginate-results" request)]
    (if results-to-paginate
      (do 
        (enlive/transform template [:#tma-paginate-items :.tma-paginated-list]  
                          (enlive/clone-for [item results-to-paginate]
                                            [:.tma-paginated-list :a]
                                            (enlive/html-content (create-string-with-this-items-important-fields item))
                                            [:.tma-paginated-list :a]
                                            (enlive/set-attr :href (str "/admin/show/" (:item-name item))))))
      template)))

(defn tma-show-count-of-pages-to-paginate [template request]
  {:pre [
         (vector? template)
         (map? request)
         ]
   :post [(vector? %)]}
  (let [vector-holding-one-number (secretary/fetch "get-count" request)
        seq-of-seqs-of-numbers (partition 100 (range (first vector-holding-one-number)))]
    (enlive/transform template [:#tma-show-count-of-pages-to-paginate :a]
                      (enlive/clone-for [n seq-of-seqs-of-numbers]
                                        [:a.link-for-pagination]
                                        (enlive/content (str (first n)))
                                        [:a.link-for-pagination]
                                        (enlive/set-attr :href (str "/admin/list/" (get-in request [:params :item-type]) "/" (str (first n))))))))

(defn tma-span-to-show-item-type  [template request]
  {:pre [
         (vector? template)
         (map? request)
         ]
   :post [(vector? %)]}
  (enlive/transform template [:#tma-span-to-show-item-type] (enlive/content (get-in request [:params :item-type]))))







