(ns examples.buffer.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [put! chan alts!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom {:items [{:text "cat"}
                 {:text "dog"}
                 {:text "bird"}]}))

(def app-history (atom [@app-state]))

(defn items []
  (om/ref-cursor (:items (om/root-cursor app-state))))

(defn aview [{:keys [text]} owner]
  (reify
    om/IRender
    (render [_]
      (let [xs (om/observe owner (items))]
        (dom/div nil
          (dom/h2 nil text)
          (apply dom/ul nil
            (map #(dom/li nil (:text %)) xs)))))))

(defn main-view [_ owner]
  (reify
    om/IRender
    (render [_]
      (let [xs (items)]
        (dom/div nil
          (om/build aview {:text "View A"})
          (om/build aview {:text "View B"})
          (dom/button
            #js {:onClick
                 (fn [e] (om/transact! xs #(assoc % 1 {:text "zebra"})))}
            "Switch To Zebra!")
          (dom/button
            #js {:onClick
                 (fn [e] (om/transact! xs #(assoc % 1 {:text "dog"})))}
            "Switch to Dog!"))))))

(defn root [empty owner]
  (reify
    om/IWillMount
  (will-mount [_]
        (let [tx-chan (om/get-shared owner :tx-chan)
              txs (chan)]
          (async/sub tx-chan :txs txs)
          (om/set-state! owner :txs txs)
          (go (loop []
                (let [m (<! txs)]
                  (println "A transaction was logged")
                  (recur))))))
    om/IRender
    (render [_]
      (dom/div nil
        (om/build main-view {})
        (dom/div #js {:id "message"} nil)
        (dom/button
          #js {:onClick
               (fn [e]
                 (when (> (count @app-history) 1)
                   (swap! app-history pop)
                   (reset! app-state (last @app-history))))}
          "Undo")))))

(let [tx-chan (chan)
      tx-pub-chan (async/pub tx-chan (fn [_] :txs))]
  (om/root root app-state
           {:target (.getElementById js/document "app")
            :shared {:tx-chan tx-pub-chan}
            :tx-listen
            (fn [tx-data root-cursor]
              (put! tx-chan [tx-data root-cursor]))}))

(defn pluralize [n s]
  (if (== n 1)
    s
    (str s "s")))

(add-watch app-state :history
  (fn [_ _ _ n]
    (when-not (= (last @app-history) n)
      (swap! app-history conj n))
    (set! (.-innerHTML (.getElementById js/document "message"))
      (let [c (count @app-history)]
        (str c " Saved " (pluralize c "State"))))))
