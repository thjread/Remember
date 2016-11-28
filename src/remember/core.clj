(ns remember.core
  (:gen-class)
  (:require [clj-time.core :as t])
  (:require [clj-time.coerce :as c])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.edn :as edn])
  (:require [clojure.tools.cli :as cli])
  (:require [clojure.java.io :as io]))

(defrecord Memory
    [question answer type-in lastdate timer karma])

(defn make-memory [question answer type-in]
  (atom (map->Memory {:question question :answer answer :type-in type-in :lastdate (t/now) :timer 0 :karma 0})))

(defn memory-to-printable [memory]
  (let [map (into {} @memory)]
    (update map :lastdate c/to-date)))

(defn read-to-memory [read]
  (atom (map->Memory (update read :lastdate c/from-date))))

(def memories (atom []))
;;(def test-memories (atom (map atom [(map->Memory {:question "canis"
;;                                             :answer "dog"
;;                                             :type-in true
;;                                             :lastdate (t/minus (t/now) (t/days 2))
;;                                             :timer 1
;;                                             :karma 0})
;;                               (map->Memory {:question "servus"
;;                                             :answer "slave"
;;                                             :type-in true
;;                                             :lastdate (t/minus (t/now) (t/days 2))
;;                                             :timer 1
;;                                             :karma 0})])))

(defn add-memory [memory]
  (swap! memories #(conj % memory)))

(defn delete-memory [question]
  (swap! memories (partial remove #(= question (:question (deref %))))))

(defn karma-time [karma]
  (let [time      (* 24 (math/expt 2 (dec karma)))
        variation (/ time 4)]
    (/ (+ time (- (rand-int (* 2 variation)) variation)) 24)))

(defn correct-answer [memory]
  (letfn [(correct [mem]
         (let [karma (inc (:karma mem))
               timer (karma-time karma)
               lastdate (t/now)]
           (merge mem
               {:karma karma
               :timer timer
               :lastdate lastdate})))]
    (swap! memory correct)))

(defn incorrect-answer [memory]
  (letfn [(incorrect [mem]
         (let [karma (max -3 (dec (:karma mem)))]
           (assoc mem :karma karma)))]
    (swap! memory incorrect)))

(defn days-since-test [memory]
  (/ (t/in-hours (t/interval (:lastdate @memory) (t/now))) 24))

(defn time-till-test [memory]
  (- (:timer @memory) (days-since-test memory)))

(defn needs-test? [memory]
  (<= (time-till-test memory) 0))

(defn quit [code]
  (System/exit code))

(def default-file (str (System/getenv "HOME") "/.remember"))

(defn load-memories [file]
  (when (not (.exists file))
    (println "File not found.")
    (quit 1))
  (reset! memories (map read-to-memory (edn/read-string (slurp file)))))

(defn save-memories [file]
  (spit file (prn-str (map memory-to-printable @memories))))

(defn test-state [questions]
  {:ask (->> questions
             (map (fn [x] [x true]))
             (shuffle)
             (into []))
   :correct [] :prev nil
   :prev-state nil :all-correct true})

(defn amend [test-state]
  (reset! (:prev test-state) (:prev-state test-state)))

(defn ask-question [update memory]
  (letfn [(do-question [memory]
            (println (:question @memory))
            (print "> ")
            (flush)
            (let [input (read-line)]
              (case input
                "undo" 'undo
                "quit" 'quit
                (if (= input (:answer @memory))
                       'correct 'incorrect))))
          (do-show-question [memory]
            (println (:question @memory))
            (print "Show? ")
            (flush)
            (read-line)
            (println (:answer @memory))
            (print "Correct? (Y/n) ")
            (flush)
            (let [input (read-line)]
              (case input
                "undo" 'undo
                "quit" 'quit
                "" 'correct "Y" 'correct "y" 'correct
                "yes" 'correct "Yes" 'correct
                'incorrect)))]
    (if (nil? memory)
      'quit
      (let [type-in (:type-in @memory)
            result (if type-in
                     (do-question memory)
                     (do-show-question memory))]
        (case result
          correct (do (when type-in (println "Correct!"))
                      (when update (correct-answer memory))
                      'correct)
          incorrect (do (when type-in (println "Wrong -" (:answer @memory)))
                        (when update (incorrect-answer memory))
                        'incorrect)
          result)))))

(defn ask-next [all-correct test-state]
  (let [questions      (:ask test-state)
        correct        (:correct test-state)
        [first update] (first questions)
        rest           (into [] (rest questions))]
    (if (and all-correct (not (seq questions)) (not (:all-correct test-state)))
      (ask-next true (merge test-state {:ask correct
                                        :correct []
                                        :all-correct true}))
      (let [prev-state (if first @first nil)
            result     (ask-question update first)]
        (case result
          correct (merge test-state {:ask rest
                                     :correct (conj correct [first false])
                                     :prev first
                                     :prev-state prev-state})
          incorrect (merge test-state {:ask (conj rest [first false])
                                       :prev first
                                       :prev-state prev-state
                                       :all-correct false})
          result)))))

(defn do-test [all-correct all]
  (let [questions (if all @memories
                      (filter needs-test? @memories))]
    (if (not (seq questions))
      (println "No memories to test!")
      (loop [test-state (test-state questions)
             prev-state nil]
        (let [result (ask-next all-correct test-state)]
          (case result
            undo (do (if (nil? prev-state)
                       (recur test-state nil)
                       (do (amend test-state)
                           (recur prev-state nil))))
            quit nil
            (recur result test-state)))))))

(defn print-memory [mem]
  (letfn [(format-time [t]
            (if (<= t 0) "now"
                (str "in " (if (< t 2)
                              (str (math/floor (* t 24)) " hours")
                              (str (math/floor t) " days")))))]
    (str (:question @mem) " -> " (:answer @mem)
         " (next test " (let [time (- (:timer @mem) (days-since-test mem))]
                         (format-time time))
         ")")))

(defn list-memories []
  (if (not (seq @memories))
    (println "No memories.")
    (doseq [s (concat ["Memories:"
                       ""]
                      (map print-memory
                           (sort-by time-till-test @memories)))]
      (println s))))

(defn path-exists? [file]
  (let [parent (.getParent file)]
    (or (nil? parent) (.isDirectory (io/file parent)))))

(def cli-options [["-h" "--help" "Print this help"
                   :default false]
                  ["-c" "--all-correct" "Repeat questions until all answered correctly in a row"
                   :default false]
                  ["-a" "--all" "Test on all questions"
                   :default false]
                  ["-s" "--show" "Show answer rather than type in"
                   :default false]
                  ["-f" "--file FILE" "Specify save file"
                   :default (io/file default-file)
                   :validate [path-exists?]
                   :parse-fn io/file]])

(defn usage [options-summary]
  (->> ["Usage: remember [options] [action]"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  QUESTION ANSWER     Add a new memory"
        "  delete QUESTION         Delete a memory"
        "  list                    List memories"
        ""
        "If no action is given the program will test difficult memories."
        "Type quit to exit, and undo to undo the previous answer"]
       (clojure.string/join \newline)))

(defn -main [& args]
  (let [{:keys [options arguments summary errors]} (cli/parse-opts args cli-options)]
    (cond (:help options) (println (usage summary))
          errors (println (clojure.string/join \newline errors))
          :else
          (let [file (:file options)]
            (case (first arguments)
              nil      (do (load-memories file)
                           (do-test (:all-correct options) (:all options)))
              "delete" (do (load-memories file)
                           (delete-memory (second arguments)))
              "list"   (do (load-memories file)
                           (list-memories))
              (add-memory (do (when (.exists file)
                                (load-memories file))
                              (make-memory (first arguments) (second arguments)
                                           (not (:show options))))))
            (save-memories file)))))

