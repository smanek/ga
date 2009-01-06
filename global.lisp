(in-package :ga)

(defparameter *population-size* 1) ;;the total population size

(defparameter *bred-per-gen* 0) ;;number of offspring per generation
(defparameter *mutated-per-gen* 1) ;;number of mutated per generation
(defparameter *new-per-gen* 0)

(defparameter *mutation-rate* 0.1) ;;the probability that a particular polygon of an individual is mutated
(defparameter *remove-vertex-rate* 0.1) ;;if a polygon is mutated, the chance it has a vertex removed
(defparameter *insert-vertex-rate* 0.1) ;;if a polygon is mutated, the chance it has a vertex added
(defparameter *poly-shift-rate* 0.1) ;;if a polygon is mutated, the chance it is shifted
(defparameter *poly-scale-rate* 0.1) ;;if a polygon is mutated, the chance it is scaled in size
(defparameter *color-mutate-rate* 0.1) ;;if a polygon is mutated, the chance its color is changed

;;the following three are indepenent of the mutation rate
(defparameter *insertion-rate* 0.1) ;;the probability that an individual has a random polygon inserted
(defparameter *deletion-rate* 0.1)  ;;the probability that an individual has a random polygon deleted
(defparameter *shuffle-rate* 0.1) ;;the probability that one polygon is moved

(defparameter *crossover-rate* 0.5) ;;the amount of DNA a child takes from the first parent

(defparameter *min-polygons* 1)
(defparameter *max-polygons* 50)
(defparameter *max-polygon-vertices* 10)
(defparameter *min-initial-polygons* 1)
(defparameter *max-initial-polygons* 1)

(defparameter *output-directory* "/home/smanek/temp/results/")
(defparameter *output-type* ".bmp")

(defparameter *debug-output* 7) ;;the higher the number, the more verbose the debugging info. Do not exceed 7, unless you want to drink from the hose.
