(in-package :ga)

;;the directory where the program should write its results to.
;;(concatenate 'string (sb-ext:posix-getenv "HOME") "/") tells it to output to ~/
;;make sure you remember to add a trailing slash if you change it
;(defparameter *output-directory* (concatenate 'string (sb-ext:posix-getenv "HOME") "/")) 
(defparameter *output-directory* "/home/smanek/temp/results/") 

(defparameter *population-size* 10) ;;the total population size
(defparameter *bred-per-gen* 5) ;;number of new children that should be bred per generation
(defparameter *mutated-per-gen* 5) ;;number of new mutated individuals that should be created per generation
(defparameter *new-per-gen* 0) ;;number of random individuals created per generation

(defparameter *mutation-rate* 0.1) ;;the probability that a particular polygon of an individual is mutated
(defparameter *remove-vertex-rate* 0.2) ;;if a polygon is mutated, the chance it has a vertex removed
(defparameter *insert-vertex-rate* 0.2) ;;if a polygon is mutated, the chance it has a vertex added
(defparameter *poly-shift-rate* 0.1) ;;if a polygon is mutated, the chance it is shifted
(defparameter *poly-scale-rate* 0.05) ;;if a polygon is mutated, the chance it is scaled in size
(defparameter *color-mutate-rate* 0.1) ;;if a polygon is mutated, the chance its color is changed

;;the following three are indepenent of the mutation rate
(defparameter *insertion-rate* 0.1) ;;the probability that an individual has a random polygon inserted
(defparameter *deletion-rate* 0.1)  ;;the probability that an individual has a random polygon deleted
(defparameter *shuffle-rate* 0.1) ;;the probability that one polygon is moved

(defparameter *crossover-rate* 0.8) ;;the amount of DNA a child takes from the first parent

(defparameter *min-polygons* 1) ;;the absolute minimum number of polygons an individual can have. 
(defparameter *max-polygons* 50) ;;the absolute max number of polygons. The more polygons, the more CPU the program requires
(defparameter *max-polygon-vertices* 10) ;;the min. number of vertices is (obviously) 3. The more vertices, the more CPU
(defparameter *min-initial-polygons* 1) ;;when an individual is randomly created, the min number of polygons
(defparameter *max-initial-polygons* 50) ;;when an individual is randomly created, the max number of polygons

(defparameter *output-type* ".bmp")

(defparameter *debug-output* 7) ;;the higher the number, the more verbose the debugging info. Do not exceed 7, unless you want to drink from the hose.
