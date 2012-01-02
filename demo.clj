(ns user
  (:use (com.sbrian.mathrender arctan-rendering))
  (:import (com.sbrian.mathrender.arctan SimplifiedRenderer)))

(differentiate-n-to-template 10 (SimplifiedRenderer.))

