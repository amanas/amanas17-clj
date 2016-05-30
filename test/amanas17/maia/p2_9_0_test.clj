(ns amanas17.maia.p2-9-0-test
  (:use
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-9-0]))


(assert (and (=  0 (cmp-concepto-TC '(0 (*)) '(0 (*))))
             (= -1 (cmp-concepto-TC '(0 ())  '(0 (*))))
             (=  1 (cmp-concepto-TC '(0 (*)) '(0 ())))
             (=  0 (cmp-concepto-TC '(1 (*)) '(1 (*))))
             (= -1 (cmp-concepto-TC '(1 (*)) '(0 (*))))
             (=  1 (cmp-concepto-TC '(0 (*)) '(1 (*))))
             (=  0 (cmp-concepto-TC '(0 ())  '(1 (*))))
             (=  0 (cmp-concepto-TC '(0 ())  '(1 (a))))
             (= -1 (cmp-concepto-TC '(0 ())  '(0 (a))))
             (=  0 (cmp-concepto-TC '(0 (b)) '(0 (a))))))
