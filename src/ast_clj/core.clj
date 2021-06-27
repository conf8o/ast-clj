(ns ast-clj.core)

(defprotocol AST (red [this env]))

(defrecord Val [raw]
  AST (red [this _] this)
  Object (toString [{x :raw}] (str x)))

(defrecord Id [raw]
  AST (red [{id :raw} env] (@env id))
  Object (toString [{id :raw}] (name id)))

(defmacro defope
  [ope-name raw-ope]
  `(defrecord ~ope-name [left# right#]
     AST
     (red [{:keys [left# right#]} env#]
       (Val. (~raw-ope (:raw (red left# env#))
                       (:raw (red right# env#)))))

     Object
     (toString [{:keys [left# right#]}]
       (str "(" left# " " (name (quote ~raw-ope)) " " right# ")"))))
(defope Add +)
(defope Mul *)
(defope Lt <)

(defrecord Assign [id v]
  AST
  (red [{:keys [id v]} env]
    (swap! env assoc (:raw id) (red v env)))

  Object
  (toString [{:keys [id v]}]
    (str id " = " v)))

(defrecord If [p t f]
  AST
  (red [{:keys [p t f]} env]
    (if (:raw (red p env))
      (red t env)
      (red f env)))

  Object
  (toString [{:keys [p t f]}]
    (str "if (" p ") { " t " } else { " f " }")))

(defrecord Pass []
  AST (red [_ _] nil)
  Object (toString [_] "pass"))

(let [env (atom {})
      state
      (If. (Lt. (Val. 500) (Val. 200))
           (Assign. (Id. :x) (Val. 200))
           (Pass.))]
  (print (str state))
  (red state env))