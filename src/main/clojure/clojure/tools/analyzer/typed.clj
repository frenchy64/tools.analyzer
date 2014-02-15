(ns clojure.tools.analyzer.typed
  "Annotations for tools.analyzer"
  (:require [clojure.core.typed :as t]))

(t/def-alias NsMap
  (HMap :mandatory
        {:mappings (t/Map t/Symbol Any) ;contradicts docs, which say (Map Symbol Var). What about Classes?
         :aliases (t/Map t/Symbol t/Symbol)
         :ns t/Symbol}))

(t/def-alias Env
  (HMap :mandatory
        {:locals (t/Map t/Symbol Expr)
         :context (U ':return
                     ':statement
                     ':expr)
         :ns t/Symbol
         :namespaces (t/Atom1 (t/Map t/Symbol NsMap))}))

(t/def-alias Form Any)

(t/def-alias Children (t/Vec Any))

(t/def-alias Expr
  (Rec [Expr]
  ; ----- start tools.analyzer specific ----
  (U (HMap :mandatory
           {:op ':const
            :env Env
            :type t/Keyword
            :literal Boolean
            :val Form
            :form Form}
           :optional
           {:meta Expr
            :children Children})
     (HMap :mandatory
           {:op ':vector
            :env Env
            :items (t/Vec Expr)
            :form Form
            :children Children})
     (HMap :mandatory
           {:op ':with-meta
            :env Env
            :form Form
            :meta Expr
            :expr Expr
            :children Children})
     (HMap :mandatory
           {:op ':map
            :env Env
            :keys (t/Vec Expr)
            :vals (t/Vec Expr)
            :form Form
            :children Children})
     (HMap :mandatory
           {:op ':set
            :env Env
            :items (t/Vec Expr)
            :form Form
            :children Children})
     (HMap :mandatory
           {:op ':local
            :form Form
            :assignable? Boolean
            :children Children}
           :optional
           ;env is sometimes omitted. eg. :local entry in :fn-method
           {:env Env})
     (HMap :mandatory
           {:op ':var
            :env Env
            :form Form
            :assignable? Boolean
            :var t/Symbol})
     (HMap :mandatory
           {:op ':maybe-host-form
            :env Env
            :form Form
            :class (U nil t/Symbol)
            :field t/Symbol})
     (HMap :mandatory
           {:op ':maybe-class
            :env Env
            :form Form
            ; is this just Symbol?
            :class (U nil t/Symbol)})
     (HMap :mandatory
           {:op ':do
            :env Env
            :form Form
            :statements (t/Vec Expr)
            :ret Expr
            :children Children})
     (HMap :mandatory
           {:op ':if
            :env Env
            :form Form
            :test Expr
            :then Expr
            :else Expr
            :children Children})
     (HMap :mandatory
           {:op ':new
            :env Env
            :form Form
            :class t/Symbol
            :args (t/Vec Expr)
            :children Children})
     (HMap :mandatory
           {:op ':quote
            :env Env
            :form Form
            :expr Expr
            :literal? Boolean
            :children Children})
     (HMap :mandatory
           {:op ':set!
            :env Env
            :form Form
            :target Expr
            :val Expr
            :children Children})
     (HMap :mandatory
           {:op ':try
            :env Env
            :form Form
            :body Expr
            :catches (t/Vec Expr)
            :children Children}
           :optional
           {:finally Expr})
     (HMap :mandatory
           {:op ':binding
            :env Env
            :form Form
            :name t/Symbol
            :local Any}
           :optional
           {:tag (U nil t/Symbol)
            :arg-id t/Int
            :variadic? Boolean})
     (HMap :mandatory
           {:op ':catch
            :env Env
            :form Form
            :class t/Symbol
            :local Expr
            :body Expr
            :children Children})
     (HMap :mandatory
           {:op ':throw
            :env Env
            :form Form
            :exception Expr})
     (HMap :mandatory
           {:op ':letfn
            :env Env
            :form Form
            :bindings (t/Vec Expr)
            :children Children})
     (HMap :mandatory
           {:op ':let
            :form Form
            :env Env
            :body Expr
            :bindings (t/Vec Expr)
            :children Children})
     (HMap :mandatory
           {:op ':loop
            :form Form
            :env Env
            :loop-id t/Symbol
            :body Expr
            :bindings (t/Vec Expr)
            :children Children})
     (HMap :mandatory
           {:op ':recur
            :env Env
            :form Form
            :exprs (t/Vec Expr)
            :loop-id t/Symbol
            :children Children})
     (HMap :mandatory
           {:op ':fn-method
            :env Env
            :form Form
            :loop-id t/Symbol
            :variadic? Boolean
            :params (t/Vec Expr)
            :fixed-arity t/Int
            :body Expr
            :children Children}
           :optional
           {:local Expr})
     (HMap :mandatory
           {:op ':fn
            :env Env
            :form Form
            ;unsure if nilable
            :name (U nil t/Symbol)
            :variadic? Boolean
            :max-fixed-arity (U nil t/Int)
            :methods (t/Vec Expr)
            :children Children}
           :optional
           {:local Expr})
     (HMap :mandatory
           {:op ':def
            :env Env
            :form Form
            :name t/Symbol
            ;what is the result of create-var?
            :var Any}
           :optional
           {:meta Expr
            :init Expr
            :doc String
            :children Children})
     (HMap :mandatory
           {:op ':host-call
            :env Env
            :form Form
            :target Expr
            :method t/Symbol
            :args (t/Vec Expr)
            :children Children})
     (HMap :mandatory
           {:op ':host-field
            :env Env
            :form Form
            :target Expr
            :field t/Symbol
            :children Children})
     (HMap :mandatory
           {:op ':host-interop
            :env Env
            :form Form
            :target Expr
            :m-or-f t/Symbol
            :children Children})
     (HMap :mandatory
           {:op ':invoke
            :env Env
            :form Form
            :fn Expr
            :args (t/Vec Expr)
            :children Children}
           :optional
           {:meta Expr})
; ---- end tools.analyzer specific -----
           

)))
              
    

