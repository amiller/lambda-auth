open Typedtree

class virtual omap =
  object ((self : 'self))

    method virtual ref :
      'a1. ('self -> 'a1 -> ('self * 'a1)) -> 'a1 ref -> ('self * ('a1 ref))
    method virtual option :
      'a1.
        ('self -> 'a1 -> ('self * 'a1)) ->
          'a1 option -> ('self * ('a1 option))
    method virtual list :
      'a1.
        ('self -> 'a1 -> ('self * 'a1)) -> 'a1 list -> ('self * ('a1 list))
    method pattern : pattern -> ('self * pattern) =
      fun __value ->
        let (self, pat_desc) = self#pattern_desc __value.pat_desc in
        let pat_loc = __value.pat_loc in
        let (self, pat_extra) =
          self#list
            (fun self (((__x1, __x2) as __value)) ->
               let (self, __y1) = self#pat_extra __x1 in
               let __y2 = __x2
               in
                 (self,
                  (if (__x2 == __y2) && (__x1 == __y1)
                   then __value
                   else (__y1, __y2))))
            __value.pat_extra in
        let pat_type = __value.pat_type in
        let pat_env = __value.pat_env
        in
          (self,
           (if
              (__value.pat_loc == pat_loc) &&
                ((__value.pat_extra == pat_extra) &&
                   ((__value.pat_type == pat_type) &&
                      ((__value.pat_env == pat_env) &&
                         (__value.pat_desc == pat_desc))))
            then __value
            else
              {
                pat_desc = pat_desc;
                pat_loc = pat_loc;
                pat_extra = pat_extra;
                pat_type = pat_type;
                pat_env = pat_env;
              }))
    method pat_extra : pat_extra -> ('self * pat_extra) =
      fun __value ->
        match __value with
        | Tpat_constraint __x1 ->
            let (self, __y1) = self#core_type __x1
            in
              (self,
               (if __x1 == __y1 then __value else Tpat_constraint __y1))
        | Tpat_type (__x1, __x2) -> (self, __value)
        | Tpat_unpack -> (self, __value)
    method pattern_desc : pattern_desc -> ('self * pattern_desc) =
      fun __value ->
        match __value with
        | Tpat_any -> (self, __value)
        | Tpat_var (__x1, __x2) -> (self, __value)
        | Tpat_alias (__x1, __x2, __x3) ->
            let (self, __y1) = self#pattern __x1 in
            let __y2 = __x2 in
            let __y3 = __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tpat_alias (__y1, __y2, __y3)))
        | Tpat_constant __x1 -> (self, __value)
        | Tpat_tuple __x1 ->
            let (self, __y1) = self#list (fun self -> self#pattern) __x1
            in (self, (if __x1 == __y1 then __value else Tpat_tuple __y1))
        | Tpat_construct (__x2, __x3, __x4, __x5) ->
            let __y2 = __x2 in
            let __y3 = __x3 in
            let (self, __y4) = self#list (fun self -> self#pattern) __x4 in
            let __y5 = __x5
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) &&
                       ((__x4 == __y4) && ((__x5 == __y5))))
                then __value
                else Tpat_construct (__y2, __y3, __y4, __y5)))
        | Tpat_variant (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let (self, __y2) = self#option (fun self -> self#pattern) __x2 in
            let (self, __y3) = self#ref (fun self v -> (self, v)) __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tpat_variant (__y1, __y2, __y3)))
        | Tpat_record (__x1, __x2) ->
            let (self, __y1) =
              self#list
                (fun self (((__x2, __x3, __x4) as __value)) ->
                   let __y2 = __x2 in
                   let __y3 = __x3 in
                   let (self, __y4) = self#pattern __x4
                   in
                     (self,
                      (if
                         (__x2 == __y2) &&
                           ((__x3 == __y3) &&
                              ((__x4 == __y4)))
                       then __value
                       else (__y2, __y3, __y4))))
                __x1 in
            let __y2 = __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Tpat_record (__y1, __y2)))
        | Tpat_array __x1 ->
            let (self, __y1) = self#list (fun self -> self#pattern) __x1
            in (self, (if __x1 == __y1 then __value else Tpat_array __y1))
        | Tpat_or (__x1, __x2, __x3) ->
            let (self, __y1) = self#pattern __x1 in
            let (self, __y2) = self#pattern __x2 in
            let (self, __y3) = self#option (fun self v -> (self, v)) __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tpat_or (__y1, __y2, __y3)))
        | Tpat_lazy __x1 ->
            let (self, __y1) = self#pattern __x1
            in (self, (if __x1 == __y1 then __value else Tpat_lazy __y1))
    method expression : expression -> ('self * expression) =
      fun __value ->
        let (self, exp_desc) = self#expression_desc __value.exp_desc in
        let exp_loc = __value.exp_loc in
        let (self, exp_extra) =
          self#list
            (fun self (((__x1, __x2) as __value)) ->
               let (self, __y1) = self#exp_extra __x1 in
               let __y2 = __x2
               in
                 (self,
                  (if (__x2 == __y2) && (__x1 == __y1)
                   then __value
                   else (__y1, __y2))))
            __value.exp_extra in
        let exp_type = __value.exp_type in
        let exp_env = __value.exp_env
        in
          (self,
           (if
              (__value.exp_loc == exp_loc) &&
                ((__value.exp_extra == exp_extra) &&
                   ((__value.exp_type == exp_type) &&
                      ((__value.exp_env == exp_env) &&
                         (__value.exp_desc == exp_desc))))
            then __value
            else
              {
                exp_desc = exp_desc;
                exp_loc = exp_loc;
                exp_extra = exp_extra;
                exp_type = exp_type;
                exp_env = exp_env;
              }))
    method exp_extra : exp_extra -> ('self * exp_extra) =
      fun __value ->
        match __value with
        | Texp_constraint (__x1, __x2) ->
            let (self, __y1) =
              self#option (fun self -> self#core_type) __x1 in
            let (self, __y2) = self#option (fun self -> self#core_type) __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Texp_constraint (__y1, __y2)))
        | Texp_open (__x0, __x1, __x2, __x3) -> (self, __value)
        | Texp_poly __x1 ->
            let (self, __y1) = self#option (fun self -> self#core_type) __x1
            in (self, (if __x1 == __y1 then __value else Texp_poly __y1))
        | Texp_newtype __x1 -> (self, __value)
    method expression_desc : expression_desc -> ('self * expression_desc) =
      fun __value ->
        match __value with
        | Texp_ident (__x1, __x2, __x3) -> (self, __value)
        | Texp_constant __x1 -> (self, __value)
        | Texp_let (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let (self, __y2) =
              self#list
                (fun self (((__x1, __x2) as __value)) ->
                   let (self, __y1) = self#pattern __x1 in
                   let (self, __y2) = self#expression __x2
                   in
                     (self,
                      (if (__x2 == __y2) && (__x1 == __y1)
                       then __value
                       else (__y1, __y2))))
                __x2 in
            let (self, __y3) = self#expression __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Texp_let (__y1, __y2, __y3)))
        | Texp_function (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let (self, __y2) =
              self#list
                (fun self (((__x1, __x2) as __value)) ->
                   let (self, __y1) = self#pattern __x1 in
                   let (self, __y2) = self#expression __x2
                   in
                     (self,
                      (if (__x2 == __y2) && (__x1 == __y1)
                       then __value
                       else (__y1, __y2))))
                __x2 in
            let __y3 = __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Texp_function (__y1, __y2, __y3)))
        | Texp_apply (__x1, __x2) ->
            let (self, __y1) = self#expression __x1 in
            let (self, __y2) =
              self#list
                (fun self (((__x1, __x2, __x3) as __value)) ->
                   let __y1 = __x1 in
                   let (self, __y2) =
                     self#option (fun self -> self#expression) __x2 in
                   let __y3 = __x3
                   in
                     (self,
                      (if
                         (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                       then __value
                       else (__y1, __y2, __y3))))
                __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Texp_apply (__y1, __y2)))
        | Texp_match (__x1, __x2, __x3) ->
            let (self, __y1) = self#expression __x1 in
            let (self, __y2) =
              self#list
                (fun self (((__x1, __x2) as __value)) ->
                   let (self, __y1) = self#pattern __x1 in
                   let (self, __y2) = self#expression __x2
                   in
                     (self,
                      (if (__x2 == __y2) && (__x1 == __y1)
                       then __value
                       else (__y1, __y2))))
                __x2 in
            let __y3 = __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Texp_match (__y1, __y2, __y3)))
        | Texp_try (__x1, __x2) ->
            let (self, __y1) = self#expression __x1 in
            let (self, __y2) =
              self#list
                (fun self (((__x1, __x2) as __value)) ->
                   let (self, __y1) = self#pattern __x1 in
                   let (self, __y2) = self#expression __x2
                   in
                     (self,
                      (if (__x2 == __y2) && (__x1 == __y1)
                       then __value
                       else (__y1, __y2))))
                __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Texp_try (__y1, __y2)))
        | Texp_tuple __x1 ->
            let (self, __y1) = self#list (fun self -> self#expression) __x1
            in (self, (if __x1 == __y1 then __value else Texp_tuple __y1))
        | Texp_construct (__x2, __x3, __x4, __x5) ->
            let __y2 = __x2 in
            let __y3 = __x3 in
            let (self, __y4) =
              self#list (fun self -> self#expression) __x4 in
            let __y5 = __x5
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) &&
                       ((__x4 == __y4) && ((__x5 == __y5))))
                then __value
                else Texp_construct (__y2, __y3, __y4, __y5)))
        | Texp_variant (__x1, __x2) ->
            let __y1 = __x1 in
            let (self, __y2) = self#option (fun self -> self#expression) __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Texp_variant (__y1, __y2)))
        | Texp_record (__x1, __x2) ->
            let (self, __y1) =
              self#list
                (fun self (((__x2, __x3, __x4) as __value)) ->
                   let __y1 = __x1 in
                   let __y2 = __x2 in
                   let __y3 = __x3 in
                   let (self, __y4) = self#expression __x4
                   in
                     (self,
                      (if
                         (__x2 == __y2) &&
                           ((__x3 == __y3) &&
                              ((__x4 == __y4)))
                       then __value
                       else (__y2, __y3, __y4))))
                __x1 in
            let (self, __y2) = self#option (fun self -> self#expression) __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Texp_record (__y1, __y2)))
        | Texp_field (__x1, __x3, __x4) ->
            let (self, __y1) = self#expression __x1 in
            let __y3 = __x3 in
            let __y4 = __x4
            in
              (self,
               (if
                    ((__x3 == __y3) && ((__x4 == __y4) && (__x1 == __y1)))
                then __value
                else Texp_field (__y1, __y3, __y4)))
        | Texp_setfield (__x1, __x3, __x4, __x5) ->
            let (self, __y1) = self#expression __x1 in
            let __y3 = __x3 in
            let __y4 = __x4 in
            let (self, __y5) = self#expression __x5
            in
              (self,
               (if
                    ((__x3 == __y3) &&
                       ((__x4 == __y4) && ((__x5 == __y5) && (__x1 == __y1))))
                then __value
                else Texp_setfield (__y1, __y3, __y4, __y5)))
        | Texp_array __x1 ->
            let (self, __y1) = self#list (fun self -> self#expression) __x1
            in (self, (if __x1 == __y1 then __value else Texp_array __y1))
        | Texp_ifthenelse (__x1, __x2, __x3) ->
            let (self, __y1) = self#expression __x1 in
            let (self, __y2) = self#expression __x2 in
            let (self, __y3) = self#option (fun self -> self#expression) __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Texp_ifthenelse (__y1, __y2, __y3)))
        | Texp_sequence (__x1, __x2) ->
            let (self, __y1) = self#expression __x1 in
            let (self, __y2) = self#expression __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Texp_sequence (__y1, __y2)))
        | Texp_while (__x1, __x2) ->
            let (self, __y1) = self#expression __x1 in
            let (self, __y2) = self#expression __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Texp_while (__y1, __y2)))
        | Texp_for (__x1, __x2, __x3, __x4, __x5, __x6) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#expression __x3 in
            let (self, __y4) = self#expression __x4 in
            let __y5 = __x5 in
            let (self, __y6) = self#expression __x6
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) &&
                       ((__x4 == __y4) &&
                          ((__x5 == __y5) &&
                             ((__x6 == __y6) && (__x1 == __y1)))))
                then __value
                else Texp_for (__y1, __y2, __y3, __y4, __y5, __y6)))
        | Texp_when (__x1, __x2) ->
            let (self, __y1) = self#expression __x1 in
            let (self, __y2) = self#expression __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Texp_when (__y1, __y2)))
        | Texp_send (__x1, __x2, __x3) ->
            let (self, __y1) = self#expression __x1 in
            let (self, __y2) = self#meth __x2 in
            let (self, __y3) = self#option (fun self -> self#expression) __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Texp_send (__y1, __y2, __y3)))
        | Texp_new (__x1, __x2, __x3) -> (self, __value)
        | Texp_instvar (__x1, __x2, __x3) -> (self, __value)
        | Texp_setinstvar (__x1, __x2, __x3, __x4) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let __y3 = __x3 in
            let (self, __y4) = self#expression __x4
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) && ((__x4 == __y4) && (__x1 == __y1)))
                then __value
                else Texp_setinstvar (__y1, __y2, __y3, __y4)))
        | Texp_override (__x1, __x2) ->
            let __y1 = __x1 in
            let (self, __y2) =
              self#list
                (fun self (((__x1, __x2, __x3) as __value)) ->
                   let __y1 = __x1 in
                   let __y2 = __x2 in
                   let (self, __y3) = self#expression __x3
                   in
                     (self,
                      (if
                         (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                       then __value
                       else (__y1, __y2, __y3))))
                __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Texp_override (__y1, __y2)))
        | Texp_letmodule (__x1, __x2, __x3, __x4) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#module_expr __x3 in
            let (self, __y4) = self#expression __x4
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) && ((__x4 == __y4) && (__x1 == __y1)))
                then __value
                else Texp_letmodule (__y1, __y2, __y3, __y4)))
        | Texp_assert __x1 ->
            let (self, __y1) = self#expression __x1
            in (self, (if __x1 == __y1 then __value else Texp_assert __y1))
        | Texp_assertfalse -> (self, __value)
        | Texp_lazy __x1 ->
            let (self, __y1) = self#expression __x1
            in (self, (if __x1 == __y1 then __value else Texp_lazy __y1))
        | Texp_object (__x1, __x2) ->
            let (self, __y1) = self#class_structure __x1 in
            let (self, __y2) = self#list (fun self v -> (self, v)) __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Texp_object (__y1, __y2)))
        | Texp_pack __x1 ->
            let (self, __y1) = self#module_expr __x1
            in (self, (if __x1 == __y1 then __value else Texp_pack __y1))
    method meth : meth -> ('self * meth) = fun __value -> (self, __value)
    method class_expr : class_expr -> ('self * class_expr) =
      fun __value ->
        let (self, cl_desc) = self#class_expr_desc __value.cl_desc in
        let cl_loc = __value.cl_loc in
        let cl_type = __value.cl_type in
        let cl_env = __value.cl_env
        in
          (self,
           (if
              (__value.cl_loc == cl_loc) &&
                ((__value.cl_type == cl_type) &&
                   ((__value.cl_env == cl_env) &&
                      (__value.cl_desc == cl_desc)))
            then __value
            else
              {
                cl_desc = cl_desc;
                cl_loc = cl_loc;
                cl_type = cl_type;
                cl_env = cl_env;
              }))
    method class_expr_desc : class_expr_desc -> ('self * class_expr_desc) =
      fun __value ->
        match __value with
        | Tcl_ident (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#list (fun self -> self#core_type) __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tcl_ident (__y1, __y2, __y3)))
        | Tcl_structure __x1 ->
            let (self, __y1) = self#class_structure __x1
            in (self, (if __x1 == __y1 then __value else Tcl_structure __y1))
        | Tcl_fun (__x1, __x2, __x3, __x4, __x5) ->
            let __y1 = __x1 in
            let (self, __y2) = self#pattern __x2 in
            let (self, __y3) =
              self#list
                (fun self (((__x1, __x2, __x3) as __value)) ->
                   let __y1 = __x1 in
                   let __y2 = __x2 in
                   let (self, __y3) = self#expression __x3
                   in
                     (self,
                      (if
                         (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                       then __value
                       else (__y1, __y2, __y3))))
                __x3 in
            let (self, __y4) = self#class_expr __x4 in
            let __y5 = __x5
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) &&
                       ((__x4 == __y4) && ((__x5 == __y5) && (__x1 == __y1))))
                then __value
                else Tcl_fun (__y1, __y2, __y3, __y4, __y5)))
        | Tcl_apply (__x1, __x2) ->
            let (self, __y1) = self#class_expr __x1 in
            let (self, __y2) =
              self#list
                (fun self (((__x1, __x2, __x3) as __value)) ->
                   let __y1 = __x1 in
                   let (self, __y2) =
                     self#option (fun self -> self#expression) __x2 in
                   let __y3 = __x3
                   in
                     (self,
                      (if
                         (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                       then __value
                       else (__y1, __y2, __y3))))
                __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Tcl_apply (__y1, __y2)))
        | Tcl_let (__x1, __x2, __x3, __x4) ->
            let __y1 = __x1 in
            let (self, __y2) =
              self#list
                (fun self (((__x1, __x2) as __value)) ->
                   let (self, __y1) = self#pattern __x1 in
                   let (self, __y2) = self#expression __x2
                   in
                     (self,
                      (if (__x2 == __y2) && (__x1 == __y1)
                       then __value
                       else (__y1, __y2))))
                __x2 in
            let (self, __y3) =
              self#list
                (fun self (((__x1, __x2, __x3) as __value)) ->
                   let __y1 = __x1 in
                   let __y2 = __x2 in
                   let (self, __y3) = self#expression __x3
                   in
                     (self,
                      (if
                         (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                       then __value
                       else (__y1, __y2, __y3))))
                __x3 in
            let (self, __y4) = self#class_expr __x4
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) && ((__x4 == __y4) && (__x1 == __y1)))
                then __value
                else Tcl_let (__y1, __y2, __y3, __y4)))
        | Tcl_constraint (__x1, __x2, __x3, __x4, __x5) ->
            let (self, __y1) = self#class_expr __x1 in
            let (self, __y2) =
              self#option (fun self -> self#class_type) __x2 in
            let (self, __y3) = self#list (fun self v -> (self, v)) __x3 in
            let (self, __y4) = self#list (fun self v -> (self, v)) __x4 in
            let __y5 = __x5
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) &&
                       ((__x4 == __y4) && ((__x5 == __y5) && (__x1 == __y1))))
                then __value
                else Tcl_constraint (__y1, __y2, __y3, __y4, __y5)))
    method class_structure : class_structure -> ('self * class_structure) =
      fun __value ->
        let (self, cstr_pat) = self#pattern __value.cstr_pat in
        let (self, cstr_fields) =
          self#list (fun self -> self#class_field) __value.cstr_fields in
        let cstr_type = __value.cstr_type in
        let cstr_meths = __value.cstr_meths
        in
          (self,
           (if
              (__value.cstr_fields == cstr_fields) &&
                ((__value.cstr_type == cstr_type) &&
                   ((__value.cstr_meths == cstr_meths) &&
                      (__value.cstr_pat == cstr_pat)))
            then __value
            else
              {
                cstr_pat = cstr_pat;
                cstr_fields = cstr_fields;
                cstr_type = cstr_type;
                cstr_meths = cstr_meths;
              }))
    method class_field : class_field -> ('self * class_field) =
      fun __value ->
        let (self, cf_desc) = self#class_field_desc __value.cf_desc in
        let cf_loc = __value.cf_loc
        in
          (self,
           (if (__value.cf_loc == cf_loc) && (__value.cf_desc == cf_desc)
            then __value
            else { cf_desc = cf_desc; cf_loc = cf_loc; }))
    method class_field_kind :
      class_field_kind -> ('self * class_field_kind) =
      fun __value ->
        match __value with
        | Tcfk_virtual __x1 ->
            let (self, __y1) = self#core_type __x1
            in (self, (if __x1 == __y1 then __value else Tcfk_virtual __y1))
        | Tcfk_concrete __x1 ->
            let (self, __y1) = self#expression __x1
            in (self, (if __x1 == __y1 then __value else Tcfk_concrete __y1))
    method class_field_desc :
      class_field_desc -> ('self * class_field_desc) =
      fun __value ->
        match __value with
        | Tcf_inher (__x1, __x2, __x3, __x4, __x5) ->
            let __y1 = __x1 in
            let (self, __y2) = self#class_expr __x2 in
            let (self, __y3) = self#option (fun self v -> (self, v)) __x3 in
            let (self, __y4) =
              self#list
                (fun self (((__x1, __x2) as __value)) -> (self, __value))
                __x4 in
            let (self, __y5) =
              self#list
                (fun self (((__x1, __x2) as __value)) -> (self, __value))
                __x5
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) &&
                       ((__x4 == __y4) && ((__x5 == __y5) && (__x1 == __y1))))
                then __value
                else Tcf_inher (__y1, __y2, __y3, __y4, __y5)))
        | Tcf_val (__x1, __x2, __x3, __x4, __x5, __x6) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let __y3 = __x3 in
            let __y4 = __x4 in
            let (self, __y5) = self#class_field_kind __x5 in
            let __y6 = __x6
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) &&
                       ((__x4 == __y4) &&
                          ((__x5 == __y5) &&
                             ((__x6 == __y6) && (__x1 == __y1)))))
                then __value
                else Tcf_val (__y1, __y2, __y3, __y4, __y5, __y6)))
        | Tcf_meth (__x1, __x2, __x3, __x4, __x5) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let __y3 = __x3 in
            let (self, __y4) = self#class_field_kind __x4 in
            let __y5 = __x5
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) &&
                       ((__x4 == __y4) && ((__x5 == __y5) && (__x1 == __y1))))
                then __value
                else Tcf_meth (__y1, __y2, __y3, __y4, __y5)))
        | Tcf_constr (__x1, __x2) ->
            let (self, __y1) = self#core_type __x1 in
            let (self, __y2) = self#core_type __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Tcf_constr (__y1, __y2)))
        | Tcf_init __x1 ->
            let (self, __y1) = self#expression __x1
            in (self, (if __x1 == __y1 then __value else Tcf_init __y1))
    method module_expr : module_expr -> ('self * module_expr) =
      fun __value ->
        let (self, mod_desc) = self#module_expr_desc __value.mod_desc in
        let mod_loc = __value.mod_loc in
        let mod_type = __value.mod_type in
        let mod_env = __value.mod_env
        in
          (self,
           (if
              (__value.mod_loc == mod_loc) &&
                ((__value.mod_type == mod_type) &&
                   ((__value.mod_env == mod_env) &&
                      (__value.mod_desc == mod_desc)))
            then __value
            else
              {
                mod_desc = mod_desc;
                mod_loc = mod_loc;
                mod_type = mod_type;
                mod_env = mod_env;
              }))
    method module_type_constraint :
      module_type_constraint -> ('self * module_type_constraint) =
      fun __value ->
        match __value with
        | Tmodtype_implicit -> (self, __value)
        | Tmodtype_explicit __x1 ->
            let (self, __y1) = self#module_type __x1
            in
              (self,
               (if __x1 == __y1 then __value else Tmodtype_explicit __y1))
    method module_expr_desc :
      module_expr_desc -> ('self * module_expr_desc) =
      fun __value ->
        match __value with
        | Tmod_ident (__x1, __x2) -> (self, __value)
        | Tmod_structure __x1 ->
            let (self, __y1) = self#structure __x1
            in
              (self, (if __x1 == __y1 then __value else Tmod_structure __y1))
        | Tmod_functor (__x1, __x2, __x3, __x4) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#module_type __x3 in
            let (self, __y4) = self#module_expr __x4
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) && ((__x4 == __y4) && (__x1 == __y1)))
                then __value
                else Tmod_functor (__y1, __y2, __y3, __y4)))
        | Tmod_apply (__x1, __x2, __x3) ->
            let (self, __y1) = self#module_expr __x1 in
            let (self, __y2) = self#module_expr __x2 in
            let (self, __y3) = self#module_coercion __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tmod_apply (__y1, __y2, __y3)))
        | Tmod_constraint (__x1, __x2, __x3, __x4) ->
            let (self, __y1) = self#module_expr __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#module_type_constraint __x3 in
            let (self, __y4) = self#module_coercion __x4
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) && ((__x4 == __y4) && (__x1 == __y1)))
                then __value
                else Tmod_constraint (__y1, __y2, __y3, __y4)))
        | Tmod_unpack (__x1, __x2) ->
            let (self, __y1) = self#expression __x1 in
            let __y2 = __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Tmod_unpack (__y1, __y2)))
    method structure : structure -> ('self * structure) =
      fun __value ->
        let (self, str_items) =
          self#list (fun self -> self#structure_item) __value.str_items in
        let str_type = __value.str_type in
        let str_final_env = __value.str_final_env
        in
          (self,
           (if
              (__value.str_type == str_type) &&
                ((__value.str_final_env == str_final_env) &&
                   (__value.str_items == str_items))
            then __value
            else 
              {
                str_items = str_items;
                str_type = str_type;
                str_final_env = str_final_env;
              }))
    method structure_item : structure_item -> ('self * structure_item) =
      fun __value ->
        let (self, str_desc) = self#structure_item_desc __value.str_desc in
        let str_loc = __value.str_loc in
        let str_env = __value.str_env
        in
          (self,
           (if
              (__value.str_loc == str_loc) &&
                ((__value.str_env == str_env) &&
                   (__value.str_desc == str_desc))
            then __value
            else
              { str_desc = str_desc; str_loc = str_loc; str_env = str_env; }))
    method structure_item_desc :
      structure_item_desc -> ('self * structure_item_desc) =
      fun __value ->
        match __value with
        | Tstr_eval __x1 ->
            let (self, __y1) = self#expression __x1
            in (self, (if __x1 == __y1 then __value else Tstr_eval __y1))
        | Tstr_value (__x1, __x2) ->
            let __y1 = __x1 in
            let (self, __y2) =
              self#list
                (fun self (((__x1, __x2) as __value)) ->
                   let (self, __y1) = self#pattern __x1 in
                   let (self, __y2) = self#expression __x2
                   in
                     (self,
                      (if (__x2 == __y2) && (__x1 == __y1)
                       then __value
                       else (__y1, __y2))))
                __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Tstr_value (__y1, __y2)))
        | Tstr_primitive (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#value_description __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tstr_primitive (__y1, __y2, __y3)))
        | Tstr_type __x1 ->
            let (self, __y1) =
              self#list
                (fun self (((__x1, __x2, __x3) as __value)) ->
                   let __y1 = __x1 in
                   let __y2 = __x2 in
                   let (self, __y3) = self#type_declaration __x3
                   in
                     (self,
                      (if
                         (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                       then __value
                       else (__y1, __y2, __y3))))
                __x1
            in (self, (if __x1 == __y1 then __value else Tstr_type __y1))
        | Tstr_exception (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#exception_declaration __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tstr_exception (__y1, __y2, __y3)))
        | Tstr_exn_rebind (__x1, __x2, __x3, __x4) -> (self, __value)
        | Tstr_module (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#module_expr __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tstr_module (__y1, __y2, __y3)))
        | Tstr_recmodule __x1 ->
            let (self, __y1) =
              self#list
                (fun self (((__x1, __x2, __x3, __x4) as __value)) ->
                   let __y1 = __x1 in
                   let __y2 = __x2 in
                   let (self, __y3) = self#module_type __x3 in
                   let (self, __y4) = self#module_expr __x4
                   in
                     (self,
                      (if
                         (__x2 == __y2) &&
                           ((__x3 == __y3) &&
                              ((__x4 == __y4) && (__x1 == __y1)))
                       then __value
                       else (__y1, __y2, __y3, __y4))))
                __x1
            in
              (self, (if __x1 == __y1 then __value else Tstr_recmodule __y1))
        | Tstr_modtype (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#module_type __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tstr_modtype (__y1, __y2, __y3)))
        | Tstr_open (__x0, __x1, __x2) -> (self, __value)
        | Tstr_class __x1 ->
            let (self, __y1) =
              self#list
                (fun self (((__x1, __x2, __x3) as __value)) ->
                   let (self, __y1) = self#class_declaration __x1 in
                   let (self, __y2) =
                     self#list (fun self v -> (self, v)) __x2 in
                   let __y3 = __x3
                   in
                     (self,
                      (if
                         (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                       then __value
                       else (__y1, __y2, __y3))))
                __x1
            in (self, (if __x1 == __y1 then __value else Tstr_class __y1))
        | Tstr_class_type __x1 ->
            let (self, __y1) =
              self#list
                (fun self (((__x1, __x2, __x3) as __value)) ->
                   let __y1 = __x1 in
                   let __y2 = __x2 in
                   let (self, __y3) = self#class_type_declaration __x3
                   in
                     (self,
                      (if
                         (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                       then __value
                       else (__y1, __y2, __y3))))
                __x1
            in
              (self,
               (if __x1 == __y1 then __value else Tstr_class_type __y1))
        | Tstr_include (__x1, __x2) ->
            let (self, __y1) = self#module_expr __x1 in
            let (self, __y2) = self#list (fun self v -> (self, v)) __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Tstr_include (__y1, __y2)))
    method module_coercion : module_coercion -> ('self * module_coercion) =
      fun __value ->
        match __value with
        | Tcoerce_none -> (self, __value)
        | Tcoerce_structure __x1 ->
            let (self, __y1) =
              self#list
                (fun self (((__x1, __x2) as __value)) ->
                   let __y1 = __x1 in
                   let (self, __y2) = self#module_coercion __x2
                   in
                     (self,
                      (if (__x2 == __y2) && (__x1 == __y1)
                       then __value
                       else (__y1, __y2))))
                __x1
            in
              (self,
               (if __x1 == __y1 then __value else Tcoerce_structure __y1))
        | Tcoerce_functor (__x1, __x2) ->
            let (self, __y1) = self#module_coercion __x1 in
            let (self, __y2) = self#module_coercion __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Tcoerce_functor (__y1, __y2)))
        | Tcoerce_primitive __x1 -> (self, __value)
    method module_type : module_type -> ('self * module_type) =
      fun __value ->
        let (self, mty_desc) = self#module_type_desc __value.mty_desc in
        let mty_type = __value.mty_type in
        let mty_env = __value.mty_env in
        let mty_loc = __value.mty_loc
        in
          (self,
           (if
              (__value.mty_type == mty_type) &&
                ((__value.mty_env == mty_env) &&
                   ((__value.mty_loc == mty_loc) &&
                      (__value.mty_desc == mty_desc)))
            then __value
            else
              {
                mty_desc = mty_desc;
                mty_type = mty_type;
                mty_env = mty_env;
                mty_loc = mty_loc;
              }))
    method module_type_desc :
      module_type_desc -> ('self * module_type_desc) =
      fun __value ->
        match __value with
        | Tmty_ident (__x1, __x2) -> (self, __value)
        | Tmty_signature __x1 ->
            let (self, __y1) = self#signature __x1
            in
              (self, (if __x1 == __y1 then __value else Tmty_signature __y1))
        | Tmty_functor (__x1, __x2, __x3, __x4) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#module_type __x3 in
            let (self, __y4) = self#module_type __x4
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) && ((__x4 == __y4) && (__x1 == __y1)))
                then __value
                else Tmty_functor (__y1, __y2, __y3, __y4)))
        | Tmty_with (__x1, __x2) ->
            let (self, __y1) = self#module_type __x1 in
            let (self, __y2) =
              self#list
                (fun self (((__x1, __x2, __x3) as __value)) ->
                   let __y1 = __x1 in
                   let __y2 = __x2 in
                   let (self, __y3) = self#with_constraint __x3
                   in
                     (self,
                      (if
                         (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                       then __value
                       else (__y1, __y2, __y3))))
                __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Tmty_with (__y1, __y2)))
        | Tmty_typeof __x1 ->
            let (self, __y1) = self#module_expr __x1
            in (self, (if __x1 == __y1 then __value else Tmty_typeof __y1))
    method signature : signature -> ('self * signature) =
      fun __value ->
        let (self, sig_items) =
          self#list (fun self -> self#signature_item) __value.sig_items in
        let sig_type = __value.sig_type in
        let sig_final_env = __value.sig_final_env
        in
          (self,
           (if
              (__value.sig_type == sig_type) &&
                ((__value.sig_final_env == sig_final_env) &&
                   (__value.sig_items == sig_items))
            then __value
            else
              {
                sig_items = sig_items;
                sig_type = sig_type;
                sig_final_env = sig_final_env;
              }))
    method signature_item : signature_item -> ('self * signature_item) =
      fun __value ->
        let (self, sig_desc) = self#signature_item_desc __value.sig_desc in
        let sig_env = __value.sig_env in
        let sig_loc = __value.sig_loc
        in
          (self,
           (if
              (__value.sig_env == sig_env) &&
                ((__value.sig_loc == sig_loc) &&
                   (__value.sig_desc == sig_desc))
            then __value
            else
              { sig_desc = sig_desc; sig_env = sig_env; sig_loc = sig_loc; }))
    method signature_item_desc :
      signature_item_desc -> ('self * signature_item_desc) =
      fun __value ->
        match __value with
        | Tsig_value (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#value_description __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tsig_value (__y1, __y2, __y3)))
        | Tsig_type __x1 ->
            let (self, __y1) =
              self#list
                (fun self (((__x1, __x2, __x3) as __value)) ->
                   let __y1 = __x1 in
                   let __y2 = __x2 in
                   let (self, __y3) = self#type_declaration __x3
                   in
                     (self,
                      (if
                         (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                       then __value
                       else (__y1, __y2, __y3))))
                __x1
            in (self, (if __x1 == __y1 then __value else Tsig_type __y1))
        | Tsig_exception (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#exception_declaration __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tsig_exception (__y1, __y2, __y3)))
        | Tsig_module (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#module_type __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tsig_module (__y1, __y2, __y3)))
        | Tsig_recmodule __x1 ->
            let (self, __y1) =
              self#list
                (fun self (((__x1, __x2, __x3) as __value)) ->
                   let __y1 = __x1 in
                   let __y2 = __x2 in
                   let (self, __y3) = self#module_type __x3
                   in
                     (self,
                      (if
                         (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                       then __value
                       else (__y1, __y2, __y3))))
                __x1
            in
              (self, (if __x1 == __y1 then __value else Tsig_recmodule __y1))
        | Tsig_modtype (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#modtype_declaration __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tsig_modtype (__y1, __y2, __y3)))
        | Tsig_open (__x0, __x1, __x2) -> (self, __value)
        | Tsig_include (__x1, __x2) ->
            let (self, __y1) = self#module_type __x1 in
            let __y2 = __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Tsig_include (__y1, __y2)))
        | Tsig_class __x1 ->
            let (self, __y1) =
              self#list (fun self -> self#class_description) __x1
            in (self, (if __x1 == __y1 then __value else Tsig_class __y1))
        | Tsig_class_type __x1 ->
            let (self, __y1) =
              self#list (fun self -> self#class_type_declaration) __x1
            in
              (self,
               (if __x1 == __y1 then __value else Tsig_class_type __y1))
    method modtype_declaration :
      modtype_declaration -> ('self * modtype_declaration) =
      fun __value ->
        match __value with
        | Tmodtype_abstract -> (self, __value)
        | Tmodtype_manifest __x1 ->
            let (self, __y1) = self#module_type __x1
            in
              (self,
               (if __x1 == __y1 then __value else Tmodtype_manifest __y1))
    method with_constraint : with_constraint -> ('self * with_constraint) =
      fun __value ->
        match __value with
        | Twith_type __x1 ->
            let (self, __y1) = self#type_declaration __x1
            in (self, (if __x1 == __y1 then __value else Twith_type __y1))
        | Twith_module (__x1, __x2) -> (self, __value)
        | Twith_typesubst __x1 ->
            let (self, __y1) = self#type_declaration __x1
            in
              (self,
               (if __x1 == __y1 then __value else Twith_typesubst __y1))
        | Twith_modsubst (__x1, __x2) -> (self, __value)
    method core_type : core_type -> ('self * core_type) =
      fun __value ->
        let (self, ctyp_desc) = self#core_type_desc __value.ctyp_desc in
        let ctyp_type = __value.ctyp_type in
        let ctyp_env = __value.ctyp_env in
        let ctyp_loc = __value.ctyp_loc
        in
          (self,
           (if
              (__value.ctyp_type == ctyp_type) &&
                ((__value.ctyp_env == ctyp_env) &&
                   ((__value.ctyp_loc == ctyp_loc) &&
                      (__value.ctyp_desc == ctyp_desc)))
            then __value
            else
              {
                ctyp_desc = ctyp_desc;
                ctyp_type = ctyp_type;
                ctyp_env = ctyp_env;
                ctyp_loc = ctyp_loc;
              }))
    method core_type_desc : core_type_desc -> ('self * core_type_desc) =
      fun __value ->
        match __value with
        | Ttyp_any -> (self, __value)
        | Ttyp_var __x1 -> (self, __value)
        | Ttyp_arrow (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let (self, __y2) = self#core_type __x2 in
            let (self, __y3) = self#core_type __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Ttyp_arrow (__y1, __y2, __y3)))
        | Ttyp_tuple __x1 ->
            let (self, __y1) = self#list (fun self -> self#core_type) __x1
            in (self, (if __x1 == __y1 then __value else Ttyp_tuple __y1))
        | Ttyp_constr (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#list (fun self -> self#core_type) __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Ttyp_constr (__y1, __y2, __y3)))
        | Ttyp_object __x1 ->
            let (self, __y1) =
              self#list (fun self -> self#core_field_type) __x1
            in (self, (if __x1 == __y1 then __value else Ttyp_object __y1))
        | Ttyp_class (__x1, __x2, __x3, __x4) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#list (fun self -> self#core_type) __x3 in
            let (self, __y4) = self#list (fun self v -> (self, v)) __x4
            in
              (self,
               (if
                  (__x2 == __y2) &&
                    ((__x3 == __y3) && ((__x4 == __y4) && (__x1 == __y1)))
                then __value
                else Ttyp_class (__y1, __y2, __y3, __y4)))
        | Ttyp_alias (__x1, __x2) ->
            let (self, __y1) = self#core_type __x1 in
            let __y2 = __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Ttyp_alias (__y1, __y2)))
        | Ttyp_variant (__x1, __x2, __x3) ->
            let (self, __y1) = self#list (fun self -> self#row_field) __x1 in
            let __y2 = __x2 in
            let (self, __y3) =
              self#option (fun self -> self#list (fun self v -> (self, v)))
                __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Ttyp_variant (__y1, __y2, __y3)))
        | Ttyp_poly (__x1, __x2) ->
            let (self, __y1) = self#list (fun self v -> (self, v)) __x1 in
            let (self, __y2) = self#core_type __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Ttyp_poly (__y1, __y2)))
        | Ttyp_package __x1 ->
            let (self, __y1) = self#package_type __x1
            in (self, (if __x1 == __y1 then __value else Ttyp_package __y1))
    method package_type : package_type -> ('self * package_type) =
      fun __value ->
        let pack_name = __value.pack_name in
        let (self, pack_fields) =
          self#list
            (fun self (((__x1, __x2) as __value)) ->
               let __y1 = __x1 in
               let (self, __y2) = self#core_type __x2
               in
                 (self,
                  (if (__x2 == __y2) && (__x1 == __y1)
                   then __value
                   else (__y1, __y2))))
            __value.pack_fields in
        let pack_type = __value.pack_type in
        let pack_txt = __value.pack_txt
        in
          (self,
           (if
              (__value.pack_fields == pack_fields) &&
                ((__value.pack_type == pack_type) &&
                   ((__value.pack_txt == pack_txt) &&
                      (__value.pack_name == pack_name)))
            then __value
            else
              {
                pack_name = pack_name;
                pack_fields = pack_fields;
                pack_type = pack_type;
                pack_txt = pack_txt;
              }))
    method core_field_type : core_field_type -> ('self * core_field_type) =
      fun __value ->
        let (self, field_desc) = self#core_field_desc __value.field_desc in
        let field_loc = __value.field_loc
        in
          (self,
           (if
              (__value.field_loc == field_loc) &&
                (__value.field_desc == field_desc)
            then __value
            else { field_desc = field_desc; field_loc = field_loc; }))
    method core_field_desc : core_field_desc -> ('self * core_field_desc) =
      fun __value ->
        match __value with
        | Tcfield (__x1, __x2) ->
            let __y1 = __x1 in
            let (self, __y2) = self#core_type __x2
            in
              (self,
               (if (__x2 == __y2) && (__x1 == __y1)
                then __value
                else Tcfield (__y1, __y2)))
        | Tcfield_var -> (self, __value)
    method row_field : row_field -> ('self * row_field) =
      fun __value ->
        match __value with
        | Ttag (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#list (fun self -> self#core_type) __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Ttag (__y1, __y2, __y3)))
        | Tinherit __x1 ->
            let (self, __y1) = self#core_type __x1
            in (self, (if __x1 == __y1 then __value else Tinherit __y1))
    method value_description :
      value_description -> ('self * value_description) =
      fun __value ->
        let (self, val_desc) = self#core_type __value.val_desc in
        let val_val = __value.val_val in
        let (self, val_prim) =
          self#list (fun self v -> (self, v)) __value.val_prim in
        let val_loc = __value.val_loc
        in
          (self,
           (if
              (__value.val_val == val_val) &&
                ((__value.val_prim == val_prim) &&
                   ((__value.val_loc == val_loc) &&
                      (__value.val_desc == val_desc)))
            then __value
            else
              {
                val_desc = val_desc;
                val_val = val_val;
                val_prim = val_prim;
                val_loc = val_loc;
              }))
    method type_declaration :
      type_declaration -> ('self * type_declaration) =
      fun __value ->
        let (self, typ_params) =
          self#list (fun self -> self#option (fun self v -> (self, v)))
            __value.typ_params in
        let typ_type = __value.typ_type in
        let (self, typ_cstrs) =
          self#list
            (fun self (((__x1, __x2, __x3) as __value)) ->
               let (self, __y1) = self#core_type __x1 in
               let (self, __y2) = self#core_type __x2 in
               let __y3 = __x3
               in
                 (self,
                  (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                   then __value
                   else (__y1, __y2, __y3))))
            __value.typ_cstrs in
        let (self, typ_kind) = self#type_kind __value.typ_kind in
        let typ_private = __value.typ_private in
        let (self, typ_manifest) =
          self#option (fun self -> self#core_type) __value.typ_manifest in
        let (self, typ_variance) =
          self#list (fun self (((__x1, __x2) as __value)) -> (self, __value))
            __value.typ_variance in
        let typ_loc = __value.typ_loc
        in
          (self,
           (if
              (__value.typ_type == typ_type) &&
                ((__value.typ_cstrs == typ_cstrs) &&
                   ((__value.typ_kind == typ_kind) &&
                      ((__value.typ_private == typ_private) &&
                         ((__value.typ_manifest == typ_manifest) &&
                            ((__value.typ_variance == typ_variance) &&
                               ((__value.typ_loc == typ_loc) &&
                                  (__value.typ_params == typ_params)))))))
            then __value
            else
              {
                typ_params = typ_params;
                typ_type = typ_type;
                typ_cstrs = typ_cstrs;
                typ_kind = typ_kind;
                typ_private = typ_private;
                typ_manifest = typ_manifest;
                typ_variance = typ_variance;
                typ_loc = typ_loc;
              }))
    method type_kind : type_kind -> ('self * type_kind) =
      fun __value ->
        match __value with
        | Ttype_abstract -> (self, __value)
        | Ttype_variant __x1 ->
            let (self, __y1) =
              self#list
                (fun self (((__x1, __x2, __x3, __x4) as __value)) ->
                   let __y1 = __x1 in
                   let __y2 = __x2 in
                   let (self, __y3) =
                     self#list (fun self -> self#core_type) __x3 in
                   let __y4 = __x4
                   in
                     (self,
                      (if
                         (__x2 == __y2) &&
                           ((__x3 == __y3) &&
                              ((__x4 == __y4) && (__x1 == __y1)))
                       then __value
                       else (__y1, __y2, __y3, __y4))))
                __x1
            in (self, (if __x1 == __y1 then __value else Ttype_variant __y1))
        | Ttype_record __x1 ->
            let (self, __y1) =
              self#list
                (fun self (((__x1, __x2, __x3, __x4, __x5) as __value)) ->
                   let __y1 = __x1 in
                   let __y2 = __x2 in
                   let __y3 = __x3 in
                   let (self, __y4) = self#core_type __x4 in
                   let __y5 = __x5
                   in
                     (self,
                      (if
                         (__x2 == __y2) &&
                           ((__x3 == __y3) &&
                              ((__x4 == __y4) &&
                                 ((__x5 == __y5) && (__x1 == __y1))))
                       then __value
                       else (__y1, __y2, __y3, __y4, __y5))))
                __x1
            in (self, (if __x1 == __y1 then __value else Ttype_record __y1))
    method exception_declaration :
      exception_declaration -> ('self * exception_declaration) =
      fun __value ->
        let (self, exn_params) =
          self#list (fun self -> self#core_type) __value.exn_params in
        let exn_exn = __value.exn_exn in
        let exn_loc = __value.exn_loc
        in
          (self,
           (if
              (__value.exn_exn == exn_exn) &&
                ((__value.exn_loc == exn_loc) &&
                   (__value.exn_params == exn_params))
            then __value
            else
              {
                exn_params = exn_params;
                exn_exn = exn_exn;
                exn_loc = exn_loc;
              }))
    method class_type : class_type -> ('self * class_type) =
      fun __value ->
        let (self, cltyp_desc) = self#class_type_desc __value.cltyp_desc in
        let cltyp_type = __value.cltyp_type in
        let cltyp_env = __value.cltyp_env in
        let cltyp_loc = __value.cltyp_loc
        in
          (self,
           (if
              (__value.cltyp_type == cltyp_type) &&
                ((__value.cltyp_env == cltyp_env) &&
                   ((__value.cltyp_loc == cltyp_loc) &&
                      (__value.cltyp_desc == cltyp_desc)))
            then __value
            else
              {
                cltyp_desc = cltyp_desc;
                cltyp_type = cltyp_type;
                cltyp_env = cltyp_env;
                cltyp_loc = cltyp_loc;
              }))
    method class_type_desc : class_type_desc -> ('self * class_type_desc) =
      fun __value ->
        match __value with
        | Tcty_constr (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let __y2 = __x2 in
            let (self, __y3) = self#list (fun self -> self#core_type) __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tcty_constr (__y1, __y2, __y3)))
        | Tcty_signature __x1 ->
            let (self, __y1) = self#class_signature __x1
            in
              (self, (if __x1 == __y1 then __value else Tcty_signature __y1))
        | Tcty_fun (__x1, __x2, __x3) ->
            let __y1 = __x1 in
            let (self, __y2) = self#core_type __x2 in
            let (self, __y3) = self#class_type __x3
            in
              (self,
               (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                then __value
                else Tcty_fun (__y1, __y2, __y3)))
    method class_signature : class_signature -> ('self * class_signature) =
      fun __value ->
        let (self, csig_self) = self#core_type __value.csig_self in
        let (self, csig_fields) =
          self#list (fun self -> self#class_type_field) __value.csig_fields in
        let csig_type = __value.csig_type in
        let csig_loc = __value.csig_loc
        in
          (self,
           (if
              (__value.csig_fields == csig_fields) &&
                ((__value.csig_type == csig_type) &&
                   ((__value.csig_loc == csig_loc) &&
                      (__value.csig_self == csig_self)))
            then __value
            else
              {
                csig_self = csig_self;
                csig_fields = csig_fields;
                csig_type = csig_type;
                csig_loc = csig_loc;
              }))
    method class_type_field :
      class_type_field -> ('self * class_type_field) =
      fun __value ->
        let (self, ctf_desc) = self#class_type_field_desc __value.ctf_desc in
        let ctf_loc = __value.ctf_loc
        in
          (self,
           (if (__value.ctf_loc == ctf_loc) && (__value.ctf_desc == ctf_desc)
            then __value
            else { ctf_desc = ctf_desc; ctf_loc = ctf_loc; }))
    method class_type_field_desc :
      class_type_field_desc -> ('self * class_type_field_desc) =
      fun __value ->
        match __value with
        | Tctf_inher __x1 ->
            let (self, __y1) = self#class_type __x1
            in (self, (if __x1 == __y1 then __value else Tctf_inher __y1))
        | Tctf_val __x1 ->
            let (self, __y1) =
              (fun (((__x1, __x2, __x3, __x4) as __value)) ->
                 let __y1 = __x1 in
                 let __y2 = __x2 in
                 let __y3 = __x3 in
                 let (self, __y4) = self#core_type __x4
                 in
                   (self,
                    (if
                       (__x2 == __y2) &&
                         ((__x3 == __y3) &&
                            ((__x4 == __y4) && (__x1 == __y1)))
                     then __value
                     else (__y1, __y2, __y3, __y4))))
                __x1
            in (self, (if __x1 == __y1 then __value else Tctf_val __y1))
        | Tctf_virt __x1 ->
            let (self, __y1) =
              (fun (((__x1, __x2, __x3) as __value)) ->
                 let __y1 = __x1 in
                 let __y2 = __x2 in
                 let (self, __y3) = self#core_type __x3
                 in
                   (self,
                    (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                     then __value
                     else (__y1, __y2, __y3))))
                __x1
            in (self, (if __x1 == __y1 then __value else Tctf_virt __y1))
        | Tctf_meth __x1 ->
            let (self, __y1) =
              (fun (((__x1, __x2, __x3) as __value)) ->
                 let __y1 = __x1 in
                 let __y2 = __x2 in
                 let (self, __y3) = self#core_type __x3
                 in
                   (self,
                    (if (__x2 == __y2) && ((__x3 == __y3) && (__x1 == __y1))
                     then __value
                     else (__y1, __y2, __y3))))
                __x1
            in (self, (if __x1 == __y1 then __value else Tctf_meth __y1))
        | Tctf_cstr __x1 ->
            let (self, __y1) =
              (fun (((__x1, __x2) as __value)) ->
                 let (self, __y1) = self#core_type __x1 in
                 let (self, __y2) = self#core_type __x2
                 in
                   (self,
                    (if (__x2 == __y2) && (__x1 == __y1)
                     then __value
                     else (__y1, __y2))))
                __x1
            in (self, (if __x1 == __y1 then __value else Tctf_cstr __y1))
    method class_declaration :
      class_declaration -> ('self * class_declaration) =
      fun __value -> self#class_infos (fun self -> self#class_expr) __value
    method class_description :
      class_description -> ('self * class_description) =
      fun __value -> self#class_infos (fun self -> self#class_type) __value
    method class_type_declaration :
      class_type_declaration -> ('self * class_type_declaration) =
      fun __value -> self#class_infos (fun self -> self#class_type) __value
    method class_infos :
      'a.
        ('self -> 'a -> ('self * 'a)) ->
          'a class_infos -> ('self * ('a class_infos)) =
      fun __f_a __value ->
        let ci_virt = __value.ci_virt in
        let (self, ci_params) =
          (fun (((__x1, __x2) as __value)) ->
             let (self, __y1) = self#list (fun self v -> (self, v)) __x1 in
             let __y2 = __x2
             in
               (self,
                (if (__x2 == __y2) && (__x1 == __y1)
                 then __value
                 else (__y1, __y2))))
            __value.ci_params in
        let ci_id_name = __value.ci_id_name in
        let ci_id_class = __value.ci_id_class in
        let ci_id_class_type = __value.ci_id_class_type in
        let ci_id_object = __value.ci_id_object in
        let ci_id_typesharp = __value.ci_id_typesharp in
        let (self, ci_expr) = __f_a self __value.ci_expr in
        let ci_decl = __value.ci_decl in
        let ci_type_decl = __value.ci_type_decl in
        let (self, ci_variance) =
          self#list (fun self (((__x1, __x2) as __value)) -> (self, __value))
            __value.ci_variance in
        let ci_loc = __value.ci_loc
        in
          (self,
           (if
              (__value.ci_params == ci_params) &&
                ((__value.ci_id_name == ci_id_name) &&
                   ((__value.ci_id_class == ci_id_class) &&
                      ((__value.ci_id_class_type == ci_id_class_type) &&
                         ((__value.ci_id_object == ci_id_object) &&
                            ((__value.ci_id_typesharp == ci_id_typesharp) &&
                               ((__value.ci_expr == ci_expr) &&
                                  ((__value.ci_decl == ci_decl) &&
                                     ((__value.ci_type_decl == ci_type_decl)
                                        &&
                                        ((__value.ci_variance == ci_variance)
                                           &&
                                           ((__value.ci_loc == ci_loc) &&
                                              (__value.ci_virt == ci_virt)))))))))))
            then __value
            else
              {
                ci_virt = ci_virt;
                ci_params = ci_params;
                ci_id_name = ci_id_name;
                ci_id_class = ci_id_class;
                ci_id_class_type = ci_id_class_type;
                ci_id_object = ci_id_object;
                ci_id_typesharp = ci_id_typesharp;
                ci_expr = ci_expr;
                ci_decl = ci_decl;
                ci_type_decl = ci_type_decl;
                ci_variance = ci_variance;
                ci_loc = ci_loc;
              }))
  end

class map = object (self)
  inherit omap

  method ref f r = let self, v = f self !r in if !r == v then self, r else self, ref v

  method list f xs = 
    let self, ys = List.fold_right (fun x (self, xs) -> 
      let self, x = f self x  in
      self, x::xs) xs (self, [])
    in
    if List.for_all2 (==) xs ys then self, xs else self, ys 

  (* Oh it's really a map. not bind *)    
  method option f = function 
    | (Some v as o) -> let self, v' = f self v in if v == v' then self, o else self, Some v'
    | None -> self, None
end
