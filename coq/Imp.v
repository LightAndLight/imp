Require Import Coq.Strings.String.

Definition context A := list ( string * A ).

Inductive lookup {A} : string -> context A -> A -> Prop :=
| lookup_here : forall s a rest, lookup s (cons (s , a) rest) a
| lookup_there : forall s rest a x, lookup s rest a -> lookup s (cons x rest) a.

Inductive insert {A} : string -> A -> context A -> context A -> Prop :=
| insert_here : forall s a b rest, insert s a (cons (s , b) rest) (cons (s , a) rest)
| insert_there : forall s a xs xs' x, insert s a xs xs' -> insert s a (cons x xs) (cons x xs').

Inductive expr : Set :=
| expr_ref : nat -> expr
| expr_var : string -> expr
| expr_int : nat -> expr
| expr_bool : bool -> expr
| expr_unit : expr.

Inductive stmt : Set :=
| st_seq : stmt -> stmt -> stmt
| st_pass : stmt
| st_read : stmt -> stmt
| st_expr : expr -> stmt
| st_write : stmt -> stmt -> stmt
| st_assign : string -> stmt -> stmt -> stmt.

Inductive entry : Set :=
| expr_entry : expr -> entry
| stmt_entry : stmt -> entry.

Definition bindings := context entry.
Definition store := list expr.

Inductive type : Set :=
| type_unit : type
| type_bool : type
| type_int : type
| type_ref : type -> type.

Inductive step_expr : bindings -> expr -> expr -> Prop :=
| step_var : forall v bds a, lookup v bds (expr_entry a) -> step_expr bds (expr_var v) a.

Inductive evalue : expr -> Prop :=
| evalue_int : forall n, evalue (expr_int n)
| evalue_bool : forall b, evalue (expr_bool b)
| evalue_unit : evalue expr_unit.

Inductive svalue : stmt -> Prop :=
| svalue_seq : forall st1 st2, svalue (st_seq st1 st2)
| svalue_pass : svalue st_pass
| svalue_read : forall st, svalue (st_read st)
| svalue_expr : forall e, evalue e -> svalue (st_expr e)
| svalue_write : forall st1 st2, svalue (st_write st1 st2).

Inductive retrieve {A} : nat -> list A -> A -> Prop :=
| retrieve_here : forall x xs, retrieve 0 (cons x xs) x
| retrieve_there : forall n xs a x, retrieve n xs a -> retrieve (S n) (cons x xs) a.

Inductive update {A} : nat -> A -> list A -> list A -> Prop :=
| update_here : forall x y xs, update 0 x (cons y xs) (cons x xs)
| update_there : forall n x xs xs' y, update n x xs xs' -> update (S n) x (cons y xs) (cons y xs').

Inductive step_stmt : (bindings * store * stmt) -> (bindings * store * stmt) -> Prop :=
| step_expr_var :
    forall bds str name a,
    lookup name bds (stmt_entry a) ->
    step_stmt (bds , str , st_expr (expr_var name)) (bds , str , a)
| step_seq_left :
    forall bds bds' str str' st1 st1' st2,
      step_stmt (bds , str , st1) (bds' , str', st1') ->
      step_stmt (bds , str , st_seq st1 st2) (bds' , str' , st_seq st1' st2)
| step_seq_right :
    forall bds str st1 st2,
      svalue st1 ->
      step_stmt (bds , str , st_seq st1 st2) (bds , str , st2)
| step_pass :
    forall bds str,
      step_stmt (bds , str , st_pass) (bds , str , st_expr expr_unit)
| step_read :
    forall bds bds' str str' st1 st1',
      step_stmt (bds , str , st1) (bds' , str' , st1') ->
      step_stmt (bds , str , st_read st1) (bds , str , st_read st1')
| step_read_reduce :
    forall bds str n a,
      retrieve n str a ->
      step_stmt (bds , str , st_read (st_expr (expr_ref n))) (bds , str , st_expr a)
| step_stexpr :
    forall bds str e e',
      step_expr bds e e' ->
      step_stmt (bds , str , st_expr e) (bds , str , st_expr e')
| step_write_left :
    forall bds bds' str str' st1 st1' st2,
      step_stmt (bds , str , st1) (bds' , str' , st1') ->
      step_stmt (bds , str , st_write st1 st2) (bds , str , st_write st1' st2)
| step_write_right :
    forall bds bds' str str' st1 st2 st2',
      svalue st1 ->
      step_stmt (bds , str , st2) (bds' , str' , st2') ->
      step_stmt (bds , str , st_write st1 st2) (bds , str , st_write st1 st2')
| step_write_reduce :
    forall bds str str' n val,
      update n val str str' ->
      step_stmt
        (bds , str , st_write (st_expr (expr_ref n)) (st_expr val))
        (bds , str' , st_expr expr_unit)
| step_assign :
    forall bds bds' str str' name val val' rest,
    step_stmt (bds , str , val) (bds' , str' , val') ->
    step_stmt (bds , str , st_assign name val rest) (bds , str , st_assign name val' rest)
| step_assign_reduce_expr :
    forall bds bds' str e name rest,
    insert name (expr_entry e) bds bds' ->
    step_stmt (bds , str , st_assign name (st_expr e) rest) (bds' , str , rest)
| step_assign_reduce_stmt :
    forall bds bds' str val name rest,
    svalue val ->
    insert name (stmt_entry val) bds bds' ->
    step_stmt (bds , str , st_assign name val rest) (bds' , str , rest).

Definition tybindings := context type.
Definition tystore := list type.

Inductive ety : tybindings -> tystore -> expr -> type -> Prop :=
| ety_ref : forall bds str n ty, retrieve n str ty -> ety bds str (expr_ref n) (type_ref ty)
| ety_var : forall bds str name ty, lookup name bds ty -> ety bds str (expr_var name) ty
| ety_int : forall bds str n, ety bds str (expr_int n) type_int
| ety_bool : forall bds str b, ety bds str (expr_bool b) type_bool
| ety_unit : forall bds str, ety bds str expr_unit type_unit.

Inductive sty : tybindings -> tystore -> stmt -> type -> Prop :=
| sty_seq :
    forall bds str st1 st2 ty1 ty2,
    sty bds str st1 ty1 ->
    sty bds str st2 ty2 ->
    sty bds str (st_seq st1 st2) ty2
| sty_pass :
    forall bds str,
      sty bds str st_pass type_unit
| sty_read :
    forall bds str st ty,
    sty bds str st (type_ref ty) ->
    sty bds str (st_read st) ty
| sty_expr : forall bds str e ty, ety bds str e ty -> sty bds str (st_expr e) ty
| sty_write :
    forall bds str st1 st2 ty,
    sty bds str st1 (type_ref ty) ->
    sty bds str st2 ty ->
    sty bds str (st_write st1 st2) type_unit
| sty_assign :
    forall bds str st1 st2 ty1 ty2 name,
    sty bds str st1 ty1 ->
    sty (cons (name , ty1) bds) str st2 ty2 ->
    sty bds str (st_assign name st1 st2) ty2.

Theorem expr_preservation :
  forall tybds tystr bds e e' ty,
    ety tybds tystr e ty -> step_expr bds e e' -> ety tybds tystr e' ty.
Proof.
  Admitted.