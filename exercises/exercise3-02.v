Require Import Basics.
Require Import DecBool.
Require Import Nat.
Require Import String.
Require Import List.

Import ListNotations.

Inductive a_expr :=
| Num : nat -> a_expr
| Plus : a_expr -> a_expr -> a_expr
| Mult : a_expr -> a_expr -> a_expr
| AVar : string -> a_expr
| ALet : list (string * a_expr) -> a_expr -> a_expr
.

Definition a_int_env := list (string * nat).

Fixpoint a_interpret' (e : a_expr) (env : a_int_env) : option nat :=
  match e with
  | Num n => Some n
  | Plus e1 e2 =>
    match (a_interpret' e1 env, a_interpret' e2 env) with
      | (Some n1, Some n2) => Some (n1 + n2)
      | _ => None
    end
  | Mult e1 e2 =>
    match (a_interpret' e1 env, a_interpret' e2 env) with
      | (Some n1, Some n2) => Some (n1 * n2)
      | _ => None
    end
  | ALet bs bodyE =>
    let b_folder :=
        fun acc b =>
          match a_interpret' (snd b) env with
          | Some n => ((compose (cons (fst b, n)) (fst acc)), snd acc)
          | _ => (fst acc, false)
          end
    in
    let (make_b_env, is_success) := fold_left b_folder bs (id, true) in
    if is_success
    then a_interpret' bodyE (make_b_env [] ++ env)
    else None
  | AVar id =>
    match find (compose (eqb id) fst) env with
    | Some (_, n) => Some n
    | _ => None
    end
  end
.

Definition a_interpret (e : a_expr) : option nat := a_interpret' e [].

Inductive a_instruction :=
| INum : nat -> a_instruction
| IPlus : a_instruction
| IMult : a_instruction
| ICopy : nat -> a_instruction
| ISlide : nat -> a_instruction
| IFail : a_instruction
.

Definition a_stack := list nat.

Fixpoint a_eval' (a_is : list a_instruction) (stk : a_stack) : option nat :=
  match a_is with
  | [] =>
    match stk with
    | [n] => Some n
    | _ => None
    end
  | INum n :: a_is' => a_eval' a_is' (n :: stk)
  | IPlus :: a_is' =>
    match stk with
    | n0 :: n1 :: stk' => a_eval' a_is' (n1 + n0 :: stk')
    | _ => None
    end
  | IMult :: a_is' =>
    match stk with
    | n0 :: n1 :: stk' => a_eval' a_is' (n1 * n0 :: stk')
    | _ => None
    end
  | ICopy i :: a_is' =>
    match nth i (map Some stk) None with
    | Some n => a_eval' a_is' (n :: stk)
    | _ => None
    end
  | ISlide i :: a_is' =>
    match stk with
    | n :: stk' => a_eval' a_is' (n :: skipn i stk')
    | _ => None
    end
  | IFail :: _ => None
  end.

Definition a_eval (state : list a_instruction * list nat) : option nat :=
  match state with
  | (a_is, stk) => a_eval' a_is stk
  end
.

Definition a_com_env := list (string * nat).

Fixpoint env_offset (o : nat) (e : a_com_env) : a_com_env :=
  match e with
  | [] => []
  | (id, n) :: e' => (id, n + o) :: env_offset o e'
  end
.

Fixpoint a_compile' (e : a_expr) (env : a_com_env) : list a_instruction :=
  match e with
  | Num n => [INum n]
  | Plus e1 e2 => a_compile' e1 env ++ a_compile' e2 (env_offset 1 env) ++ [IPlus]
  | Mult e1 e2 => a_compile' e1 env ++ a_compile' e2 (env_offset 1 env) ++ [IMult]
  | ALet bs eBody =>
    let b_length := length bs in
    let b_folder :=
        fun acc b =>
          match acc with
          | (n, f) => (n - 1, a_compile' (snd b) (env_offset n env) ++ f)
          end in
    let b_is := snd (fold_left b_folder bs (b_length - 1, [])) in
    let b_env := combine (map fst bs) (rev (seq 0 b_length)) ++ (env_offset b_length env) in
    b_is ++ a_compile' eBody b_env ++ [ISlide b_length]
  | AVar id =>
    match find (compose (eqb id) fst) env with
      | Some (_, i) => [ICopy i]
      | _ => [IFail]
    end
  end
.

Definition a_compile (e : a_expr) : list a_instruction := a_compile' e [].

Definition int_env_to_com_env (int_env : a_int_env) : a_com_env := combine (map fst int_env) (rev (seq 0 (length int_env))).

Definition int_env_to_stack (int_env : a_int_env) : a_stack := rev (map snd int_env).

Definition com_env_stack_to_int_env (com_env : a_com_env) (stack : a_stack) : option a_int_env :=
  let max_ind := fold_left max (map (compose (plus 1) snd) com_env) 0 in
  if max_ind <=? length stack
  then Some (map (fun b => (fst b, nth (snd b) stack 0)) com_env)
  else None
.

(*
Lemma interpret_compile_equiv_general : forall (a_is : list a_instruction) (stk : a_stack) (e : a_expr),
    match a_interpret e with
    | Some n => a_eval (a_is, n :: stk) = a_eval (a_compile e ++ a_is, stk)
    | _ => True
    end
.
Proof.
  intros; generalize dependent stk; generalize dependent a_is.
  induction e; intros; simpl; try auto;
    try ( unfold a_interpret; unfold a_interpret in IHe1; unfold a_interpret in IHe2; simpl;
          destruct (a_interpret' e1 []); destruct (a_interpret' e2 []); auto;
          unfold a_compile; simpl; unfold a_eval in IHe1; unfold a_eval in IHe2;
          repeat rewrite app_assoc_reverse; rewrite <- IHe1; rewrite <- IHe2; now auto
        ).
  unfold a_interpret; unfold a_interpret in IHe; simpl.
  (* Need some lemma for `a_interpret e p` case *)
Abort.
*)

(*
Theorem interpret_compile_equiv : forall (e : a_expr),
    Some (a_interpret e) = a_eval (a_compile e, [])
.
Proof.
  intros.
  replace (a_compile e) with (a_compile e ++ []);
    auto with datatypes.
  rewrite <- interpret_compile_equiv_general.
  now auto.
Qed.
*)
