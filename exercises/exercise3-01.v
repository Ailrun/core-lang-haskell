Require Import List.

Import ListNotations.

Inductive a_expr :=
| Num : nat -> a_expr
| Plus : a_expr -> a_expr -> a_expr
| Mult : a_expr -> a_expr -> a_expr
.

Fixpoint a_interpret (e : a_expr) : nat :=
  match e with
  | Num n => n
  | Plus e1 e2 => a_interpret e1 + a_interpret e2
  | Mult e1 e2 => a_interpret e1 * a_interpret e2
  end
.

Inductive a_instruction :=
| INum : nat -> a_instruction
| IPlus : a_instruction
| IMult : a_instruction
.

Fixpoint a_eval' (a_is : list a_instruction) (stk : list nat) : option nat :=
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
  end.

Definition a_eval (state : list a_instruction * list nat) : option nat :=
  match state with
  | (a_is, stk) => a_eval' a_is stk
  end
.

Fixpoint a_compile (e : a_expr) : list a_instruction :=
  match e with
  | Num n => [INum n]
  | Plus e1 e2 => a_compile e1 ++ a_compile e2 ++ [IPlus]
  | Mult e1 e2 => a_compile e1 ++ a_compile e2 ++ [IMult]
  end
.

Lemma interpret_compile_equiv_general : forall (a_is : list a_instruction) (stk : list nat) (e : a_expr),
    a_eval (a_is, a_interpret e :: stk) = a_eval (a_compile e ++ a_is, stk)
.
Proof.
  intros; generalize dependent stk; generalize dependent a_is.
  induction e; intros; simpl; auto; try (
    repeat rewrite app_assoc_reverse;
    unfold a_eval in IHe1; unfold a_eval in IHe2;
    rewrite <- IHe1; rewrite <- IHe2; now auto
  ).
Qed.

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
