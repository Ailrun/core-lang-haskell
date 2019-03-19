Require Import List.

Import ListNotations.

Inductive aExpr :=
| Num : nat -> aExpr
| Plus : aExpr -> aExpr -> aExpr
| Mult : aExpr -> aExpr -> aExpr
.

Fixpoint aInterpret (e : aExpr) : nat :=
  match e with
  | Num n => n
  | Plus e1 e2 => aInterpret e1 + aInterpret e2
  | Mult e1 e2 => aInterpret e1 * aInterpret e2
  end
.

Inductive aInstruction :=
| INum : nat -> aInstruction
| IPlus : aInstruction
| IMult : aInstruction
.

Fixpoint aEval' (aIs : list aInstruction) (stk : list nat) : option nat :=
  match aIs with
  | [] =>
    match stk with
    | [n] => Some n
    | _ => None
    end
  | INum n :: aIs' => aEval' aIs' (n :: stk)
  | IPlus :: aIs' =>
    match stk with
    | n0 :: n1 :: stk' => aEval' aIs' (n1 + n0 :: stk')
    | _ => None
    end
  | IMult :: aIs' =>
    match stk with
    | n0 :: n1 :: stk' => aEval' aIs' (n1 * n0 :: stk')
    | _ => None
    end
  end.

Definition aEval (state : list aInstruction * list nat) : option nat :=
  match state with
  | (aIs, stk) => aEval' aIs stk
  end
.

Fixpoint aCompile (e : aExpr) : list aInstruction :=
  match e with
  | Num n => [INum n]
  | Plus e1 e2 => aCompile e1 ++ aCompile e2 ++ [IPlus]
  | Mult e1 e2 => aCompile e1 ++ aCompile e2 ++ [IMult]
  end
.

Lemma interpret_compile_equiv_general : forall (aIs : list aInstruction) (stk : list nat) (e : aExpr),
    aEval (aIs, aInterpret e :: stk) = aEval (aCompile e ++ aIs, stk)
.
Proof.
  intros; generalize dependent stk; generalize dependent aIs.
  induction e; intros; simpl; auto; try (
    repeat rewrite app_assoc_reverse;
    unfold aEval in IHe1; unfold aEval in IHe2;
    rewrite <- IHe1; rewrite <- IHe2; now auto
  ).
Qed.

Theorem interpret_compile_equiv : forall (e : aExpr),
    Some (aInterpret e) = aEval (aCompile e, [])
.
Proof.
  intros.
  replace (aCompile e) with (aCompile e ++ []);
    auto with datatypes.
  rewrite <- interpret_compile_equiv_general.
  now auto.
Qed.
