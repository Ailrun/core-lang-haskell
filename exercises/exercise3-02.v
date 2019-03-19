Require Import Basics.
Require Import DecBool.
Require Import List.
Require Import String.

Import ListNotations.

Inductive aExpr :=
| Num : nat -> aExpr
| Plus : aExpr -> aExpr -> aExpr
| Mult : aExpr -> aExpr -> aExpr
| AVar : string -> aExpr
| ALet : list (string * aExpr) -> aExpr -> aExpr
.

Definition beq_string s1 s2 : bool := if string_dec s1 s2 then true else false.

Fixpoint aInterpret' (e : aExpr) (env : list (string * option nat)) : option nat :=
  match e with
  | Num n => Some n
  | Plus e1 e2 =>
    match (aInterpret' e1 env, aInterpret' e2 env) with
      | (Some n1, Some n2) => Some (n1 + n2)
      | _ => None
    end
  | Mult e1 e2 =>
    match (aInterpret' e1 env, aInterpret' e2 env) with
      | (Some n1, Some n2) => Some (n1 * n2)
      | _ => None
    end
  | ALet bs bodyE => aInterpret' bodyE ((map (fun b => match b with (bId, bExpr) => (bId, aInterpret' bExpr env) end) bs) ++ env)
  | AVar id =>
    match find (compose (flip beq_string id) fst) env with
    | Some (_, Some n) => Some n
    | _ => None
    end
  end
.

Definition aInterpret (e : aExpr) : option nat := aInterpret' e [].

Inductive aInstruction :=
| INum : nat -> aInstruction
| IPlus : aInstruction
| IMult : aInstruction
| ICopy : nat -> aInstruction
| ISlide : nat -> aInstruction
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
  | ICopy i :: aIs' =>
    match nth i (map Some stk) None with
    | Some n => aEval' aIs' (n :: stk)
    | _ => None
    end
  | ISlide i :: aIs' =>
    match stk with
    | n :: stk' => aEval' aIs' (n :: skipn i stk')
    | _ => None
    end
  end.

Definition aEval (state : list aInstruction * list nat) : option nat :=
  match state with
  | (aIs, stk) => aEval' aIs stk
  end
.

Fixpoint envOffset (o : nat) (e : list (string * nat)) : list (string * nat) :=
  match e with
  | [] => []
  | (id, n) :: e' => (id, n + o) :: envOffset o e'
  end
.

Fixpoint aCompileExpr (e : aExpr) (env : list (string * nat)) : list aInstruction :=
  match e with
  | Num n => [INum n]
  | Plus e1 e2 => aCompileExpr e1 env ++ aCompileExpr e2 (envOffset 1 env) ++ [IPlus]
  | Mult e1 e2 => aCompileExpr e1 env ++ aCompileExpr e2 (envOffset 1 env) ++ [IMult]
  | ALet bs eBody =>
    let bLength := List.length bs in
    let bFolder :=
        fun acc b =>
          match acc with
          | (n, f) => (n - 1, aCompileExpr (snd b) (envOffset n env) ++ f)
          end in
    let bIs := snd (fold_left bFolder bs (bLength - 1, [])) in
    let bEnv := combine (map fst bs) (rev (seq 0 bLength)) ++ (envOffset bLength env) in
    bIs ++ aCompileExpr eBody bEnv ++ [ISlide bLength]
  | AVar id =>
    match find (compose (flip beq_string id) fst) env with
      | Some (_, i) => [ICopy i]
      | _ => []
    end
  end
.

Definition aCompile (e : aExpr) : list aInstruction := aCompileExpr e [].

Lemma interpret_compile_equiv_general : forall (aIs : list aInstruction) (stk : list nat) (e : aExpr),
    match aInterpret e with
    | Some n => aEval (aIs, n :: stk) = aEval (aCompile e ++ aIs, stk)
    | _ => True
    end
.
Proof.
  intros; generalize dependent stk; generalize dependent aIs.
  induction e; intros; simpl; try auto;
    try ( unfold aInterpret; unfold aInterpret in IHe1; unfold aInterpret in IHe2; simpl;
          destruct (aInterpret' e1 []); destruct (aInterpret' e2 []); auto;
          unfold aCompile; simpl; unfold aEval in IHe1; unfold aEval in IHe2;
          repeat rewrite app_assoc_reverse; rewrite <- IHe1; rewrite <- IHe2; now auto
        ).
  unfold aInterpret; unfold aInterpret in IHe; simpl.
  (* Need some lemma for `aInterpret e p` case *)
Abort.
(* Qed. *)

(*
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
 *)
