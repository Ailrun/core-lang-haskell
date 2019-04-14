Require Import Relations.

Theorem same_clos_same : forall {A : Type} R S,
    same_relation A R S ->
    same_relation A (clos_refl_trans_1n A R) (clos_refl_trans_1n A S).
Proof.
  intros; inversion_clear H as [HRS HSR];
    split; intros x y Hxy;
      induction Hxy; try (now constructor);
        constructor 2 with y; now auto.
Qed.

Theorem clos_refl_trans_1n_reverse : forall A R x y z,
    clos_refl_trans_1n A R x y ->
    R y z ->
    clos_refl_trans_1n A R x z.
Proof.
  intros.
  induction H; intros.
  - constructor 2 with z; auto; now constructor.
  - constructor 2 with y; auto.
Qed.

Theorem clos_refl_trans_1n_transp_permute : forall A R x y,
    transp A (clos_refl_trans_1n A R) x y <-> clos_refl_trans_1n A (transp A R) x y.
Proof.
  intros; split; intros;
    unfold transp in H;
    induction H; try (now constructor);
      apply clos_refl_trans_1n_reverse with y; auto.
Qed.

Definition DiGraph (V : Set) := V -> V -> bool.

Definition dg_outs {V : Set} (gr : DiGraph V) : relation V :=
  fun (v v' : V) => gr v v' = true.
Definition dg_ins {V : Set} (gr : DiGraph V) : relation V :=
  fun (v v' : V) => gr v' v = true.

Definition dg_outs_star {V : Set} (gr : DiGraph V) := clos_refl_trans_1n V (dg_outs gr).
Definition dg_ins_star {V : Set} (gr : DiGraph V) := clos_refl_trans_1n V (dg_ins gr).

Definition dg_scc {V : Set} (gr : DiGraph V) : relation V := fun x y => dg_outs_star gr x y /\ dg_ins_star gr x y.

Definition in_the_same_scc {V : Set} (gr : DiGraph V) : relation V :=
  fun (v v' : V) => exists (a : V), dg_scc gr a v /\ dg_scc gr a v'.


Lemma dg_ins__equiv__trasp_dg_outs : forall {V : Set} gr,
    same_relation V (dg_ins gr) (transp V (dg_outs gr)).
Proof.
  intros; split; intros x y Hxy; now auto.
Qed.

Lemma dg_ins_star_transitive : forall {V : Set} gr,
    transitive V (dg_ins_star gr).
Proof.
  intros. intros x y z Hxy Hyz.
  generalize dependent z.
  induction Hxy as [x | x y v]; try (now auto).
  intros z Hvz.
  constructor 2 with y; auto.
  apply IHHxy. apply Hvz.
Qed.

Lemma dg_outs_star_transitive : forall {V : Set} gr,
    transitive V (dg_outs_star gr).
Proof.
  intros. intros x y z Hxy Hyz.
  generalize dependent z.
  induction Hxy as [x | x y v]; try (now auto).
  intros z Hvz.
  constructor 2 with y; auto.
  apply IHHxy. apply Hvz.
Qed.

Theorem in_the_same_scc_reflexive : forall {V : Set} gr,
    reflexive V (in_the_same_scc gr).
Proof.
  intros; intro x.
  exists x; now (repeat constructor).
Qed.

Theorem in_the_same_scc_symmetric : forall {V : Set} gr,
    symmetric V (in_the_same_scc gr).
Proof.
  intros; intros x y H.
  inversion_clear H as [v Hv].
  inversion_clear Hv as [Hx Hy].
  inversion_clear Hx as [Hxouts Hxins].
  inversion_clear Hy as [Hyouts Hyins].
  exists v; split; split; now auto.
Qed.

Theorem in_the_same_scc_transitive : forall {V : Set} gr,
    transitive V (in_the_same_scc gr).
Proof.
  intros; intros x y z Hxy Hyz.
  enough (HDG : (forall a b, dg_ins_star gr a b -> dg_outs_star gr b a) /\ (forall a b, dg_outs_star gr a b -> dg_ins_star gr b a)).
  destruct HDG.
  - inversion_clear Hxy as [vxy Hvxy].
    inversion_clear Hvxy as [Hvxyx Hvxyy].
    inversion_clear Hvxyx as [Hvxyxout Hvxyxin].
    inversion_clear Hvxyy as [Hvxyyout Hvxyyin].
    inversion_clear Hyz as [vyz Hvyz].
    inversion_clear Hvyz as [Hvyzy Hvyzz].
    inversion_clear Hvyzy as [Hvyzyout Hvyzyin].
    inversion_clear Hvyzz as [Hvyzzout Hvyzzin].
    exists y; split;
      (split; [eapply dg_outs_star_transitive; swap 1 2 | eapply dg_ins_star_transitive; swap 1 2]);
      now eauto.
  - destruct (same_clos_same (dg_ins gr) (transp V (dg_outs gr)) (dg_ins__equiv__trasp_dg_outs gr)).
    split; intros;
      [replace (dg_outs_star gr b a) with (transp V (dg_outs_star gr) a b) |
       replace (dg_outs_star gr a b) with (transp V (dg_outs_star gr) b a) in H1]; auto;
        unfold inclusion in H;
        apply clos_refl_trans_1n_transp_permute; auto.
Qed.

Lemma in_the_same_scc_equiv : forall {V : Set} gr,
    equiv V (in_the_same_scc gr).
Proof.
  intros; repeat (try split);
    try apply in_the_same_scc_reflexive;
    try apply in_the_same_scc_symmetric;
    try apply in_the_same_scc_transitive.
Qed.
