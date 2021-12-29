Require Import Arith Recdef List Utf8.
Import ListNotations.

Inductive t :=
  | Leaf (n: nat)
  | Node (lef rig: t).

Record elt (A: Type) := { a: A; depth: nat }.
Arguments a {A}.

Definition flatT A: Type := list (elt A).

Fixpoint flatten t depth : flatT nat :=
  match t with
  | Leaf n => [{| a := n; depth := depth |}]
  | Node lef rig =>
      flatten lef (S depth) ++ flatten rig (S depth)
  end.

Function collapse_stack stack {measure length stack} :=
  match stack with
  | {| a := y; depth := dy |}::{| a := x; depth := dx |}::t =>
    if dx =? dy
    then collapse_stack ({| a := Node x y; depth := pred dx |}::t)
    else stack
  | _ => stack
  end.
Proof.
  auto.
Defined.

(* Check collapse_stack_equation. *)
(* Check collapse_stack_ind. *)

Fixpoint unflatten' stack (elts: flatT nat) :=
  match elts with
  | [] => match stack with
          | s::_ => Some (a s)
          | _ => None
          end
  | {| a := n; depth := d |}::t =>
    unflatten' (collapse_stack ({| a := Leaf n; depth := d |}::stack)) t
  end.

Definition unflatten := unflatten' [].

Example ex1:
  unflatten (flatten (Node (Leaf 5) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 6))) 0)
  = Some (Node (Leaf 5) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 6))).
Proof.
  reflexivity.
  (* With 'Qed' instead of 'Defined' in collapse_stack,
   * reflexivity is no longer enough, as the computation is opaque. So we
   * rewrite where we can and collapse (this computes the result). *)
  (* now repeat (repeat rewrite collapse_stack_equation; cbn). *)
Qed.

Inductive flatT_is_t {A}: nat → flatT A → Prop :=
  | flat_leaf d x: flatT_is_t d [{| a := x; depth := d |}]
  | flat_node d lef rig:
      flatT_is_t (S d) lef →
      flatT_is_t (S d) rig →
      flatT_is_t d (lef ++ rig)
.

Example ex2:
  flatT_is_t 2 [{| a := 3; depth := 3 |}; {| a := 4; depth := 3 |}].
Proof.
  apply (flat_node _ [{| a := 3; depth := 3 |}] _); constructor.
Qed.

Example ex3:
  flatT_is_t 0 (flatten (Node (Leaf 5) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 6))) 0).
Proof.
  simpl.
  apply (flat_node _ [_] _); try constructor.
  apply (flat_node _ [_; _] _); try constructor.
  apply (flat_node _ [_] _); constructor.
Qed.

Lemma flatten_never_empty t d:
  flatten t d ≠ [].
Proof.
  generalize dependent d.
  induction t as [ n | t1 IHt1 t2 _]; try easy. simpl.
  intros d bogus.
  destruct (app_eq_nil _ _ bogus) as [Ht1 _].
  exact (IHt1 _ Ht1).
Qed.

Lemma flatten_flatT_is_t t n:
  flatT_is_t n (flatten t n).
Proof.
  generalize dependent n.
  induction t; intros; simpl; constructor; auto.
Qed.

Lemma unflatten_app_lef lef rig d x:
  flatT_is_t d lef →
  unflatten lef = Some x →
  unflatten (lef ++ rig) = unflatten' [{| a := x; depth := d |}] rig.
Proof.
  intro H.
  generalize dependent rig.
  (* generalize dependent d. *)
  generalize dependent x.
  induction H; intros.
  - unfold unflatten in *; cbn in *.
    now inversion H.
  - unfold unflatten in *; cbn in *.
    (* clear IHflatT_is_t2. *)
    rewrite <- app_assoc.
    (* If I can find an XXX such that
       unflatten' [] lef = Some XXX
     * (there is one, by H1, somehow…), then I can use IHflatT_is_t1 to collapse
     * the goal a bit to

       unflatten' [{…}] (rig ++ rig0) =
       unflatten' [{| a := x; depth := d |}] rig0

     * Then it will be _very_ close in shape to IHflatT_is_t2.
     * Then I need a YYY such that
       unflatten' [] rig = Some YYY
     * (there is one, by H1, somehow…). Then can I use IHflatT_is_t2 somehow?
     *
     * Perhaps instead of unflatten I can use (unflatten' s) for any s?
     *)
    admit.
Admitted.

Lemma unflatten_rig rig y d x:
  flatT_is_t d rig →
  unflatten rig = Some y →
  unflatten' [{| a := x; depth := d |}] rig = Some (Node x y).
Proof.
Admitted.

Theorem unflatten'_flatT ft d:
  flatT_is_t d ft →
  (* ∀ s, option_map (depth t) (hd_error s) ≠ Some d → *)
  ∃ tr, unflatten ft = Some tr.
Proof.
  intros H. induction H.
  - now exists (Leaf x).
    (* destruct s; simpl; [ easy | ]. *)
    (* destruct e as [y dep]. *)
    (* rewrite collapse_stack_equation. *)
    (* assert (dep ≠ d) by (now intros ->); clear H. *)
    (* destruct (Nat.eqb_spec dep d); easy. *)
  - destruct IHflatT_is_t1 as [x Hx].
    destruct IHflatT_is_t2 as [y Hy]. (* note it starts from the empty stack *)
    exists (Node x y).
    rewrite (unflatten_app_lef _ _ (S d) x); auto.
    now apply unflatten_rig.
Qed.

(* Somehow I need to characterize the stack:
 * any flatT nat that forms a binary tree should collapse to a single-element on
 * the stack, and therefore any two flatT nat that form a single binary tree
 * should collapse first to [t1, t2] on the stack and then finally to [Node t1
 * t2].
 *)

Lemma unflatten_app_flatten t1 t2 d:
  unflatten (flatten (Node t1 t2) d) = Some (Node t1 t2).
Proof.
  generalize dependent t2.
  generalize dependent d.
  induction t1; intros.

  - cbn.
    destruct_with_eqn (flatten t2 (S d)); simpl.
    * destruct (flatten_never_empty _ _ Heqf).
    * destruct e.
      destruct t2.
      + inversion Heqf; subst; cbn.
        now destruct (Nat.eqb_spec d d).
      +
        (* simpl in Heqf. *)
      (* not sure where to go here:
       * we don't really know anything about the relationship between
       * depth0 and S d, partly because we know so little about t2.
       * Possibly something about the way t2_1 and a0/depth0 relate? *)
        destruct (Nat.eqb_spec depth0 (S d)); subst; cbn.
        -- contradict Heqf. admit.
        -- 
      admit.
  - 
    (* seems like I should be able to run
     * the flatten part of (Node t11 t12) (with depth (S d))
     * (IHt11 says we'll get Some (Node t11 t12))
     * and then plug that in to the stuff about t2
     * But we know nothing about t2, so probably I need some lemma there.
     * The issue then is what lemma? Trying to look at
     * unflatten (flatten t2 (S d)) isn't really different than the main inverse
     * theorem, and looking at the one-hole context might not be either. *)
    pose (IHt1_1 (S d) t1_2).
    simpl.
    admit.

  (* - destruct_with_eqn t2; cbn. *)
  (*   * now destruct (Nat.eqb_spec d d). *)
  (*   * *) 

  (* possibly using flatten_flatT_is_t somehow? *)
Admitted.

Theorem inverse_unflatten_flatten t n:
  unflatten (flatten t n) = Some t.
Proof.
  generalize dependent n.
  induction t.
  - easy.
  - now apply unflatten_app_flatten.
Qed.

Theorem inverse_flatten_unflatten f n:
  flatT_is_t f → option_map (λ t, flatten t n) (unflatten f) = Some f.
Proof.
Abort.
