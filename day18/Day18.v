Require Import Arith List Utf8.
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

Fixpoint collapse_stack' fuel stack :=
  match fuel with
  | O => stack
  | S n => match stack with
           | {| a := y; depth := dy |}::{| a := x; depth := dx |}::t =>
             if dx =? dy
             then collapse_stack' n ({| a := Node x y; depth := pred dx |}::t)
             else stack
           | _ => stack
           end
  end.

Definition collapse_stack stack := collapse_stack' (length stack) stack.

Fixpoint unflatten' stack (elts: flatT nat) :=
  match elts with
  | [] => match stack with
          | [s] => Some (a s)
          | _ => None
          end
  | {| a := n; depth := d |}::t =>
    unflatten' (collapse_stack ({| a := Leaf n; depth := d |}::stack)) t
  end.

Definition unflatten := unflatten' [].

Example ex1:
  unflatten (flatten (Node (Leaf 5) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 6))) 0)
  = Some (Node (Leaf 5) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 6))).
Proof. reflexivity. Qed.

(* Definition flatT_is_t {A}: flatT A → Prop. *)
(* Proof. *)
(* Admitted. *)

(* Lemma flatten_flatT_is_t t n: *)
(*   flatT_is_t (flatten t n). *)
(* Proof. *)
(* Admitted. *)

Lemma flatten_never_empty t d:
  flatten t d ≠ [].
Proof.
  generalize dependent d.
  induction t as [ n | t1 IHt1 t2 _]; try easy. simpl.
  intros d bogus.
  destruct (app_eq_nil _ _ bogus) as [Ht1 _].
  exact (IHt1 _ Ht1).
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
