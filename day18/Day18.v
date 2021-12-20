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
      (flatten lef (S depth)) ++
      (flatten rig (S depth))
  end.

Fixpoint collapse_stack' fuel stack :=
  match fuel with
  | O => stack
  | S n => match stack with
           | {| a := x; depth := dx |}::{| a := y; depth := dy |}::t =>
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

Definition flatT_is_t {A}: flatT A → Prop.
Proof.
Admitted.

Lemma flatten_flatT_is_t t n:
  flatT_is_t (flatten t n).
Proof.
Admitted.

Definition ojoin {A} (x : option (option A)) :=
  match x with
  | None => None
  | Some x => x
  end.

Definition ofmap {A B} (f: A → option B) (x: option A): option B :=
  ojoin (option_map f x).

Lemma unflatten_app_flatten t1 t2 n:
  unflatten (flatten t1 n ++ flatten t2 n)
  =
  ofmap (λ t1,
    option_map (λ t2,
      Node t1 t2)
    (unflatten (flatten t2 n)))
  (unflatten (flatten t1 n)).
Proof.
  (* possibly using flatten_flatT_is_t somehow? *)
Admitted.

Theorem inverse_unflatten_flatten t n:
  unflatten (flatten t n) = Some t.
Proof.
  generalize dependent n.
  induction t.
  - easy.
  - intro n. cbn.
    rewrite unflatten_app_flatten.
    now rewrite IHt1, IHt2.
Qed.

Theorem inverse_flatten_unflatten f n:
  flatT_is_t f → option_map (λ t, flatten t n) (unflatten f) = Some f.
Proof.
Abort.
