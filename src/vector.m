%----------------------------------------------------------------------------%
% A constant size vector (in the lin alg sense) over rings.
%----------------------------------------------------------------------------%

:- module vector.
:- interface.

:- import_module ring.
:- use_module uint32.
:- import_module random.

%----------------------------------------------------------------------------%

% Vector dimension reified as a type.
:- typeclass dimension(D) where [
  % Returns the dimension.
  func dim(D) = uint64
].

% It's possible to generate a random element.
:- typeclass random_elm(T) where [
  % Returns a random element of T.
  pred rand(T::out, R::in, R::out) is det <= random(R)
].

:- instance random_elm(uint32).

:- type vector(T, D). % <= (ring(T), dimension(D), random_elm(T))

:- pred mk_zero_vec(D::in, vector(T, D)::out) is det
<= (ring(T), dimension(D), random_elm(T)).

:- pred mk_random_vec(D::in, vector(T, D)::out, R::in, R::out) is det 
<= (ring(T), dimension(D), random_elm(T), random(R)).

:- func dot(vector(T, D), vector(T, D)) = 
  T is det <= (ring(T), dimension(D), random_elm(T)).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module uint64.

%----------------------------------------------------------------------------%

:- type vector(T, D) --->  vector(
                            store :: list(T), 
                            size :: D
                          ).

:- func mk_zero_list(uint64) = list(T) <= ring(T).
mk_zero_list(N) = 
  (if N = 0u64 then [ ] else [ zero | mk_zero_list(N - 1u64) ]).

:- pred mk_random_list(
  uint64::in, list(T)::out, R::in, R::out
) <= (ring(T), random(R), random_elm(T)).

mk_random_list(N, L, R0, R2) :-
  (if N = 0u64 then 
    R0 = R2,
    L = [ ] 
  else 
    rand(V, R0, R1),
    mk_random_list(N - 1u64, T, R1, R2),
    L = [ V | T ]).

mk_zero_vec(Dim, V) :- V = vector(mk_zero_list(dim(Dim)), Dim).

mk_random_vec(Dim, V, !R) :- 
  mk_random_list(dim(Dim), L, !R),
  V = vector(L, Dim).

% This is guarrenteed to be a valid dot product.
% The type system guarrentees the vectores are the same size.
dot(vector([], _), _) = one.
dot(vector([ _ | _ ], _), vector([], _)) = one.
dot(vector([ H1 | T1 ], D1), vector([ H2 | T2 ], D2)) = 
  H1 * H2 + dot(vector(T1, D1), vector(T2, D2)).

% Impelmentations of random_elm for common types.
:- instance random_elm(uint32) where [
  rand(T, !R) :- generate_uint32(T, !R)
].
