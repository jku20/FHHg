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
:- instance random_elm(int32).

:- type vector(T, D). % <= (ring(T), dimension(D), random_elm(T))

:- pred mk_zero_vec(D::in, vector(T, D)::out) is det
<= (ring(T), dimension(D), random_elm(T)).

:- pred mk_random_vec(D::in, vector(T, D)::out, R::in, R::out) is det 
<= (ring(T), dimension(D), random_elm(T), random(R)).

% Common Operations.

:- func vector(T, D) * vector(T, D) = 
  T is det <= (ring(T), dimension(D), random_elm(T)).

:- func dot(vector(T, D), vector(T, D)) = 
  T is det <= (ring(T), dimension(D), random_elm(T)).

:- func vector(T, D) + vector(T, D) = 
  vector(T, D) is det <= (ring(T), dimension(D), random_elm(T)).

:- func sum(vector(T, D), vector(T, D)) = 
  vector(T, D) is det <= (ring(T), dimension(D), random_elm(T)).

:- func scale(T, vector(T, D)) = 
  vector(T, D) is det <= (ring(T), dimension(D), random_elm(T)).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module int.
:- import_module int32.
:- import_module uint64.
:- import_module uint32.

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

% These are guarrenteed to be valid binary operations.
% The type system guarrentees the vectores are the same size.
dot(vector([], _), _) = zero.
dot(vector([ _ | _ ], _), vector([], _)) = zero.
dot(vector([ H1 | T1 ], D1), vector([ H2 | T2 ], D2)) = 
  (H1 * H2) + dot(vector(T1, D1), vector(T2, D2)).

V1 * V2 = dot(V1, V2).

:- func sum_lists(list(T), list(T)) = list(T) <= ring(T).
sum_lists([], _) = [].
sum_lists([ _ | _ ], []) = [].
sum_lists([ H1 | T1 ], [ H2 | T2 ]) = [ H1 + H2 | sum_lists(T1, T2) ].

sum(vector(L1, D), vector(L2, _)) = vector(sum_lists(L1, L2), D).

V1 + V2 = sum(V1, V2).

scale(_, vector([], D)) = vector([], D).
scale(T, vector([ H | L ], D)) = vector([ T * H | Tail ], D) :-
  vector(Tail, _) = scale(T, vector(L, D)).

% Impelmentations of random_elm for common types.
:- instance random_elm(uint32) where [
  rand(T, !R) :- generate_uint32(T, !R)
].

:- pred random_int32(int32::out, R::in, R::out) is det <= random(R).
random_int32(T, !R) :-
  generate_uint32(U, !R),
  I = cast_to_int(U),
  (if I > int32.to_int(int32.max_int32) then
    T = int32.det_from_int(
      int32.to_int(int32.min_int32) + I - int32.to_int(int32.max_int32))
  else
    T = int32.det_from_int(I)).

:- instance random_elm(int32) where [
  rand(T, !R) :- random_int32(T, !R)
].
