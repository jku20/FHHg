%----------------------------------------------------------------------------%
% Tests the vector library.
%----------------------------------------------------------------------------%

:- module test_vector.
:- interface.

:- import_module io.

%----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module string.
:- import_module unit.
:- import_module random.
:- import_module random.sfc16.
:- use_module vector.

%----------------------------------------------------------------------------%

:- pred test_mk_zero_vec(io::di, io::uo) is det.
:- pred test_mk_random_vec(io::di, io::uo) is det.
:- pred test_mk_dot_product(io::di, io::uo) is det.
:- func random_seed = uint64.

%----------------------------------------------------------------------------%

random_seed = 0xdeadbeefu64.

main(!IO) :-
  test_mk_zero_vec(!IO),
  test_mk_random_vec(!IO),
  test_mk_dot_product(!IO).

:- type vsize ---> size.
:- instance vector.dimension(vsize) where [
  dim(size) = 5u64
].

test_mk_zero_vec(!IO) :-
  vector.mk_zero_vec(size, V: vector.vector(uint32, vsize)),
  io.write_string(string(V), !IO),
  io.nl(!IO).

test_mk_random_vec(!IO) :-
  R0 = sfc16.seed(random_seed),
  vector.mk_random_vec(size, V: vector.vector(uint32, vsize), R0, _),
  io.write_string(string(V), !IO),
  io.nl(!IO).

test_mk_dot_product(!IO) :- 
  R0 = sfc16.seed(random_seed),
  vector.mk_random_vec(size, V1: vector.vector(uint32, vsize), R0, R1),
  vector.mk_random_vec(size, V2: vector.vector(uint32, vsize), R1, _),
  vector.mk_zero_vec(size, V3: vector.vector(uint32, vsize)),
  vector.mk_zero_vec(size, V4: vector.vector(uint32, vsize)),
  P1 = vector.dot(V1, V2),
  P2 = vector.dot(V1, V3),
  P3 = vector.dot(V3, V4),
  io.write_string(string(P1), !IO),
  io.nl(!IO),
  io.write_string(string(P2), !IO),
  io.nl(!IO),
  io.write_string(string(P3), !IO),
  io.nl(!IO).
