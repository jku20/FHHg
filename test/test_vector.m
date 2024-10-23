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
:- import_module vector.

%----------------------------------------------------------------------------%

:- pred test_mk_zero_vec(io::di, io::uo) is det.
:- pred test_mk_random_vec(io::di, io::uo) is det.
:- pred test_dot_product(io::di, io::uo) is det.
:- pred test_sum(io::di, io::uo) is det.
:- pred test_scale(io::di, io::uo) is det.
:- func random_seed = uint64.

%----------------------------------------------------------------------------%

random_seed = 0xdeadbeefu64.

main(!IO) :-
  test_mk_zero_vec(!IO),
  test_mk_random_vec(!IO),
  test_dot_product(!IO),
  test_sum(!IO),
  test_scale(!IO).

:- type vsize ---> size.
:- instance vector.dimension(vsize) where [
  dim(size) = 5u64
].

test_mk_zero_vec(!IO) :-
  vector.mk_zero_vec(size, V: vector(uint32, vsize)),
  io.write_string(string(V), !IO),
  io.nl(!IO).

test_mk_random_vec(!IO) :-
  R0 = sfc16.seed(random_seed),
  vector.mk_random_vec(size, V: vector(uint32, vsize), R0, _),
  io.write_string(string(V), !IO),
  io.nl(!IO).

test_dot_product(!IO) :- 
  R0 = sfc16.seed(random_seed),
  vector.mk_random_vec(size, V1: vector(uint32, vsize), R0, R1),
  vector.mk_random_vec(size, V2: vector(uint32, vsize), R1, _),
  vector.mk_zero_vec(size, V3: vector(uint32, vsize)),
  vector.mk_zero_vec(size, V4: vector(uint32, vsize)),
  P1 = V1 * V2,
  P2 = V1 * V3,
  P3 = V3 * V4,
  io.write_string(string(P1), !IO),
  io.nl(!IO),
  io.write_string(string(P2), !IO),
  io.nl(!IO),
  io.write_string(string(P3), !IO),
  io.nl(!IO).

test_sum(!IO) :- 
  R0 = sfc16.seed(random_seed),
  vector.mk_random_vec(size, V1: vector(uint32, vsize), R0, R1),
  vector.mk_random_vec(size, V2: vector(uint32, vsize), R1, _),
  vector.mk_zero_vec(size, V3: vector(uint32, vsize)),
  vector.mk_zero_vec(size, V4: vector(uint32, vsize)),
  P1 = V1 + V2,
  P2 = V1 + V3,
  P3 = V3 + V4,
  io.write_string(string(P1), !IO),
  io.nl(!IO),
  io.write_string(string(P2), !IO),
  io.nl(!IO),
  io.write_string(string(P3), !IO),
  io.nl(!IO).

test_scale(!IO) :- 
  R0 = sfc16.seed(random_seed),
  vector.mk_random_vec(size, V0: vector(uint32, vsize), R0, _),
  V1 = scale(2u32, V0),
  io.write_string(string(V1), !IO),
  io.nl(!IO).
