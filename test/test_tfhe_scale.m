%----------------------------------------------------------------------------%
% Tests the tfhe.scale function.
%----------------------------------------------------------------------------%

:- module test_tfhe_scale.
:- interface.

:- import_module io.

%----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module random.
:- import_module random.sfc16.
:- import_module string.
:- import_module tfhe.

%----------------------------------------------------------------------------%

:- func random_seed = uint64.

%----------------------------------------------------------------------------%

random_seed = 0xdeadbeefu64.

main(!IO) :-
  R0 = sfc16.seed(random_seed),
  tfhe.mk_key(K, R0, R1),
  P0 = tfhe.plaintext(2i8),
  tfhe.encrypt(P0, K, C0, R1, _),
  C1 = -15i32 * C0,
  tfhe.decrypt(C1, K, P1),
  io.write_string(string(P1), !IO),
  io.nl(!IO).
