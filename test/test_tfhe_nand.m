%----------------------------------------------------------------------------%
% Tests the tfhe_nand function.
%----------------------------------------------------------------------------%

:- module test_tfhe_nand.
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
:- use_module tfhe.

%----------------------------------------------------------------------------%

:- func random_seed = uint64.

%----------------------------------------------------------------------------%

random_seed = 0xdeadbeefu64.

main(!IO) :-
  R0 = sfc16.seed(random_seed),
  tfhe.mk_key(K, R0, R1),
  P1 = tfhe.plaintext(0i8),
  P2 = tfhe.plaintext(0i8),
  tfhe.encrypt(P1, K, C1, R1, R2),
  tfhe.encrypt(P2, K, C2, R2, _),
  C3 = tfhe.nand(C1, C2),
  tfhe.decrypt(C3, K, P3),
  io.write_string(string(P3), !IO),
  io.nl(!IO).
