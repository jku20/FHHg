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

:- import_module string.
:- use_module tfhe.

%----------------------------------------------------------------------------%

main(!IO) :-
  K = tfhe.key(0u32),
  P1 = tfhe.plaintext(0u32),
  P2 = tfhe.plaintext(0u32),
  C1 = tfhe.encrypt(P1, K),
  C2 = tfhe.encrypt(P2, K),
  C3 = tfhe.nand(C1, C2),
  P3 = tfhe.decrypt(C3, K),
  io.write_string(string(P3), !IO),
  io.nl(!IO).
