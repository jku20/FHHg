%----------------------------------------------------------------------------%
% The driver for the program.
%----------------------------------------------------------------------------%

:- module driver.
:- interface.

:- import_module io.

%----------------------------------------------------------------------------%

:- pred run(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module string.
:- import_module tfhe.

%----------------------------------------------------------------------------%

run(!IO) :-
  K = tfhe_key(0u32),
  P1 = tfhe_plaintext(0u32),
  P2 = tfhe_plaintext(0u32),
  C1 = tfhe_encrypt(P1, K),
  C2 = tfhe_encrypt(P2, K),
  C3 = tfhe_nand(C1, C2),
  P3 = tfhe_decrypt(C3, K),
  io.write_string(string.string(P3), !IO),
  io.nl(!IO).
