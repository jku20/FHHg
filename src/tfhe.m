%----------------------------------------------------------------------------%
% An implementation of a NAND gate in the TFHE scheme.
%----------------------------------------------------------------------------%

:- module tfhe.
:- interface.

%----------------------------------------------------------------------------%

:- type key ---> key(uint32).
:- type plaintext ---> plaintext(uint32).
:- type ciphertext ---> ciphertext(uint32).

:- func encrypt(plaintext, key) = ciphertext.
:- func decrypt(ciphertext, key) = plaintext.

:- func nand(ciphertext, ciphertext) = ciphertext.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

encrypt(P, K) =
  ciphertext(0u32).

decrypt(C, K) =
  plaintext(0u32).

nand(C1, C2) =
  ciphertext(0u32).
