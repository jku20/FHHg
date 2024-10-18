%----------------------------------------------------------------------------%
% An implementation of a NAND gate in the TFHE scheme.
%----------------------------------------------------------------------------%

:- module tfhe.
:- interface.

%----------------------------------------------------------------------------%

:- type tfhe_key ---> tfhe_key(uint32).
:- type tfhe_plaintext ---> tfhe_plaintext(uint32).
:- type tfhe_ciphertext ---> tfhe_ciphertext(uint32).

:- func tfhe_encrypt(tfhe_plaintext, tfhe_key) = tfhe_ciphertext.
:- func tfhe_decrypt(tfhe_ciphertext, tfhe_key) = tfhe_plaintext.

:- func tfhe_nand(tfhe_ciphertext, tfhe_ciphertext) = tfhe_ciphertext.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

tfhe_encrypt(P, K) =
  tfhe_ciphertext(0u32).

tfhe_decrypt(C, K) =
  tfhe_plaintext(0u32).

tfhe_nand(C1, C2) =
  tfhe_ciphertext(0u32).
