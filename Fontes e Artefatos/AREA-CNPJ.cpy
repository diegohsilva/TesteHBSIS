       77  Quociente-CGC             pic 9(03)   value zeros.
       77  Restos-CGC                pic 9(03)   value zeros.
       77  Total-Digitos-CGC         pic 9(04)   value zeros.
       77  CampoA-CGC                pic 9(01)   value zeros.
       77  CampoB-CGC                pic 9(01)   value zeros.
       77  Ws-Erro-CGC               pic 9       value zeros.

       01  WS-CGC-R                  pic 9(14).
       01  CGC redefines WS-CGC-R.
           05 NumeroA-CGC occurs 14 times pic 9.
       01  CGC-R redefines WS-CGC-R.
           05 NumeroB-CGC            pic 9(14).


       01 WS-ERRO-CNPJ PIC X(01) VALUE SPACES.
