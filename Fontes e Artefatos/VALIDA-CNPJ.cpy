       VALIDA-CNPJ.


          compute Total-Digitos-CGC =
                  NumeroA-CGC (1)  *  5 + NumeroA-CGC (2)  * 4 +
                  NumeroA-CGC (3)  *  3 + NumeroA-CGC (4)  * 2 +
                  NumeroA-CGC (5)  *  9 + NumeroA-CGC (6)  * 8 +
                  NumeroA-CGC (7)  *  7 + NumeroA-CGC (8)  * 6 +
                  NumeroA-CGC (9)  *  5 + NumeroA-CGC (10) * 4 +
                  NumeroA-CGC (11) *  3 + NumeroA-CGC (12) * 2

          divide Total-Digitos-CGC by 11 giving Quociente-CGC remainder
                 Restos-CGC
          compute Total-Digitos-CGC = 11 - Restos-CGC

          if Restos-CGC = zeros
             move Restos-CGC to Campoa-CGC
          else
             move Total-Digitos-CGC to Campoa-CGC
          end-if

          if Campoa-CGC = NumeroA-CGC (13)
             next sentence
          else
             move 1 to Ws-Erro-CGC
             exit paragraph
          end-if.

          compute Total-Digitos-CGC =
                  NumeroA-CGC (1)  * 6 + NumeroA-CGC (2)  * 5 +
                  NumeroA-CGC (3)  * 4 + NumeroA-CGC (4)  * 3 +
                  NumeroA-CGC (5)  * 2 + NumeroA-CGC (6)  * 9 +
                  NumeroA-CGC (7)  * 8 + NumeroA-CGC (8)  * 7 +
                  NumeroA-CGC (9)  * 6 + NumeroA-CGC (10) * 5 +
                  NumeroA-CGC (11) * 4 + NumeroA-CGC (12) * 3 +
                  CampoA-CGC  * 2

          divide Total-Digitos-CGC by 11 giving Quociente-CGC remainder
                 Restos-CGC
          compute Total-Digitos-CGC = 11 - Restos-CGC

          if Restos-CGC = zeros
             move Restos-CGC to CampoB-CGC
          else
             move Total-Digitos-CGC to CampoB-CGC
          end-if

          if CampoB-CGC = NumeroA-CGC (14)
             move "N" to WS-ERRO-CNPJ
          else
             move "S" to WS-ERRO-CNPJ
          end-if.
       F-VALIDA-CNPJ. EXIT.

