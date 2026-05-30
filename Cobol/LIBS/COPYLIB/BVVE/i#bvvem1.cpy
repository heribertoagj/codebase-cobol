      ******************************************************************
      *    I#BVVEM1 - LAY-OUT DA AREA DE COMUNICACAO COM O SUBPROGRAMA *
      *               BVVE1899                                         *
      *    LRECL: 316                                                  *
      ******************************************************************
       01  RELA-VISA-VALE.
           03  RELA-VV-INPUT.
               05  RELA-VV-NOME-ARQUIVO      PIC X(030).
               05  RELA-VV-COD-PRODUTO.
                   07  RELA-VV-COD-PRODUTO-N PIC 9(003).
               05  RELA-VV-COD-TARIFA.
                   07  RELA-VV-COD-TARIFA-N  PIC 9(003).
               03  FILLER                    PIC X(100).
           03  RELA-VV-OUTPUT.
               05  RELA-VV-COD-RETORNO       PIC 9(001).
               05  RELA-VV-MENSAGEM          PIC X(079).
               03  FILLER                    PIC X(100).

