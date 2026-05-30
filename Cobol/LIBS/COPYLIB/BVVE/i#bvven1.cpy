      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK....: I#BVVEN1                                    *
      *  DESCRICAO.......: ARQUIVO AGENDA PARA ANTECIPACAO PROGRAMADA  *
      *                    DCOM                                        *
      *  TAMANHO.........: 100 BYTES                                   *
      ******************************************************************
      *                                                                *
       01  REG-BVVEN1.
           03 BVVEN1-AGENCIA           PIC 9(005).
           03 BVVEN1-CONTA             PIC 9(013).
           03 BVVEN1-PRODUTO           PIC 9(005).
           03 BVVEN1-DATA-UR           PIC X(010).
           03 BVVEN1-VALOR-UR          PIC 9(015)V99.
           03 BVVEN1-VALOR-DISPONIVEL  PIC 9(015)V99.
           03 FILLER                   PIC X(033).
