      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      ******************************************************************
      *  NOME DO BOOK....: I#BVVEN2                                    *
      *  DESCRICAO.......: ANTECIPACAO PROGRAMADA
      *                    ARQUIVO DE INTENCOES DCOM                   *
      *  TAMANHO.........: 150 BYTES                                   *
      ******************************************************************
      *                                                                *
       01  REG-BVVEN2.
           03 BVVEN2-DADOS-CONTRATO.
              05 BVVEN2-AGENCIA                     PIC  9(005).
              05 BVVEN2-CONTA                       PIC  9(013).
              05 BVVEN2-COD-OPERACAO                PIC  X(019).
              05 BVVEN2-IND-I-A                     PIC  X(001).
              05 BVVEN2-DT-VCTO-OPER                PIC  X(010).
              05 BVVEN2-VLR-TOT-OPER                PIC  9(015)V99.
           03 BVVEN2-DADOS-UR.
              05 BVVEN2-PRODUTO                     PIC  9(005).
              05 BVVEN2-VLR-OPER-PRODUTO            PIC  9(015)V99.
              05 BVVEN2-DT-UR                       PIC  X(010).
           03 FILLER                                PIC  X(053).
