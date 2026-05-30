      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      ******************************************************************
      *  NOME DO BOOK....: I#BVVEN3                                    *
      *  DESCRICAO.......: ANTECIPACAO PROGRAMADA
      *                    RETORNO DE PROCESSAMENTO CIP                *
      *  TAMANHO.........: 150 BYTES                                   *
      ******************************************************************
      *                                                                *
       01  REG-BVVEN3.
           03 BVVEN3-AGENCIA                        PIC  9(005).
           03 BVVEN3-CONTA                          PIC  9(013).
           03 BVVEN3-COD-OPERACAO                   PIC  X(019).
           03 BVVEN3-PRODUTO                        PIC  9(005).
           03 BVVEN3-DATA-UR                        PIC  X(010).
           03 BVVEN3-VLR-MARCADO                    PIC  9(015)V99.
           03 BVVEN3-IND-SIT                        PIC  X(001).
           03 BVVEN3-MOTIVO-REJEI                   PIC  X(050).
           03 FILLER                                PIC  X(030).
      *    BVVEN3-IND-SIT: 'A' ACEITA OU 'R' REJEITADA
