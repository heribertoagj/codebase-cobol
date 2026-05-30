      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      ******************************************************************
      *  NOME DO BOOK....: I#BVVEN0                                    *
      *  DESCRICAO.......: ANTECIPACAO PROGRAMADA - CANCELAMENTO OPER  *
      *                    ARQUIVO PREPARA ENVIO DO ARRC023            *
      *  TAMANHO.........: 200 BYTES                                   *
      ******************************************************************
      *                                                                *
       01  REG-BVVEN0.
           03 BVVEN0-AGENCIA                        PIC  9(005).
           03 BVVEN0-CONTA                          PIC  9(013).
           03 BVVEN0-COD-OPERACAO                   PIC  X(019).
           03 BVVEN0-IND-CANC-TOT                   PIC  X(001).
           03 BVVEN0-IND-LIQUIDA                    PIC  X(001).
           03 BVVEN0-IND-CANC-A-CONSTI              PIC  X(001).
           03 BVVEN0-PRODUTO                        PIC  9(005).
           03 BVVEN0-VLR-UR                         PIC  9(015)V99.
           03 BVVEN0-VLR-A-CONST                    PIC  9(015)V99.
           03 BVVEN0-DT-UR                          PIC  X(010).
           03 BVVEN0-CNPJ-CREDENC                   PIC  9(014).
           03 BVVEN0-CNPJ-USUAR-FNAL                PIC  9(014).
           03 BVVEN0-BANDEIRA                       PIC  X(003).
           03 BVVEN0-COD-OPER-CIP                   PIC  X(019).
           03 BVVEN0-CNPJ-VENDEDOR                  PIC  9(014).
           03 FILLER                                PIC  X(047).
