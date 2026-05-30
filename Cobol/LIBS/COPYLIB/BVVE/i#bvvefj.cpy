      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      ******************************************************************
      *  NOME DO BOOK....: I#BVVEFJ                                    *
      *  DESCRICAO.......: OPERACOES EM LOTE - CANCELAMENTO            *
      *                    PREPARA ARQUIVO PARA ENVIO DO ARRC023       *
      *  TAMANHO.........: 200 BYTES                                   *
      ******************************************************************
      *                                                                *
       01  REG-BVVEFJ.
           03 BVVEFJ-AGENCIA                        PIC  9(005).
           03 BVVEFJ-CONTA                          PIC  9(013).
           03 BVVEFJ-COD-OPERACAO                   PIC  X(019).
           03 BVVEFJ-IND-CANC-TOT                   PIC  X(001).
           03 BVVEFJ-IND-LIQUIDA                    PIC  X(001).
           03 BVVEFJ-IND-CANC-A-CONSTI              PIC  X(001).
           03 BVVEFJ-PRODUTO                        PIC  9(005).
           03 BVVEFJ-VLR-UR                         PIC  9(015)V99.
           03 BVVEFJ-VLR-A-CONST                    PIC  9(015)V99.
           03 BVVEFJ-DT-UR                          PIC  X(010).
           03 BVVEFJ-CNPJ-CREDENC                   PIC  9(014).
           03 BVVEFJ-CNPJ-USUAR-FNAL                PIC  9(014).
           03 BVVEFJ-BAND-VISAO                     PIC  9(003).
           03 BVVEFJ-ARRANJO                        PIC  X(003).
           03 BVVEFJ-COD-OPER-CIP                   PIC  X(019).
           03 BVVEFJ-CNPJ-VENDEDOR                  PIC  9(014).
           03 BVVEFJ-CONTR-MAE-GRAN                 PIC  X(019).
           03 FILLER                                PIC  X(025).
