      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      ******************************************************************
      *  NOME DO BOOK....: I#BVVEFN                                    *
      *  DESCRICAO.......: CANCELAMENTO DE CESSAO EM LOTE              *
      *                    PREPARA ARQUIVO PARA ENVIO DO ARRC023       *
      *  TAMANHO.........: 250 BYTES                                   *
      ******************************************************************
      *                                                                *
       01  REG-BVVEFN.
           03 BVVEFN-AGENCIA                        PIC  9(005).
           03 BVVEFN-CONTA                          PIC  9(013).
           03 BVVEFN-CONTRATO                       PIC  X(019).
           03 BVVEFN-IND-CANC-TOT                   PIC  X(001).
           03 BVVEFN-IND-LIQ-CANC                   PIC  X(001).
           03 BVVEFN-IND-CANC-A-CONSTI              PIC  X(001).
           03 BVVEFN-PRODUTO                        PIC  9(005).
           03 BVVEFN-VLR-UR                         PIC  9(015)V99.
           03 BVVEFN-VLR-A-CONST                    PIC  9(015)V99.
           03 BVVEFN-DT-UR                          PIC  X(010).
           03 BVVEFN-CNPJ-CREDENC                   PIC  9(014).
           03 BVVEFN-CNPJ-USUAR-FNAL                PIC  9(014).
           03 BVVEFN-ARRANJO                        PIC  X(003).
           03 BVVEFN-COD-OPER-CIP                   PIC  X(019).
           03 BVVEFN-CNPJ-VENDEDOR                  PIC  9(014).
           03 BVVEFN-CANAL                          PIC  X(004).
           03 BVVEFN-PROTOCOLO                      PIC  X(050).
           03 FILLER                                PIC  X(093).
