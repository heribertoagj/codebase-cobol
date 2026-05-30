      *----------------------------------------------------------------*
      * BOOK I#BVVEBW     ARQUIVO DE PAGAMENTOS ANTECIPACAO            *
      * TAMANHO : 122                                                  *
      *----------------------------------------------------------------*
      *
       01 BVVEBW-REGISTRO.
          05 BVVEBW-GRUPO                    PIC  9(03).
          05 BVVEBW-SUB-GRUPO                PIC  9(03).
          05 BVVEBW-AGENCIA-FORNEC           PIC  9(05).
          05 BVVEBW-CONTA-FORNEC             PIC  9(13).
          05 BVVEBW-CNPJ-CPF-FORNEC          PIC  9(09).
          05 BVVEBW-CNPJ-FILIAL-FORNEC       PIC  9(04).
          05 BVVEBW-CNPJ-CPF-CTR-FORNEC      PIC  9(02).
          05 BVVEBW-NOME-FORNEC              PIC  X(30).
          05 BVVEBW-DT-VENCTO                PIC  9(08).
          05 BVVEBW-VALOR-PAGTO              PIC  9(13)V99.
          05 BVVEBW-PAGADOR                  PIC  X(30).
