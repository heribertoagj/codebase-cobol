      ******************************************************************
      * NOME BOOK : DCOMW88I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DATA      : 22/11/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 0928 BYTES                                         *
      ******************************************************************
       05 DCOMW88I-HEADER.
         10 DCOMW88I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW88I'.
         10 DCOMW88I-TAM-LAYOUT        PIC 9(05)   VALUE 0928.
       05 DCOMW88I-REGISTRO.
         10 DCOMW88I-BLOCO-ENTRADA.
           15 DCOMW88I-E-NUM-OPER                       PIC 9(13).
         10 DCOMW88I-BLOCO-SAIDA.
           15 DCOMW88I-S-NNOTA-PROMIS                    PIC 9(11).
           15 DCOMW88I-S-DVCTO-FNAL-OPER                 PIC X(10).
           15 DCOMW88I-S-VALOR-NOTA-PROMIS               PIC 9(15)V9(2).
           15 DCOMW88I-S-NOME-BANCO                      PIC X(20).
           15 DCOMW88I-S-NOME-AGENCIA                    PIC X(20).
           15 DCOMW88I-S-VAL-OPER-EXT                    PIC X(200).
           15 DCOMW88I-S-NOME-EMINENTE                   PIC X(40).
           15 DCOMW88I-S-RG-EMITENTE                     PIC X(15).
           15 DCOMW88I-S-CNPJ-EMINENTE                   PIC 9(9).
           15 DCOMW88I-S-FILIAL-EMINENTE                 PIC 9(4).
           15 DCOMW88I-S-CTRL-EMINENTE                   PIC 9(2).
           15 DCOMW88I-S-END-EMINENTE                    PIC X(40).
           15 DCOMW88I-S-CCEP                            PIC X(8).
           15 DCOMW88I-S-NOCOR                           PIC 9(2).
           15 DCOMW88I-S-AVALISTAS                 OCCURS 04 TIMES.
              20 DCOMW88I-S-CD-LANC                      PIC X(08).
              20 DCOMW88I-S-NOME-AVAL                    PIC X(40).
              20 DCOMW88I-S-CDOCTO-ID-AVAL               PIC X(15).
              20 DCOMW88I-S-CNPJ-AVAL                    PIC 9(09).
              20 DCOMW88I-S-FILIAL-AVAL                  PIC 9(04).
              20 DCOMW88I-S-CTRL-AVAL                    PIC 9(02).
              20 DCOMW88I-S-END-AVAL                     PIC X(40).
              20 DCOMW88I-S-CEP-AVAL                     PIC 9(08).
              20 DCOMW88I-S-NOME-CONJUGE                 PIC X(40).
              20 DCOMW88I-S-CNPJ-CNJG                    PIC 9(09).
              20 DCOMW88I-S-FILIAL-CNJG                  PIC 9(04).
              20 DCOMW88I-S-CTRL-CNJG                    PIC 9(02).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
