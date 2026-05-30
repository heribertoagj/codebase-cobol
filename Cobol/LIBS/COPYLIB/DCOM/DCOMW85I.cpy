      ******************************************************************
      * NOME BOOK : DCOMW85I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DATA      : 14/11/2017                                         *
      * AUTOR     : SHERLY GUEDES                                      *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 1085 BYTES                                         *
      ******************************************************************
       05 DCOMW85I-HEADER.
         10 DCOMW85I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW85I'.
         10 DCOMW85I-TAM-LAYOUT        PIC 9(05)   VALUE 1085.
       05 DCOMW85I-REGISTRO.
         10 DCOMW85I-BLOCO-ENTRADA.
           15 DCOMW85I-E-NUM-OPER                       PIC 9(13).
         10 DCOMW85I-BLOCO-SAIDA.
           15 DCOMW85I-S-CAG-BCRIA                       PIC 9(5).
           15 DCOMW85I-S-DIGITO-AGENCIA                  PIC X(2).
           15 DCOMW85I-S-NOME-AGEN                       PIC X(20).
           15 DCOMW85I-S-CCTA-BCRIA-CLI                  PIC 9(13).
           15 DCOMW85I-S-DIGITO-CONTA                    PIC X(2).
           15 DCOMW85I-S-CCNPJ-CPF                       PIC 9(9).
           15 DCOMW85I-S-CFLIAL-CNPJ                     PIC 9(4).
           15 DCOMW85I-S-CCTRL-CNPJ-CPF                  PIC 9(2).
           15 DCOMW85I-S-OPERACAO                        PIC 9(13).
           15 DCOMW85I-S-DINIC-OPER-DESC                 PIC X(10).
           15 DCOMW85I-S-VOPER-DESC-COML                 PIC 9(15)V9(2).
           15 DCOMW85I-S-IPSSOA-DESC-COML                PIC X(40).
           15 DCOMW85I-S-VTAC-OPER-DESC                  PIC 9(13)V9(2).
           15 DCOMW85I-S-VTAR-OPER                       PIC 9(13)V9(2).
           15 DCOMW85I-S-VIOF-OPER-DESC                  PIC 9(13)V9(2).
           15 DCOMW85I-S-VTOT-JUROS                      PIC 9(13)V9(2).
           15 DCOMW85I-S-CONTR-COD-BARR                  PIC 9(9).
           15 DCOMW85I-S-NUM-COD-BARR                    PIC 9(1).
           15 DCOMW85I-S-MM-AAAA-BARR                    PIC 9(6).
           15 DCOMW85I-S-QTDE-TITLO                      PIC 9(5).
           15 DCOMW85I-S-VTOT-IOF                        PIC 9(17).
           15 DCOMW85I-S-VTOT-JUR                        PIC 9(17).
           15 DCOMW85I-S-VTOT-TITLO                      PIC 9(17).
           15 DCOMW85I-S-VTOT-LIQUIDO                    PIC 9(17).
           15 DCOMW85I-TIPO-VIA                          PIC 9(1).
           15 DCOMW85I-S-NOCOR                           PIC 9(2).
           15 DCOMW85I-S-OCORRENCIAS     OCCURS 50 TIMES.
              20 DCOMW85I-S-CD-LANC                    PIC X(8).
              20 DCOMW85I-S-CTITLO-COBR-BCO            PIC X(20).
              20 DCOMW85I-S-NOME-SACADO                PIC X(40).
              20 DCOMW85I-S-CNPJ-SAC                   PIC 9(9).
              20 DCOMW85I-S-FILIAL-SAC                 PIC 9(4).
              20 DCOMW85I-S-CTRL-SAC                   PIC 9(2).
              20 DCOMW85I-S-DVCTO-TITLO                PIC X(10).
              20 DCOMW85I-S-VALOR-IOF                  PIC 9(15).
              20 DCOMW85I-S-VALOR-JUROS                PIC 9(15).
              20 DCOMW85I-S-VTITLO-DESC                PIC 9(15).
              20 DCOMW85I-S-VALOR-LIQUIDO              PIC 9(15).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
