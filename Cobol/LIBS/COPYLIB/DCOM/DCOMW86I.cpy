      ******************************************************************
      * NOME BOOK : DCOMW86I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DATA      : 16/11/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 2990 BYTES                                         *
      ******************************************************************
       05 DCOMW86I-HEADER.
         10 DCOMW86I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW86I'.
         10 DCOMW86I-TAM-LAYOUT        PIC 9(05)   VALUE 2990.
       05 DCOMW86I-REGISTRO.
         10 DCOMW86I-BLOCO-ENTRADA.
           15 DCOMW86I-E-NUM-OPER                       PIC 9(13).
         10 DCOMW86I-BLOCO-SAIDA.
           15 DCOMW86I-S-CAG-BCRIA                       PIC 9(5).
           15 DCOMW86I-S-DIGITO-AGENCIA                  PIC X(2).
           15 DCOMW86I-S-NOME-AGEN                       PIC X(20).
           15 DCOMW86I-S-CCTA-BCRIA-CLI                  PIC 9(13).
           15 DCOMW86I-S-DIGITO-CONTA                    PIC X(2).
           15 DCOMW86I-S-CCNPJ-CPF                       PIC 9(9).
           15 DCOMW86I-S-CFLIAL-CNPJ                     PIC 9(4).
           15 DCOMW86I-S-CCTRL-CNPJ-CPF                  PIC 9(2).
           15 DCOMW86I-S-OPERACAO                        PIC 9(13).
           15 DCOMW86I-S-DINIC-OPER-DESC                 PIC X(10).
           15 DCOMW86I-S-VOPER-DESC-COML                 PIC 9(15)V9(2).
           15 DCOMW86I-S-IPSSOA-DESC-COML                PIC X(40).
           15 DCOMW86I-S-CONTR-COD-BARR                  PIC 9(9).
           15 DCOMW86I-S-NUM-COD-BARR                    PIC 9(1).
           15 DCOMW86I-S-MM-AAAA-BARR                    PIC 9(6).
           15 DCOMW86I-S-VTOT-IOF-CHQ                    PIC 9(09)V9(6).
           15 DCOMW86I-S-VTOT-JUR-CHQ                    PIC 9(09)V9(6).
           15 DCOMW86I-S-VTOT-LIQ-CHQ                    PIC 9(15)V9(2).
           15 DCOMW86I-S-VSOMATOT-CHQ                    PIC 9(15)V9(2).
           15 DCOMW86I-TIPO-VIA                          PIC 9(1).
           15 DCOMW86I-S-NOCOR                           PIC 9(2).
           15 DCOMW86I-S-OCORRENCIAS     OCCURS 50 TIMES.
              20 DCOMW86I-S-CD-LANC                    PIC X(8).
              20 DCOMW86I-S-CBCO-CHQ                   PIC 9(3).
              20 DCOMW86I-S-AGEN-CHQ                   PIC 9(5).
              20 DCOMW86I-S-CTA-CHQ                    PIC 9(13).
              20 DCOMW86I-S-CCPFCNPJ-CHQ               PIC 9(9).
              20 DCOMW86I-S-CFILIAL-CHQ                PIC 9(4).
              20 DCOMW86I-S-CCONT-CHQ                  PIC 9(2).
              20 DCOMW86I-S-NCHQ                       PIC 9(7).
              20 DCOMW86I-S-DT-DEPOS                   PIC X(10).
              20 DCOMW86I-S-VIOF-CHQ                   PIC 9(09)V9(6).
              20 DCOMW86I-S-VJUR-CHQ                   PIC 9(09)V9(6).
              20 DCOMW86I-S-VLIQ-CHQ                   PIC 9(13)V9(2).
              20 DCOMW86I-S-VTOT-CHQ                   PIC 9(15)V9(2).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
