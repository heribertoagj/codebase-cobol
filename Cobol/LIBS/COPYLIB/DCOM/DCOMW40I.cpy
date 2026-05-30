      ******************************************************************
      * NOME BOOK : DCOMW40I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DESCRICAO : CONSULTA INFORMACOES PESSOA                        *
      * DATA      : 27/09/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 385 BYTES                                          *
      ******************************************************************
       05 DCOMW40I-HEADER.
         10 DCOMW40I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW40I'.
         10 DCOMW40I-TAM-LAYOUT        PIC 9(05)   VALUE 385.
       05 DCOMW40I-REGISTRO.
         10 DCOMW40I-BLOCO-ENTRADA.
           15 DCOMW40I-E-CCHAVE-ELMTO-DESC    PIC  X(45).
           15 DCOMW40I-E-CINDCD-PSSOA-DESC    PIC  X(02).
           15 DCOMW40I-E-CELMTO-DESC-COML     PIC  9(03).
         10 DCOMW40I-BLOCO-SAIDA.
           15 DCOMW40I-S-CCNPJ-CPF            PIC  9(09).
           15 DCOMW40I-S-CFLIAL-CNPJ          PIC  9(05).
           15 DCOMW40I-S-CCTRL-CNPJ-CPF       PIC  9(02).
           15 DCOMW40I-S-CDOCTO-ID            PIC  X(15).
           15 DCOMW40I-S-IPSSOA-DESC-COML     PIC  X(60).
           15 DCOMW40I-S-CCLUB                PIC  9(10).
           15 DCOMW40I-S-CBCO                 PIC  9(03).
           15 DCOMW40I-S-NOME-BANCO           PIC  X(40).
           15 DCOMW40I-S-CAG-BCRIA            PIC  9(05).
           15 DCOMW40I-S-NOME-AGENCIA         PIC  X(40).
           15 DCOMW40I-S-CCTA-BCRIA-CLI       PIC  9(13).
           15 DCOMW40I-S-CPOSTO-SERVC         PIC  9(03).
           15 DCOMW40I-S-IPSSOA-CONJG         PIC  X(40).
           15 DCOMW40I-S-CCPF-CONJG           PIC  9(09).
           15 DCOMW40I-S-CCTRL-CPF-CONJG      PIC  9(02).
           15 DCOMW40I-S-CDOCTO-ID-CONJG      PIC  X(15).
           15 DCOMW40I-S-CINDCD-CONJG-AUTRZ   PIC  X(01).
           15 DCOMW40I-S-CCEP                 PIC  9(05).
           15 DCOMW40I-S-CCEP-COMPL           PIC  9(03).
           15 DCOMW40I-S-ELOGDR               PIC  X(40).
           15 DCOMW40I-S-CSGL-UF              PIC  X(02).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
