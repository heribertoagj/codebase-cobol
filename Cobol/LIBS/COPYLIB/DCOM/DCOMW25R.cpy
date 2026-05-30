      ******************************************************************
      * NOME BOOK : DCOMW25R - BOOK DE INTERFACE COM SERVICO DCOM425R  *
      * DATA      : 24/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 363 BYTES                                          *
      ******************************************************************
           05 DCOMW25R-HEADER.
             10 DCOMW25R-COD-LAYOUT      PIC X(08) VALUE 'DCOMW25R'.
             10 DCOMW25R-TAM-LAYOUT      PIC 9(05) VALUE 363.
           05 DCOMW25R-REGISTRO.
             10 DCOMW25R-AREA-ENTRADA.
                15 DCOMW25R-E-DANO-OPER-DESC       PIC 9(004).
                15 DCOMW25R-E-NSEQ-OPER-DESC       PIC 9(009).
                15 DCOMW25R-E-CINDCD-PSSOA-DESC    PIC X(002).
             10 DCOMW25R-AREA-SAIDA.
                15 DCOMW25R-S-DANO-OPER-DESC       PIC 9(004).
                15 DCOMW25R-S-NSEQ-OPER-DESC       PIC 9(009).
                15 DCOMW25R-S-NPCELA-DESC-COML     PIC 9(005).
                15 DCOMW25R-S-HULT-ATULZ           PIC X(026).
                15 DCOMW25R-S-NPSSOA-DESC-COML     PIC 9(009).
                15 DCOMW25R-S-NRELAC-PSSOA-DESC    PIC 9(009).
                15 DCOMW25R-S-CCNPJ-CPF            PIC 9(009).
                15 DCOMW25R-S-CFLIAL-CNPJ          PIC 9(005).
                15 DCOMW25R-S-CCTRL-CNPJ-CPF       PIC 9(002).
                15 DCOMW25R-S-CDOCTO-ID            PIC X(015).
                15 DCOMW25R-S-IPSSOA-DESC-COML     PIC X(060).
                15 DCOMW25R-S-CCLUB                PIC 9(010).
                15 DCOMW25R-S-CINDCD-PSSOA-DESC    PIC X(002).
                15 DCOMW25R-S-CBCO                 PIC 9(003).
                15 DCOMW25R-S-CAG-BCRIA            PIC 9(005).
                15 DCOMW25R-S-CCTA-BCRIA-CLI       PIC 9(013).
                15 DCOMW25R-S-CPOSTO-SERVC         PIC 9(005).
                15 DCOMW25R-S-IPSSOA-CONJG         PIC X(040).
                15 DCOMW25R-S-CCPF-CONJG           PIC 9(009).
                15 DCOMW25R-S-CCTRL-CPF-CONJG      PIC 9(002).
                15 DCOMW25R-S-CDOCTO-ID-CONJG      PIC X(015).
                15 DCOMW25R-S-CINDCD-CONJG-AUTRZ   PIC X(001).
                15 DCOMW25R-S-CCEP                 PIC 9(005).
                15 DCOMW25R-S-CCEP-COMPL           PIC 9(003).
                15 DCOMW25R-S-ELOGDR               PIC X(040).
                15 DCOMW25R-S-CSGL-UF              PIC X(002).
                15 DCOMW25R-S-DINCL-REG            PIC X(010).
                15 DCOMW25R-S-CFUNC-BDSCO          PIC 9(009).
                15 DCOMW25R-S-CTERM                PIC X(008).
