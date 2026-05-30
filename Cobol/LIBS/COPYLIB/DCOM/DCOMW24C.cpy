      ******************************************************************
      * NOME BOOK : DCOMW24C - BOOK DE INTERFACE COM SERVICO DCOM424C  *
      * DESCRICAO : CONSULTA INFORMACOES DA PESSOA                     *
      * DATA      : 03/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 13247 BYTES                                        *
      *----------------------------------------------------------------*
      * DCOMW24C-E-TPO-PESQUISA = 1 - PESQUISA ORIGINAL PARCIAL-01     *
      *                           2 - PESQUISA ORIGINAL PARCIAL-24     *
      *                           3 - PESQUISA ORIGINAL PARCIAL-27     *
      ******************************************************************
           05 DCOMW24C-HEADER.
             10 DCOMW24C-COD-LAYOUT      PIC X(08) VALUE 'DCOMW24C'.
             10 DCOMW24C-TAM-LAYOUT      PIC 9(05) VALUE 13247.
           05 DCOMW24C-REGISTRO.
             10 DCOMW24C-AREA-ENTRADA.
                15 DCOMW24C-E-TPO-PESQUISA            PIC 9(002).
                15 DCOMW24C-E-MAX-OCORR               PIC 9(003).
                15 DCOMW24C-E-NPSSOA-DESC-COML        PIC 9(009).
                15 DCOMW24C-E-NRELAC-PSSOA-DESC       PIC 9(009).
                15 DCOMW24C-E-CINDCD-PSSOA-DESC       PIC X(002).
                15 DCOMW24C-E-CELMTO-DESC-COML        PIC 9(003).
                15 DCOMW24C-E-CCHAVE-ELMTO-DESC       PIC X(045).
                15 DCOMW24C-E-CBCO                    PIC 9(003).
                15 DCOMW24C-E-CAG-BCRIA               PIC 9(005).
                15 DCOMW24C-E-CCTA-BCRIA-CLI          PIC 9(013).
             10 DCOMW24C-AREA-PAGINACAO.
                15 DCOMW24C-INDICADOR-PAGINACAO       PIC  X(001).
                   88 DCOMW24C-I-ANTERIOR             VALUE 'A'.
                   88 DCOMW24C-I-INICIAL              VALUE 'I'.
                   88 DCOMW24C-I-PRIMEIRA             VALUE 'P'.
                   88 DCOMW24C-I-SEGUINTE             VALUE 'S'.
                   88 DCOMW24C-I-ULTIMA               VALUE 'U'.
                15 DCOMW24C-BLOCO-RESTART.
                   20 DCOMW24C-PRI-CHAVE.
                      25 DCOMW24C-P-NPSSOA-DESC-COML  PIC 9(009).
                      25 DCOMW24C-P-NRELAC-PSSOA-DESC PIC 9(009).
                   20 DCOMW24C-ULT-CHAVE.
                      25 DCOMW24C-U-NPSSOA-DESC-COML  PIC 9(009).
                      25 DCOMW24C-U-NRELAC-PSSOA-DESC PIC 9(009).
             10 DCOMW24C-AREA-SAIDA.
                15 DCOMW24C-S-TOT-REG                 PIC  9(002).
                15 DCOMW24C-S-LISTA-SAIDA    OCCURS 050 TIMES.
                   25 DCOMW24C-S-NPSSOA-DESC-COML     PIC 9(009).
                   25 DCOMW24C-S-NRELAC-PSSOA-DESC    PIC 9(009).
                   25 DCOMW24C-S-CINDCD-PSSOA-DESC    PIC X(002).
                   25 DCOMW24C-S-CELMTO-DESC-COML     PIC 9(003).
                   25 DCOMW24C-S-CCHAVE-ELMTO-DESC    PIC X(045).
                   25 DCOMW24C-S-CBCO                 PIC 9(003).
                   25 DCOMW24C-S-CAG-BCRIA            PIC 9(005).
                   25 DCOMW24C-S-CCTA-BCRIA-CLI       PIC 9(013).
                   25 DCOMW24C-S-CPOSTO-SERVC         PIC 9(003).
                   25 DCOMW24C-S-IPSSOA-CONJG         PIC X(040).
                   25 DCOMW24C-S-CCPF-CONJG           PIC 9(009).
                   25 DCOMW24C-S-CCTRL-CPF-CONJG      PIC 9(002).
                   25 DCOMW24C-S-CDOCTO-ID-CONJG      PIC X(015).
                   25 DCOMW24C-S-CINDCD-CONJG-AUTRZ   PIC X(001).
                   25 DCOMW24C-S-CCEP                 PIC 9(005).
                   25 DCOMW24C-S-CCEP-COMPL           PIC 9(003).
                   25 DCOMW24C-S-ELOGDR               PIC X(040).
                   25 DCOMW24C-S-CSGL-UF              PIC X(002).
                   25 DCOMW24C-S-DINCL-REG            PIC X(010).
                   25 DCOMW24C-S-HULT-ATULZ           PIC X(026).
                   25 DCOMW24C-S-CFUNC-BDSCO          PIC 9(009).
                   25 DCOMW24C-S-CTERM                PIC X(008).
