      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0D6: DB2PRD.TRELAC_PSSOA_DESC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4D6S                                       *
      *    -> FETCH   - DCOM4D6F                                       *
      *    -> INSERT  - DCOM4D6I                                       *
      *    -> UPDATE  - DCOM4D6U                                       *
      *    -> DELETE  - DCOM4D6D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWD6S-HEADER.
          10 DCOMWD6S-COD-LAYOUT   PIC  X(008) VALUE 'DCOMWD6S'.
          10 DCOMWD6S-TAM-LAYOUT   PIC  9(005) VALUE 12000.
      *
          10 DCOMWD6S-BLOCO-SAIDA.
             15 DCOMWD6S-QTDE-TOTAL            PIC  9(010).
             15 DCOMWD6S-QTDE-RETORNADA        PIC  9(003).
             15 DCOMWD6S-OCORRENCIA            OCCURS 050 TIMES.
                20 DCOMWD6S-NPSSOA-DESC-COML   PIC  S9(009) COMP-3.
                20 DCOMWD6S-NRELAC-PSSOA-DESC  PIC  S9(009) COMP-3.
                20 DCOMWD6S-CINDCD-PSSOA-DESC  PIC  X(002).
                20 DCOMWD6S-CELMTO-DESC-COML   PIC  S9(003) COMP-3.
                20 DCOMWD6S-CCHAVE-ELMTO-DESC  PIC  X(045).
                20 DCOMWD6S-CBCO               PIC  S9(003) COMP-3.
                20 DCOMWD6S-CAG-BCRIA          PIC  S9(005) COMP-3.
                20 DCOMWD6S-CCTA-BCRIA-CLI     PIC  S9(013) COMP-3.
                20 DCOMWD6S-CPOSTO-SERVC       PIC  S9(003) COMP-3.
                20 DCOMWD6S-IPSSOA-CONJG       PIC  X(040).
                20 DCOMWD6S-CCPF-CONJG         PIC  S9(009) COMP-3.
                20 DCOMWD6S-CCTRL-CPF-CONJG    PIC  S9(002) COMP-3.
                20 DCOMWD6S-CDOCTO-ID-CONJG    PIC  X(015).
                20 DCOMWD6S-CINDCD-CONJG-AUTRZ PIC  X(001).
                20 DCOMWD6S-CCEP               PIC  S9(005) COMP-3.
                20 DCOMWD6S-CCEP-COMPL         PIC  S9(003) COMP-3.
                20 DCOMWD6S-ELOGDR             PIC  X(040).
                20 DCOMWD6S-CSGL-UF            PIC  X(002).
                20 DCOMWD6S-DINCL-REG          PIC  X(010).
                20 DCOMWD6S-HULT-ATULZ         PIC  X(026).
                20 DCOMWD6S-CFUNC-BDSCO        PIC  S9(009) COMP-3.
                20 DCOMWD6S-CTERM              PIC  X(008).
      *
             15 DCOMWD6S-FILLER                PIC  X(374).
      *
