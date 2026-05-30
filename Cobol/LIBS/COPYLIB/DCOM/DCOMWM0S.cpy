      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0M0: DB2PRD.TTRASI_OPER_COMPL
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4M0S                                       *
      *    -> FETCH   - DCOM4M0F                                       *
      *    -> INSERT  - DCOM4M0I                                       *
      *    -> UPDATE  - DCOM4M0U                                       *
      *    -> DELETE  - DCOM4M0D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWM0S-HEADER.
          10 DCOMWM0S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWM0S'.
          10 DCOMWM0S-TAM-LAYOUT       PIC  9(005) VALUE 04176.
      *
          10 DCOMWM0S-BLOCO-SAIDA.
             15 DCOMWM0S-QTDE-TOTAL               PIC  9(010).
             15 DCOMWM0S-QTDE-RETORNADA           PIC  9(003).
             15 DCOMWM0S-OCORRENCIA               OCCURS 050 TIMES.
                20 DCOMWM0S-DANO-OPER-DESC        PIC  S9(004) COMP-3.
                20 DCOMWM0S-NSEQ-OPER-DESC        PIC  S9(009) COMP-3.
                20 DCOMWM0S-CVAR-PRODT-RURAL      PIC  S9(005) COMP-3.
                20 DCOMWM0S-CNRO-REFT-BACEN       PIC  S9(018) COMP-3.
                20 DCOMWM0S-CCESTA-SAFRA-RURAL    PIC  S9(005) COMP-3.
                20 DCOMWM0S-CINDCD-ZNMTO          PIC  S9(001) COMP-3.
                20 DCOMWM0S-CTPO-BNEFC-RURAL      PIC  S9(002) COMP-3.
                20 DCOMWM0S-NBASE-LEGAL-RENEG     PIC  S9(005) COMP-3.
                20 DCOMWM0S-CREG-AMBTL-RURAL      PIC  X(041).
                20 DCOMWM0S-CORIGE-ANTEC-PGMD     PIC  S9(001) COMP-3.
                20 DCOMWM0S-CCONTR-LIM-DESC       PIC  S9(009) COMP-3.
                20 DCOMWM0S-CVRSAO-CONTR-LIM      PIC  S9(003) COMP-3.
                20 DCOMWM0S-CINDCD-FORMA-LIQDC    PIC  X(001).
                20 DCOMWM0S-CINDCD-UTILZ-LIM      PIC  X(001).
                20 DCOMWM0S-CINDCD-DEB-PCIAL      PIC  X(001).
                20 DCOMWM0S-CINDCD-TENTV-DEB      PIC  X(001).
