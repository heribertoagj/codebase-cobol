      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0M0: DB2PRD.TTRASI_OPER_COMPL                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4M0S                                       *
      *    -> FETCH   - DCOM4M0F                                       *
      *    -> INSERT  - DCOM4M0I                                       *
      *    -> UPDATE  - DCOM4M0U                                       *
      *    -> DELETE  - DCOM4M0D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWM0E-HEADER.
          10 DCOMWM0E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWM0E'.
          10 DCOMWM0E-TAM-LAYOUT       PIC  9(005) VALUE 01000.
      *
       05 DCOMWM0E-BLOCO-ENTRADA.
          10 DCOMWM0E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWM0E-ENTRADA.
             15 DCOMWM0E-DANO-OPER-DESC            PIC  S9(004) COMP-3.
             15 DCOMWM0E-NSEQ-OPER-DESC            PIC  S9(009) COMP-3.
             15 DCOMWM0E-CVAR-PRODT-RURAL          PIC  S9(005) COMP-3.
             15 DCOMWM0E-CNRO-REFT-BACEN           PIC  S9(018) COMP-3.
             15 DCOMWM0E-CCESTA-SAFRA-RURAL        PIC  S9(005) COMP-3.
             15 DCOMWM0E-CINDCD-ZNMTO              PIC  S9(001) COMP-3.
             15 DCOMWM0E-CTPO-BNEFC-RURAL          PIC  S9(002) COMP-3.
             15 DCOMWM0E-NBASE-LEGAL-RENEG         PIC  S9(005) COMP-3.
             15 DCOMWM0E-CREG-AMBTL-RURAL          PIC  X(041).
             15 DCOMWM0E-CORIGE-ANTEC-PGMD         PIC  S9(001) COMP-3.
             15 DCOMWM0E-CCONTR-LIM-DESC           PIC  S9(009) COMP-3.
             15 DCOMWM0E-CVRSAO-CONTR-LIM          PIC  S9(003) COMP-3.
             15 DCOMWM0E-CINDCD-FORMA-LIQDC        PIC  X(001).
             15 DCOMWM0E-CINDCD-UTILZ-LIM          PIC  X(001).
             15 DCOMWM0E-CINDCD-DEB-PCIAL          PIC  X(001).
             15 DCOMWM0E-CINDCD-TENTV-DEB          PIC  X(001).
             15 DCOMWM0E-FILLER                    PIC  X(189).
      *
          10 DCOMWM0E-BLOCO-VARIAVEL.
             15 DCOMWM0E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWM0E-BLOCO-PAGINACAO.
             15 DCOMWM0E-MAX-OCORR                 PIC  9(003).
             15 DCOMWM0E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWM0E-P-INICIAL              VALUE 'I'.
                88 DCOMWM0E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWM0E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWM0E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWM0E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWM0E-BLOCO-RESTART.
             15 DCOMWM0E-CHAVE-INI.
                20 DCOMWM0E-I-DANO-OPER-DESC       PIC  S9(004) COMP-3.
                20 DCOMWM0E-I-NSEQ-OPER-DESC       PIC  S9(009) COMP-3.
                20 DCOMWM0E-I-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMWM0E-I-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMWM0E-I-FILLER               PIC  X(035).
             15 DCOMWM0E-CHAVE-FIM.
                20 DCOMWM0E-F-DANO-OPER-DESC       PIC  S9(004) COMP-3.
                20 DCOMWM0E-F-NSEQ-OPER-DESC       PIC  S9(009) COMP-3.
                20 DCOMWM0E-F-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMWM0E-F-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMWM0E-F-FILLER               PIC  X(035).
      *
          10 DCOMWM0E-FILLER                       PIC  X(301).
      *
