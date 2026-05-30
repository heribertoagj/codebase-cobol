      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0P6: DB2PRD.TPROT_SOLTC_CANAL                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4P6S                                       *
      *    -> FETCH   - DCOM4P6F                                       *
      *    -> INSERT  - DCOM4P6I                                       *
      *    -> UPDATE  - DCOM4P6U                                       *
      *    -> DELETE  - DCOM4P6D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWP6E-HEADER.
          10 DCOMWP6E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWP6E'.
          10 DCOMWP6E-TAM-LAYOUT       PIC  9(005) VALUE 00400.
      *
       05 DCOMWP6E-BLOCO-ENTRADA.
          10 DCOMWP6E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWP6E-ENTRADA.
             15 DCOMWP6E-CBCO                      PIC S9(003) COMP-3.
             15 DCOMWP6E-CAG-BCRIA                 PIC S9(005) COMP-3.
             15 DCOMWP6E-CCTA-BCRIA-CLI            PIC S9(013) COMP-3.
             15 DCOMWP6E-CPROT-SOLTC-AGNDA         PIC  X(050).
             15 DCOMWP6E-CTPO-PROT-AGNDA           PIC  X(001).
             15 DCOMWP6E-CTPO-CANAL                PIC S9(003) COMP-3.
             15 DCOMWP6E-QTEMPO-PROCM-CANAL        PIC S9(003) COMP-3.
             15 DCOMWP6E-FILLER                    PIC  X(030).
      *
          10 DCOMWP6E-BLOCO-VARIAVEL.
             15 DCOMWP6E-V-FILLER                  PIC  X(100).
      *
          10 DCOMWP6E-BLOCO-PAGINACAO.
             15 DCOMWP6E-MAX-OCORR                 PIC  9(003).
             15 DCOMWP6E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWP6E-P-INICIAL              VALUE 'I'.
                88 DCOMWP6E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWP6E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWP6E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWP6E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWP6E-BLOCO-RESTART.
             15 DCOMWP6E-CHAVE-INI.
                20 DCOMWP6E-I-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWP6E-I-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWP6E-I-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWP6E-I-CPROT-SOLTC-AGNDA    PIC  X(050).
                20 DCOMWP6E-I-CTPO-PROT-AGNDA      PIC  X(001).
                20 DCOMWP6E-I-CTPO-CANAL           PIC S9(003) COMP-3.
                20 DCOMWP6E-I-FILLER               PIC  X(015).
             15 DCOMWP6E-CHAVE-FIM.
                20 DCOMWP6E-F-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWP6E-F-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWP6E-F-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWP6E-F-CPROT-SOLTC-AGNDA    PIC  X(050).
                20 DCOMWP6E-F-CTPO-PROT-AGNDA      PIC  X(001).
                20 DCOMWP6E-F-CTPO-CANAL           PIC S9(003) COMP-3.
                20 DCOMWP6E-F-FILLER               PIC  X(015).
          10 DCOMWP6E-FILLER                       PIC  X(016).
