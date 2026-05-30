      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMWP6S-HEADER.
          10 DCOMWP6S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWP6S'.
          10 DCOMWP6S-TAM-LAYOUT       PIC  9(005) VALUE 06000.
      *
          10 DCOMWP6S-BLOCO-SAIDA.
             15 DCOMWP6S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWP6S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWP6S-OCORRENCIA                OCCURS 050  TIMES.
                20 DCOMWP6S-CBCO                   PIC S9(003) COMP-3.
                20 DCOMWP6S-CAG-BCRIA              PIC S9(005) COMP-3.
                20 DCOMWP6S-CCTA-BCRIA-CLI         PIC S9(013) COMP-3.
                20 DCOMWP6S-CPROT-SOLTC-AGNDA      PIC  X(050).
                20 DCOMWP6S-CTPO-PROT-AGNDA        PIC  X(001).
                20 DCOMWP6S-CTPO-CANAL             PIC S9(003) COMP-3.
                20 DCOMWP6S-QTEMPO-PROCM-CANAL     PIC S9(003) COMP-3.
                20 DCOMWP6S-FILLER                 PIC  X(050).
             15 DCOMWP6S-FILLER                    PIC  X(124).
      *                                                                *
      *================================================================*
