      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0P5: DB2PRD.TPROT_SOLTC_AGNDA                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4P5S                                       *
      *    -> FETCH   - DCOM4P5F                                       *
      *    -> INSERT  - DCOM4P5I                                       *
      *    -> UPDATE  - DCOM4P5U                                       *
      *    -> DELETE  - DCOM4P5D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWP5S-HEADER.
          10 DCOMWP5S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWP5S'.
          10 DCOMWP5S-TAM-LAYOUT       PIC  9(005) VALUE 05276.
      *
          10 DCOMWP5S-BLOCO-SAIDA.
             15 DCOMWP5S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWP5S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWP5S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWP5S-CBCO                   PIC S9(003) COMP-3.
                20 DCOMWP5S-CAG-BCRIA              PIC S9(005) COMP-3.
                20 DCOMWP5S-CCTA-BCRIA-CLI         PIC S9(013) COMP-3.
                20 DCOMWP5S-CPROT-SOLTC-AGNDA      PIC  X(050).
                20 DCOMWP5S-CTPO-PROT-AGNDA        PIC  X(001).
                20 DCOMWP5S-QTEMPO-VALDD-PROCM     PIC S9(003) COMP-3.
                20 DCOMWP5S-CSIT-PROT-AGNDA        PIC S9(001) COMP-3.
                20 DCOMWP5S-HINCL-REG              PIC  X(026).
                20 DCOMWP5S-NSMULA-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWP5S-DANO-OPER-DESC         PIC S9(004) COMP-3.
                20 DCOMWP5S-NSEQ-OPER-DESC         PIC S9(009) COMP-3.
